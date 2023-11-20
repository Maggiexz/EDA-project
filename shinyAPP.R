library(ggplot2)
library(ggthemes)
library(dplyr)
library(shiny)
library(maps)
library(hexbin)
library(countrycode)
library(rworldmap)
library(leaflet)
library(plotly)
library(janitor)

plot_theme <- theme_few() + 
  theme(plot.title = element_text(color = "darkred",hjust = 0.5)) +
  theme(strip.text.x = element_text(size = 14, colour = "#202020"))

youtube_data <- read.csv('youtube_UTF_8.csv',encoding = "UTF-8")

youtube_data <- youtube_data |> janitor::clean_names()
print(names(youtube_data))

youtube_data[youtube_data == 0 | youtube_data == "nan" | youtube_data == "NaN"] <- NA

# Find those colunms
variables_to_check <- c("video_views", "uploads", "lowest_monthly_earnings", 
                        "highest_monthly_earnings", "lowest_yearly_earnings", 
                        "highest_yearly_earnings")

# Find rows that meet both conditions
rows_with_all_na <- which(rowSums(is.na(youtube_data[, variables_to_check])) == length(variables_to_check))

# Delete these rows
deleted_rows <- youtube_data[rows_with_all_na, ]
deleted_values <- deleted_rows[, variables_to_check]
youtube_data_cleaned <- youtube_data[-rows_with_all_na, ]

# Print all deleted rows
print(deleted_values)

q1 <- quantile(youtube_data_cleaned$created_year, 0.25, na.rm = TRUE)
q3 <- quantile(youtube_data_cleaned$created_year, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

youtube_data_cleaned <- youtube_data_cleaned[youtube_data_cleaned$created_year >= lower_bound & youtube_data_cleaned$created_year <= upper_bound, ]

youtube_data_cleaned$channel_type[is.na(youtube_data_cleaned$channel_type)] <- "missing"

impute_na_with_median <- function(data) {
  for (col_name in names(data)) {
    col <- data[[col_name]]
    
    if (is.numeric(col)) {
      median_val <- median(col, na.rm = TRUE)
      col[is.na(col)] <- median_val
      data[[col_name]] <- col
    }
  }
  return(data)
}
youtube_data_cleaned <- impute_na_with_median(youtube_data_cleaned)

youtube_data_transformed <- youtube_data_cleaned |>
  select(-category, -title, -abbreviation,
         -video_views_rank, -country_rank, -channel_type_rank, -created_date,
         -population, -unemployment_rate, -gross_tertiary_education_enrollment,
         -urban_population, -latitude, -longitude)

convert_to_factor <- function(data) {
  for (col_name in names(data)) {
    if (is.character(data[[col_name]])) {  
      data[[col_name]] <- as.factor(data[[col_name]])
    }
  }
  return(data)
}
youtube_data_transformed <- convert_to_factor(youtube_data_transformed)

world_map <- map_data("world")

Video_Views_by_Country <- youtube_data_transformed |>
  group_by(country) |>
  summarise(videoviews = sum(video_views))


map_data <- world_map |>
  left_join(Video_Views_by_Country, by = c("region" = "country"), copy = TRUE)


ui <- fluidPage(
  titlePanel("Shiny App"),
  
  verticalLayout(
    wellPanel(
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("scatter_plot", "density_plot", "map")),
      
      
      # When select different plot types, different widgets will be showed
      conditionalPanel(
        condition = "input.plot_type == 'scatter_plot'",
        verticalLayout(
          selectInput("numeric_var", "Select Numeric Variable:",
                      choices = c("subscribers", "video_views")),
          plotOutput("scatter_plot", width = "800px", height = "600px")
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'density_plot'",
        verticalLayout(
          radioButtons("earnings_var", "Select Earnings Variable:",
                       choices = c("monthly_earnings", "yearly_earnings")),
          plotOutput("density_plot", width = "800px", height = "400px")
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'map'",
        verticalLayout(
          sliderInput(inputId = "slider_range", label = "Video Views Range",
                      min = 0, max = max(youtube_data_transformed$video_views),
                      value = c(0, max(youtube_data_transformed$video_views))),
          plotOutput("my_map", width = "800px", height = "600px")
        )
      )
    )
  )
)


server <- function(input, output) {
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    x_var <- switch(input$numeric_var,
                    "subscribers" = "subscribers",
                    "video_views" = "video_views") 
    
    ggplot(youtube_data_transformed, aes_string(x = x_var, y = "channel_type", color = "channel_type")) +
      geom_point() +
      ggtitle(paste("Relationship between", input$numeric_var, "and Channel Type")) +
      ylab("Channel Type") +
      xlab(input$numeric_var) +
      theme_minimal() + plot_theme +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 17,hjust = 0.5),
            legend.key.size = unit(1.2, "cm"),legend.text = element_text(size = 12),legend.title = element_text( size = 13))
  })
  
  # Density plot
  output$density_plot <- renderPlot({
    earnings_var <- input$earnings_var
    
    if (earnings_var == "monthly_earnings") {
      p <- ggplot(data = youtube_data_transformed) +
        geom_density(aes(x = log(lowest_monthly_earnings), fill = "Lowest Monthly Earnings"), alpha = 0.5) +
        geom_density(aes(x = log(highest_monthly_earnings), fill = "Highest Monthly Earnings"), alpha = 0.5) +
        scale_x_log10() +
        ggtitle("Monthly Earnings") +
        xlab("Earnings(log)") +
        ylab("Density") +
        labs(fill = "Variable") +
        plot_theme +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              plot.title = element_text(size = 17,hjust = 0.5),
              legend.key.size = unit(1.2, "cm"),legend.text = element_text(size = 12),legend.title = element_text( size = 13))
      
      p + annotate("text", x = max(p$data$x), y = 0, label = "Lowest Monthly Earnings", vjust = 0, hjust = 1, color = "lightblue") +
        annotate("text", x = max(p$data$x), y = -0.02, label = "Highest Monthly Earnings", vjust = 0, hjust = 1, color = "darkred")
    } else if (earnings_var == "yearly_earnings") {
      p <- ggplot(data = youtube_data_transformed) +
        geom_density(aes(x = log(lowest_yearly_earnings), fill = "Lowest Yearly Earnings"), alpha = 0.5) +
        geom_density(aes(x = log(highest_yearly_earnings), fill = "Highest Yearly Earnings"), alpha = 0.5) +
        scale_x_log10() +
        ggtitle("Yearly Earnings") +
        xlab("Earnings(log)") +
        ylab("Density") +
        labs(fill = "Variable") + 
        plot_theme +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              plot.title = element_text(size = 17,hjust = 0.5),
              legend.key.size = unit(1.2, "cm"),legend.text = element_text(size = 12),legend.title = element_text( size = 13))
      
      p + annotate("text", x = max(p$data$x), y = 0, label = "Lowest Yearly Earnings", vjust = 0, hjust = 1, color = "lightblue") +
        annotate("text", x = max(p$data$x), y = -0.02, label = "Highest Yearly Earnings", vjust = 0, hjust = 1, color = "darkred")
    }
  })
  
  # Map
  filtered_data <- reactive({
    subset(youtube_data_transformed,
           video_views >= input$slider_range[1] & video_views <= input$slider_range[2])
  })
  
  output$my_map <- renderPlot({
    filtered_map_data <- left_join(map_data, filtered_data(), by = c("region" = "country"))
    
    ggplot(data = filtered_map_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = video_views / 1e9), color = "white") +
      scale_fill_gradient(name = "Video Views", low = "lightgrey", high = "darkred", labels = scales::comma) +
      coord_fixed(ratio = 1.3) +
      labs(title = "Video Views of Countries", fill = "Video Views") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 20,hjust = 0.5,color = "darkred"),
            legend.key.size = unit(1.2, "cm"),
            legend.text = element_text(size = 12),
            legend.title = element_text( size = 13))
  })
}

shinyApp(ui, server)

