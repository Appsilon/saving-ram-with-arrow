library(shiny)
library(shiny.semantic)
library(dplyr)
library(echarts4r)

group_values <- function(variable, levels) {
  if ("Select All" %in% levels) {
    rep(TRUE, length(variable))
  } else {
    variable %in% levels
  }
}

count_stat <- function(deaths, population, births, type) {
  numerator <- sum(deaths, na.rm = TRUE)
  denominator_vec <- 1
  if (type == "deaths/population") {
    denominator_vec <- population
  }
  if (type == "deaths/births") {
    denominator_vec <- births
  }
  denominator <- sum(denominator_vec, na.rm = TRUE)
  if (identical(denominator, 0))
    denominator <- NA
  
  return(numerator/denominator)
}

ui <- semanticPage(
  div(class = "ui two columns grid",
      div(class = "ui four wide column",
          dropdown("group_var", c("Sex", "Cause", "Country"), default_text = "Select grouping variable", value = "Sex"),
          uiOutput("group_var_options"),
          multiple_radio("plot_value", "Plot value", c("deaths", "deaths/population", "deaths/births")),
          uibutton("draw_plot", "Generate plot", icon = "chart line")
      ),
      div(class = "ui twelve wide column",
          echarts4rOutput("plot")
      )
  )
)

server <- function(input, output, session) {
  
  mortality_data <- data.table::fread("mortality_full.csv")
  
  output$group_var_options <- renderUI({
    group_var <- input$group_var
    choices <- c("Select All", unique(mortality_data[[group_var]]))
    search_selection_choices("group_values", choices, choices[1], default_text = "Choose values", multiple = TRUE)
  })
  
  data_filtered <- reactive({
    req(input$group_var)
    req(input$group_values)

    filter_var <- input$group_var
    filter_var_values <- strsplit(input$group_values, ",", fixed = TRUE)[[1]]

    mortality_data %>% 
      filter(group_values(!!sym(filter_var), filter_var_values)) %>% 
      group_by(Year, !!sym(filter_var))
  })
  
  data_aggregated <- reactive({
    req(data_filtered())
    req(input$plot_value)
    data_filtered() %>% 
      summarise(stat_value = count_stat(Deaths1, Pop1, Lb, input$plot_value))
  })
  
  output$plot <- renderEcharts4r({
    input$draw_plot
    plot_data <- isolate(data_aggregated())
    req(plot_data)
    filter_var <- isolate(input$group_var)
    min_year <- min(plot_data$Year)
    max_year <- max(plot_data$Year)
    plot_data %>% 
      group_by(!!sym(filter_var)) %>% 
      e_charts(Year) %>% 
      e_line(stat_value) %>% 
      e_x_axis(min = min_year, max = max_year)
  })
}

shinyApp(ui, server)