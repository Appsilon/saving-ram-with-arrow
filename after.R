library(shiny)
library(shiny.semantic)
library(dplyr)
library(echarts4r)
library(arrow)

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

grouping_vars <- c("Sex" = "Sex", "Cause" = "cause_description", "Country" = "Country")

ui <- semanticPage(
  tags$style(HTML(paste(
    ".inputs > * {margin: 0 10px 10px 10px !important; max-width: 80%;} \n",
    ".inputs > span, .inputs > div > label {margin-bottom: 0px; font-weight: 700}"
  ))),
  div(class = "ui two columns grid",
      div(class = "ui four wide column", class = "inputs", style = "margin-top: 10px;",
          span("Grouping variable"),
          dropdown("group_var", names(grouping_vars), grouping_vars, default_text = "Select grouping variable", value = grouping_vars[1]),
          span("Grouping variable levels"),
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
  
  mortality_data <- read_parquet("mortality_full.parquet", as_data_frame = FALSE)
  group_vars_values <- purrr::map(grouping_vars, ~ unique(unique(mortality_data$GetColumnByName(.)$as_vector()))) %>% 
    setNames(grouping_vars)
  
  output$group_var_options <- renderUI({
    group_var <- input$group_var
    choices <- c("Select All", unlist(group_vars_values[[group_var]]))
    search_selection_choices("group_values", choices, choices[1], default_text = "Choose values", multiple = TRUE)
  })
  
  data_filtered <- reactive({
    req(input$group_var)
    req(input$group_values)
    
    filter_var <- input$group_var
    filter_var_values <- strsplit(input$group_values, ",", fixed = TRUE)[[1]]
    
    mortality_data %>% 
      select(Year, !!sym(filter_var), Deaths1, Pop1, Lb) %>% {
        if (!"Select All" %in% filter_var_values) filter(., !!sym(filter_var) %in% filter_var_values) else .
      } %>% 
      group_by(Year, !!sym(filter_var))
  })
  
  data_aggregated <- reactive({
    req(data_filtered())
    req(input$plot_value)
    data_filtered() %>% 
      collect() %>% 
      summarise(stat_value = count_stat(Deaths1, Pop1, Lb, input$plot_value)) %>% 
      ungroup() %>% 
      mutate(Year = as.character(Year))
  })
  
  output$plot <- renderEcharts4r({
    input$draw_plot
    plot_data <- isolate(data_aggregated())
    req(plot_data)
    y_axis_label <- isolate(input$plot_value)
    filter_var <- isolate(input$group_var)
    min_year <- min(plot_data$Year)
    max_year <- max(plot_data$Year)
    plot_data %>% 
      group_by(!!sym(filter_var)) %>% 
      e_charts(Year) %>% 
      e_line(stat_value) %>% 
      e_x_axis(min = min_year, max = max_year) %>% 
      e_title(glue::glue("Year vs {y_axis_label}"))
  })
}

shinyApp(ui, server)