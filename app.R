library(shinydashboard)
library(shiny)
library(tidyverse)
library(highcharter)

source("global.R")

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    uiOutput("value_boxes"),
    fluidRow(
      column(12,
             column(4,
                    selectizeInput(
                      inputId = "select_attack_type",
                      label = NULL,
                      choices = attack_types,
                      multiple = TRUE,
                      selected = NULL,
                      options = list(placeholder = "Select the Attack Type",
                                     maxItems = 1
                                     ),
                      width = "100%"
                    )
                    ),
             column(4,
                    selectizeInput(
                      inputId = "select_certainty_level",
                      label = NULL,
                      choices = certainty_level,
                      multiple = TRUE,
                      selected = NULL,
                      options = list(placeholder = "Select the Certainty Level",
                                     maxItems = 1
                                     ),
                      width = "100%"
                    )
                    ),
             column(4,
                    sliderInput(
                      inputId = "select_year",
                      label = NULL,
                      min = 2017,
                      max = 2024,
                      value = c(2017, 2024),
                      animate = FALSE,
                      width = "100%"
                    )
             )
             ),
      column(12,
             uiOutput("filter_report")
             ),
      column(12,
             tabsetPanel(
               id = "tabs",
               type = 'tabs',
               tabPanel("Total Attacks", value = "total_attacks"),
               tabPanel("Total Injuries", value = "total_injured"),
               tabPanel("Total Deaths", value = "total_death"),
               tabPanel("Total Casualties", value = "total_casualties"),
               tabPanel('Health Workers Abduction/Arrest/Detention', value = "hw_abduction/arrest/detention"),
               # tabPanel('Health Workers Abduction', value = "hw_abduction"),
               # tabPanel('Health Workers Arrest', value = "hw_arrest"),
               # tabPanel('Health Workers Detention', value = "hw_detention"),
               tabPanel('Patient Abduction/Arrest/Detention', value = "patient_abduction/arrest/detention"),
               # tabPanel('Patient Abduction', value = "patient_abduction"),
               # tabPanel('Patient Arrest', value = "patient_arrest"),
               # tabPanel('Patient Detention', value = "patient_detention"),
               # tabPanel('About the data', value = "about")
               ),
             uiOutput("plot_report")
             ),
    ),
    tags$hr(),
    column(6, uiOutput("compare_country1")),
    column(6, uiOutput("compare_country2")),
    # remove below from mobile/tablet version
    tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    # tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    # tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    # tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    # tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
    tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr()
  )
)

server <- function(input, output, session){
  
  data <- reactiveValues(df = df)
  
  df <- reactive({
    
    data <- data$df

    if (!is.null(input$select_attack_type)) {
      data <- data %>% filter(!!sym(input$select_attack_type) == TRUE) # filter(!!sym(input$select_attack_type) == "True")
    }
    
    if (!is.null(input$select_certainty_level)) {
      data <- data %>% filter(`Certainty Level` == input$select_certainty_level)
    }
    
    data <- data %>% filter(year >= input$select_year[1] & year <= input$select_year[2])
    
    return(data)
  })

  summary_all <- reactive({
    summary_all_func(df())
  })
  
  output$value_boxes <- renderUI({
    
    splitLayout(cellWidths = rep("20%", 5),
                valueBox("Attacks", width = "100%", subtitle = sum(summary_all()$total_attacks), color = "red"),
                valueBox("Deaths", width = "100%", subtitle = sum(summary_all()$total_death), color = "orange"),
                valueBox("Injuries", width = "100%", subtitle = sum(summary_all()$total_injured), color = "yellow"),
                valueBox("Health Workers", width = "100%",
                         subtitle = paste0("Abduction: ", sum(summary_all()$hw_abduction),
                                           ", Arrest: ", sum(summary_all()$hw_arrest),
                                           ", Detention: ", sum(summary_all()$hw_detention)
                                           ),

                         color = "olive"),
                valueBox("Patients", width = "100%",
                         subtitle = paste0("Abduction: ", sum(summary_all()$patient_abduction),
                                           ", Arrest: ", sum(summary_all()$patient_arrest),
                                           ", Detention: ", sum(summary_all()$patient_detention)
                         ),
                         color = "navy"),
    )
  })
  
  output$compare_country_select <- renderUI({
    
    countries <- unique(summary_all()$country)
             selectizeInput(
               inputId = "country1",
               label = NULL,
               choices = countries,
               multiple = TRUE,
               selected = countries[1],
               options = list(placeholder = "Select the Country",
                              maxItems = 1
               ),
               width = "100%"
             )
  })
  
  output$compare_country_report <- renderUI({
    country_report_func(tbl = summary_all(), data = df(), selected_country = input$country1)
  })
  
  output$compare_country_select2 <- renderUI({
    
    countries <- unique(summary_all()$country)
    selectizeInput(
      inputId = "country2",
      label = NULL,
      choices = countries,
      multiple = TRUE,
      selected = countries[2],
      options = list(placeholder = "Select the Country",
                     maxItems = 1
      ),
      width = "100%"
    )
  })
  
  output$compare_country_report2 <- renderUI({
    country_report_func(tbl = summary_all(), data = df(), selected_country = input$country2)
  })
  
  output$compare_country1 <- renderUI({
    tagList(
      uiOutput("compare_country_select"),
      uiOutput("compare_country_report")
    )
  })
  
  output$compare_country2 <- renderUI({
    tagList(
      uiOutput("compare_country_select2"),
      uiOutput("compare_country_report2")
    )
  })
  
  
  
  output$filter_report <- renderUI({
    
    selected_attack_type <- ""
    if (!is.null(input$select_attack_type)) {
      selected_attack_type <- glue::glue("<li><b>Attack type</b>: {input$select_attack_type}</li>")
    }
    
    selected_certainty_level <- ""
    if (!is.null(input$select_certainty_level)) {
      selected_certainty_level <- glue::glue("<li><b>Certainty level</b>: {input$select_certainty_level}</li>")
    }
    
    selected_year <- glue::glue("<li><b>Year:</b> {input$select_year[1]}-{input$select_year[2]}</li>")
    
    if (!is.null(input$select_attack_type) | !is.null(input$select_certainty_level)) {
      tagList(
        div(style = "background:#ffa5a1; margin:0 10px 10px 10px; padding:10px 20px 10px 20px",
            HTML(
              glue::glue("The data is filtered for:<br>
                         <ul>
                            {selected_attack_type}
                            {selected_certainty_level}
                            {selected_year}
                         </ul>
                         ")
            )
        )
      )
    }
    
  })
  observe({
    if (!is.null(input$tabs)) {
      updateTabsetPanel(session, "tabs", selected = input$tabs)
    }
  })
  
  chart_type <- reactiveValues(type = "by_year")
  observeEvent(input$map_link, {chart_type$type <- "map"})
  observeEvent(input$year_link, {chart_type$type <- "by_year"})
  observeEvent(input$country_link, {chart_type$type <- "by_country"})
  
  observeEvent(input$tabs, {
    output$plot_report <- renderUI({
      indicator = input$tabs
      type = chart_type$type
      
      if (input$tabs != "about") {
        tagList(
          column(7, plot_wrapper_func(tbl = summary_all(), indicator = indicator, type = type)),
          column(5, main_report_func(summary_all(), data = df(), years = c(input$select_year[1], input$select_year[2]))),
          column(7, offset = 5,
                 # actionLink("map_link", "Map", style = ifelse(type == "map", "color:#007BBB;", "color: gray;")),
                 # " | ",
                 actionLink("year_link", "By Year", style = ifelse(type == "by_year", "color:#007BBB;", "color: gray;")),
                 " | ",
                 actionLink("country_link", "By Country", style = ifelse(type == "by_country", "color:#007BBB;", "color: gray;"))
          )
        )
      } else {
        helpText("placeholder...")
      }
      
    })
  })

}


shinyApp(ui = ui, server = server)
