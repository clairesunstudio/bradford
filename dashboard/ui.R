library(shiny)
library(shinydashboard)
library(shinyURL)
library(shinyjs)

shinyUI(navbarPage(
  theme = "custom.css",
  # Application title
  title = "KPI Dashboard",
  #### VISITOR SUCCESS ####
  tabPanel(
    "Visitor Success",
    fluidPage(
      useShinyjs(),  # Include shinyjs
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            dateRangeInput(
              inputId = "visitor.success.daterange",
              label = "Select a Date Range",
              start = "2017-01-01",
              end = yesterday,  # sourced from global.R
              startview = "month"
            ),
            radioButtons(
              inputId = "visitor.success.type",
            label =  "Filter C1s",
              choices = c("All" = "all",
                          "Page Type" = "page.type",
                          "Service Type" = "service.type",
                          "Event Type" = "event.type"),
              selected = "all",
              inline = T),
              shinyjs::hidden(div(
                id = "advanced",
                uiOutput("type.selection.options"),
                br(),
                br())
            ),
            checkboxGroupInput(
              inputId = "visitor.success.group.by",
            label = "Group By",
              choices = c("Site Section Landing" = "site_section",
                          "Topic" = "topic",
                          "Sub-Topic" = "subtopic",
                          "Event Type" = "event_action",
                          "Referrer" = "source",
                          "Page Type" = "content_type"),
              selected = "site_section",
              inline = F
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              selectInput(
                inputId = "visitor.success.top.bottom",
              label = "Limit groups displayed to",
                choices = c("Top" = "top",
                            "Bottom" = "bottom"),
                selected = "top"
              ),
              numericInput(
                inputId = "visitor.success.select.k",
              label = br(),
                value = 5,
                min = 1,
                max = 5
              )
            ),
          radioButtons(
            inputId = "visitor.success.units",
            label = "Display Unit",
            choices = c("Percent" = "percent",
                        "Number" = "number"),
            selected = "percent",
            inline = T
          ),
            # URL generator
            shinyURL.ui(display = T, copyURL = T, tinyURL = T)
          ),
          mainPanel(
            plotlyOutput("visitor.success.grouped.pareto"),
            downloadButton("visitor.success.download.aggregate", "Download Plot Data"),
            br(),
            plotlyOutput("visitor.success.grouped.timeseries"),
            downloadButton("visitor.success.download.timeseries", "Download Plot Data")
          )
        )
      )
    )
  ),

  #### SUCCESS RATE ####
  tabPanel(title = "Success Rate",
           radioButtons(
             inputId = "success.rate.percent",
             label = "Select Unit",
             choices = c("Percent" = TRUE,
                         "Number" = FALSE),
             selected = FALSE,
             inline = T
           ),
           hr(),
           fluidRow(
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("topic.sessions"),
               plotlyOutput("topic.conversions"))),
           br(),
           plotlyOutput("topic.conversion.rate"))
))
