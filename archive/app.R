#' Run the Shiny Application
#'
#'https://shiny.posit.co/
#'https://www.shinyapps.io/
#'

options(encoding = "UTF-8")
library(haven)
library(plyr)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(table1)

filename <- dir(pattern = ".sas7bdat")
adamlist <- make.names(gsub(".sas7bdat", "", filename))
list2env(
  lapply(setNames(filename, adamlist),
         read_sas), envir = .GlobalEnv)
# save.image()
# load('.RData')

source("adam_data.R")
source("data_view.R")
source("analysis_data.R")
source("analysis_result.R")

# function
source("socpt.R")



ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel("Data View", 
             titlePanel(h1("Data View", align = "center", style = "font-weight:bold;color:#2573ba")),
             tags$br(),tags$br(),
             fluidRow(
               column(3,
                      panel(
                        heading = "Module : ADaM Dataset",
                        status = "info",
                        adam_data_UI(id = "mod_adam_data")
                      )
               ),
               column(9,
                      panel(
                        heading = "Module : Data Preview",
                        status = "success",
                        data_view_UI(id = "mod_data_preview")
                      )
               ))),
    
    tabPanel("Table Generator",
             titlePanel(h1("Table Generator", align = "center", style = "font-weight:bold;color:#2573ba")),
             tags$br(),tags$br(),
             fluidRow(
               column(3,
                      panel(
                        heading = "Analysis Dataset",
                        status = "info",
                        analysis_data_UI(id = "mod_analysis_data")
                      )
               ),
               column(9,
                      panel(
                        heading = "Analysis Result",
                        status = "success",
                        analysis_result_UI(id = "mod_analysis_result")
                      )
               ))
    )
  ))



server <- function(input, output, session) {
  
  # ReactiveValue that "belongs" to Application and updated through all modules
  rv <- reactiveValues(data = NULL, variable = NULL, GroupID = NULL, adam_var = NULL, 
                       TGrequest = NULL)
  
  {
    # Call module adam_data
    adam_data1 <- callModule(module = adam_data_server, id = "mod_adam_data")
    
    observeEvent(adam_data1$trigger, {
      req(adam_data1$trigger>0)
      rv$data <- adam_data1$data
      rv$variable    <- adam_data1$variable
    })
  }
  {
    # Call module data_view
    callModule(module = data_view_server, id = "mod_data_preview",
               data = reactive(rv$data),
               data_name = reactive(adam_data1$data_name),
               variable = reactive(rv$variable),
               variable_name = reactive(adam_data1$variable_name))
  }
  
  {
    # Call module analysis_data
    adam_data2 <- callModule(module = analysis_data_server, id = "mod_analysis_data")
    
    observeEvent(adam_data2$trigger, {
      req(adam_data2$trigger>0)
      rv$data <- adam_data2$data
      rv$GroupID <- adam_data2$GroupID
      rv$adam_var <- adam_data2$adam_var
      rv$TGrequest <- adam_data2$TGrequest
    })
  }
  {
    # Call module analysis_result
    callModule(module = analysis_result_server, id = "mod_analysis_result",
               data = reactive(rv$data),
               data_name = reactive(adam_data2$data_name),
               GroupID = reactive(rv$GroupID),
               GroupID_name = reactive(adam_data2$GroupID_name),
               adam_var = reactive(rv$adam_var),
               adam_var_name = reactive(adam_data2$adam_var_name),
               TGrequest = reactive(rv$TGrequest))
  }
}

shinyApp(ui = ui, server = server)
