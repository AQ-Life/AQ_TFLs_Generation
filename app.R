rm(list = ls())

options(encoding = "UTF-8")
library(shiny)
library(shinyWidgets)
library(haven)
library(plyr)
library(tidyverse)
library(table1)

# Function
source("socpt.R")
source("rbindf.R")

# Module
source("mod_adam_data.R")
source("mod_data_preview.R")
source("mod_analysis_data.R")
source("mod_analysis_result.R")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Data View",
             titlePanel(h1("Data View", align = "Center", style = "font-weight:bold;color:#2573ba")),
             tags$br(),tags$br(),
             fluidRow(
               column(3,
                      panel(heading = "Module:ADaM Dataset",
                            status = "info",
                            mod_adam_data_UI(id = "mod_adam_data"))
                      ),
               column(9,
                      panel(heading = "Module: Data Preview",
                             status = "success",
                             mod_data_preview_UI(id="mod_data_preview"))
                      )
             ))
    ,
    tabPanel("Table Generator",
             titlePanel(h1("Table Generator", align = "center", style = "font-weight:bold;color:#2573ba")),
             tags$br(),tags$br(),
             fluidRow(
               column(3,
                      panel(
                        heading = "Analysis Dataset",
                        status = "info",
                        mod_analysis_data_UI(id = "mod_analysis_data"))
                      ),
               column(9,
                      panel(
                        heading = "Analysis Result",
                        status = "success",
                        mod_analysis_result_UI(id = "mod_analysis_result")
                      )
               ))
             )
             
  )
)


server <- function(input, output, session){
  # ReactiveValue that "belongs" to Application and updated through all modules
  rv <- reactiveValues(data = NULL, variable = NULL, GroupID = NULL, adam_var = NULL, 
                       TGrequest = NULL)
  
  {
    # Call module mod_adam_data
    Module_data1 <- callModule(module = mod_adam_data_server, id = "mod_adam_data")
  }

  {
    # Call module mod_data_view
    callModule(module = mod_data_preview_server, id = "mod_data_preview",
               data = reactive(Module_data1$data),
               variable = reactive(Module_data1$variable))
  }
  
  {
    # Call module analysis_data
    Module_data2 <-  callModule(module = mod_analysis_data_server, id = "mod_analysis_data")
  }
  {
    # Call module analysis_result
    callModule(module = mod_analysis_result_server, id = "mod_analysis_result",
               data = reactive(Module_data2$data),
               data_name = reactive(Module_data2$data_name),
               popdata = reactive(Module_data2$popdata),
               popdata_name = reactive(Module_data2$popdata_name),
               GroupID = reactive(Module_data2$GroupID),
               GroupID_name = reactive(Module_data2$GroupID_name),
               adam_var = reactive(Module_data2$adam_var),
               adam_var_name = reactive(Module_data2$adam_var_name),
               level1_var = reactive(Module_data2$level1_var),
               level2_var = reactive(Module_data2$level2_var),
               TGrequest = reactive(Module_data2$TGrequest))
  }
  
}


shinyApp(ui = ui, server = server)

