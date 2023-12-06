
mod_adam_data_UI <- function(id) {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  fluidRow(
    column(12,
           fileInput(ns("adam_file"), label = "ADaM File:", buttonLabel = "Upload...", accept = c(".sas7bdat"), multiple = TRUE),
           selectInput(ns("adam_dataset"), label = "ADaM Dataset:", choices = NULL),
           pickerInput(ns("adam_var"), label = "Columns to show:", choices = NULL, options = list('actions-box' = TRUE), multiple = TRUE),
           shinyjs::disabled(actionButton(ns("AB_load"), label = "Load !")),
           # verbatimTextOutput(ns("text_print"))
    )
  )
}


mod_adam_data_server <- function(input, output, session) {
  
  ns <- session$ns
  
  toReturn    <-  reactiveValues(
    data = NULL,
    data_name = NULL,
    variable = NULL,
    variable_name = NULL,
    trigger = 0
  )
  
  file_list <- reactive({
    if (is.null(input$adam_file)){
      return(NULL)
    }
    file_list <- lapply(input$adam_file$datapath, function(path){
      read_sas(path)
    })
    names(file_list) <- input$adam_file$name
    file_list
  })
  
  observeEvent(file_list(), {
    updateSelectInput(session, "adam_dataset", choices = names(file_list()))
  })
  
  observeEvent(input$adam_dataset, {
    df <- file_list()[[input$adam_dataset]]
    choices <- colnames(df)
    updatePickerInput(session, "adam_var", choices = choices)
  })
  
  # output$text_print <- renderPrint({
  #   print(input$adam_dataset)
  #   print(input$adam_var)
  #   file_list()[[input$adam_dataset]]
    # file_list()[[input$adam_dataset]][,input$adam_var]
  # })
  
  # ToReturn
  observeEvent(input$AB_load, {
    if (!is.null(input$adam_dataset) && !is.null(input$adam_var)) {
    toReturn$data           <- file_list()[[input$adam_dataset]]
    toReturn$data_name      <- input$adam_dataset
    toReturn$variable       <- file_list()[[input$adam_dataset]][,input$adam_var]
    toReturn$variable_name  <- input$adam_var
    toReturn$trigger        <- toReturn$trigger + 1
    }
  })
  
  return(toReturn)
}
