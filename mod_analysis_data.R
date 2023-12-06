
mod_analysis_data_UI <- function(id) {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  fluidRow(
    column(12,
           fileInput(ns("adam_file"), label = "ADaM File:", buttonLabel = "Upload...", accept = c(".sas7bdat"), multiple = TRUE),
           selectInput(inputId = ns("TGrequest"),
                       label = "Table Type:",
                       choices = c("Baseline Characterics",
                                   "Summary by SOC and PT"
                       )),
           conditionalPanel(condition = "input.TGrequest == 'Summary by SOC and PT'", ns = ns,
                            selectInput(ns("POP_dataset"), label = "Population Dataset to analysis:", choices = NULL),
                            textAreaInput(ns("poptext"),"Provide a analysis population (e.g., FASFL == 'Y')")),
           selectInput(ns("adam_dataset"), label = "ADaM Dataset:", choices = NULL),
           textAreaInput(ns("filtertext"),"Provide a filter (e.g., FASFL == 'Y')"),
           conditionalPanel(condition = "input.TGrequest == 'Baseline Characterics'", ns = ns,
                            pickerInput(ns("GroupID"), label = "Group variable:", choices = NULL, options = list('actions-box' = TRUE), multiple = FALSE),
                            pickerInput(ns("adam_var"), label = "Columns to analysis:", choices = NULL, options = list('actions-box' = TRUE), multiple = TRUE)),
           conditionalPanel(condition = "input.TGrequest == 'Summary by SOC and PT'", ns = ns,
                            pickerInput(ns("level_1"), label = "Column of 1st level:", choices = NULL, options = list('actions-box' = TRUE), multiple = FALSE),
                            pickerInput(ns("level_2"), label = "Column of 2nd level:", choices = NULL, options = list('actions-box' = TRUE), multiple = FALSE)),
           shinyjs::disabled(actionButton(ns("AB_load"), label = "Load !")),
    )
  )
}


mod_analysis_data_server <- function(input, output, session) {
  ns <- session$ns
  
  toReturn    <-  reactiveValues(
    data = NULL,
    data_name = NULL,
    popdata = NULL,
    popdata_name = NULL,
    group_var = NULL,
    group_var_name = NULL,
    adam_var = NULL,
    adam_var_name = NULL,
    level1_var = NULL,
    level2_var = NULL,
    TGrequest = NULL,
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
    updateSelectInput(session, "POP_dataset", choices = names(file_list()))
  })
  
  observeEvent(input$adam_dataset, {
    df <- file_list()[[input$adam_dataset]]
    choices <- colnames(df)
    updatePickerInput(session, "GroupID", choices = choices)
    updatePickerInput(session, "adam_var", choices = choices)
    updatePickerInput(session, "level_1", choices = choices)
    updatePickerInput(session, "level_2", choices = choices)
  })
  
  # (Re)load button
  observeEvent(input$AB_load, {
    if (str_trim(input$filtertext != "")) {
      toRdata <- file_list()[[input$adam_dataset]] %>%
        filter(eval(parse(text = input$filtertext)))
    } else {
      toRdata <- file_list()[[input$adam_dataset]]
    }
    
    
    if (str_trim(input$poptext != "")) {
      toRpopdata <- file_list()[[input$POP_dataset]] %>%
        filter(eval(parse(text = input$poptext)))
    } else {
      toRpopdata <- file_list()[[input$POP_dataset]]
    }
    
    toReturn$data <- toRdata
    toReturn$data_name      <- input$adam_dataset
    toReturn$popdata <- toRpopdata
    toReturn$popdata_name      <- input$POP_dataset
    toReturn$GroupID       <- toRdata[,input$GroupID]
    toReturn$GroupID_name  <- input$GroupID
    toReturn$adam_var       <- toRdata[,input$adam_var]
    toReturn$adam_var_name  <- input$adam_var
    toReturn$level1_var       <- toRdata[,input$level_1]
    toReturn$level1_var_name  <- input$level_1
    toReturn$level2_var       <- toRdata[,input$level_2]
    toReturn$level2_var_name  <- input$level_2
    toReturn$TGrequest        <- input$TGrequest
    toReturn$trigger        <- toReturn$trigger + 1
  })
  
  return(toReturn)
}