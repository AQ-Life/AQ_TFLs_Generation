
analysis_data_UI <- function(id) {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  fluidRow(
    column(12,
           selectInput(ns("TG_dataset"), label = "ADaM Dataset:", choices = c("adsl", "adae"), selected = "adsl"),
           textAreaInput(ns("filtertext"),"Provide a filter (e.g., FASFL == 'Y')"),
           selectInput(inputId = ns("TGrequest"),
                       label = "Table Type:",
                       choices = c("Baseline Characterics",
                                   "Summary by SOC and PT"
                       )),
           selectInput(ns("GroupID"), label = "Group variable", choices = "TRTAN"),
           conditionalPanel(condition = "input.TGrequest == 'Baseline Characterics'", ns = ns,
                            pickerInput(ns("adam_var"), label = "Columns to analysis:", choices = NULL, options = list('actions-box' = TRUE), multiple = TRUE)),
           shinyjs::disabled(actionButton(ns("AB_load"), label = "Load !"))
    )
  )
}


analysis_data_server <- function(input, output, session) {
  
  ns <- session$ns
  
  toReturn    <-  reactiveValues(
    data = NULL,
    data_name = NULL,
    group_var = NULL,
    group_var_name = NULL,
    adam_var = NULL,
    adam_var_name = NULL,
    TGrequest = NULL,
    trigger = 0
  )
  
  # Update selectInput according to dataset
  observe({
    if (!is.null(input$TG_dataset)) {
      df <- get(input$TG_dataset)
      choices <- colnames(df)
      updateSelectInput(session, "GroupID", choices = choices)
      updateSelectInput(session, "SOCID", choices = choices)
      updateSelectInput(session, "PTID", choices = choices)
      updatePickerInput(session, "adam_var", choices = choices)
    }
  })

  # (Re)load button
  observeEvent(input$AB_load, {
    if (str_squish(input$filtertext != "")) {
      toReturn$data <- get(input$TG_dataset) %>%
        filter(eval(parse(text = input$filtertext)))
    } else {
      toReturn$data <- get(input$TG_dataset)
    }
    toReturn$data_name      <- input$TG_dataset 
    toReturn$GroupID       <- get(input$TG_dataset)[,input$GroupID]
    toReturn$GroupID_name  <- input$GroupID
    toReturn$adam_var       <- get(input$TG_dataset)[,input$adam_var]
    toReturn$adam_var_name  <- input$adam_var
    toReturn$TGrequest        <- input$TGrequest
    toReturn$trigger        <- toReturn$trigger + 1
  })
  
  return(toReturn)
}