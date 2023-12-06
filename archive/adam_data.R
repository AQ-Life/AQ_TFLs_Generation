
adam_data_UI <- function(id) {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  fluidRow(
    column(12,
           selectInput(ns("adam_dataset"), label = "ADaM Dataset:", choices = c("adsl", "adae"), selected = "adsl"),
           pickerInput(ns("adam_var"), label = "Columns to show:", choices = NULL, options = list('actions-box' = TRUE), multiple = TRUE),
           shinyjs::disabled(actionButton(ns("AB_load"), label = "Load !"))
    )
  )
}


adam_data_server <- function(input, output, session) {
  
  ns <- session$ns
  
  toReturn    <-  reactiveValues(
    data = NULL,
    data_name = NULL,
    variable = NULL,
    variable_name = NULL,
    trigger = 0
  )
  
  # Update selectInput according to dataset
  observe({
    if (!is.null(input$adam_dataset)) {
      df <- get(input$adam_dataset)
      choices <- colnames(df)
      updatePickerInput(session, "adam_var", choices = choices)
    }
  })
  
  
  # (Re)load button
  observeEvent(input$AB_load, {
    toReturn$data           <- get(input$adam_dataset)
    toReturn$data_name      <- input$adam_dataset
    toReturn$variable       <- get(input$adam_dataset)[,input$adam_var]
    toReturn$variable_name  <- input$adam_var
    toReturn$trigger        <- toReturn$trigger + 1
  })
  
  return(toReturn)
}