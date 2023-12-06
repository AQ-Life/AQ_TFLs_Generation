
mod_data_preview_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           DT::dataTableOutput(ns("datapreview"))
    )
  )
}



mod_data_preview_server <- function(input, output, session, 
                             data = NULL, data_name = NULL, variable = NULL, variable_name = NULL) {
  
  ns <- session$ns
  
  # datapreview
  output$datapreview <- DT::renderDataTable({
    req(data())
    req(variable())
    
    adamdata1 <- mutate_if(data(), is.character, as.factor) %>% 
      select(colnames(variable()))
    DT::datatable(
      adamdata1,
      selection = "none",
      filter = list(position = "top"),
      escape = FALSE,
      extensions = "KeyTable",
      options = list(
        keys = TRUE,
        search = list(regex = TRUE),
        columnDefs = list(
          list(orderSequence = c("desc", "asc"), targets = "_all"),
          list(className = "dt-center", targets = "_all")
        ),
        autoWidth = TRUE,
        processing = FALSE,
        pageLength = 10
      )
    )
  })
  
}