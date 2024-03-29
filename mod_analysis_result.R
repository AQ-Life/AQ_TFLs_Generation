
mod_analysis_result_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           tableOutput(ns("TGtable")),
           verbatimTextOutput(ns("text"))
    )
  )
}

mod_analysis_result_server <- function(input, output, session, 
                                   data = NULL, data_name = NULL, 
                                   popdata = NULL, popdata_name = NULL, 
                                   GroupID = NULL, GroupID_name = NULL, 
                                   adam_var = NULL, adam_var_name = NULL,
                                   level1_var = NULL, level2_var = NULL,
                                   TGrequest = NULL) {
  ns <- session$ns
  
  # TGtable
  output$TGtable <- renderTable({
    req(data())
    req(popdata())
    req(GroupID())
    req(GroupID_name())
    req(TGrequest())
    
    if (TGrequest() == "Baseline Characterics"){
      req(adam_var())
      req(adam_var_name())
      table1(eval(parse(text = paste0("~ ",paste0(adam_var_name(),seq="",collapse = ' + '), " | ", GroupID_name()))),
             render.continuous = c(.="N (Nmiss)", .="Mean (SD)", .="Median", .="Min, Max"),
             topclass="Rtable1-grid Rtable1-shade Rtable1-times",
             data=data(), overall = "Total")
    } else if (TGrequest() == "Summary by SOC and PT"){
      req(level1_var)
      req(level2_var)
      SOCPTTable(popdata(), data(), level1_var(), level2_var())
    }
  })
  
  output$text <- renderPrint({
    req(GroupID())
    req(TGrequest())
    print(summary(GroupID()))
    print(TGrequest())
  })
}