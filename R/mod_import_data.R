#' mod_import_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_import_data_ui <- function(id){
  ns <- NS(id)
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  box(width = 12, 
      title = "Data",
      selectInput(ns("preload_select"), 
                  "Choose preloaded dataset",
                  names(cnf$datasets)
      ),
      fileInput(ns("file_input"), 
                "Or upload file",
                accept = c(
                  "text/csv", 
                  "application/vnd.ms-excel", 
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                ),
                placeholder = "CSV or EXCEL file"
      )
  )
}
    
#'  mod_import_data Server Function
#'
#' @noRd 
mod_import_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    cnf <- config::get(file = get_inst_file("config.yml"))
    
    observeEvent(input$preload_select, {
      r$initial$df <- eval(cnf$datasets[[input$preload_select]])
    })
    
    observeEvent(input$file_input, {
      req(input$file_input)
      file <- input$file_input$datapath
      extension <- tools::file_ext(file)
      
      df <- if (extension == "csv"){
        readr::read_csv(file)
      } else if (extension %in% c("xls", "xlsx")){
        readxl::read_excel(file)
      } else {
        showNotification(
          "Input a `.csv`, `.xls` or `.xlsx` file",
          type = "error")
      }
      
      req(extension %in% c("csv", "xls", "xlsx"))
      
      r$initial$df <- df
    })
    
    })
}