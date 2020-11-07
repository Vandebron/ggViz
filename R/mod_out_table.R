#' mod_out_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_out_table_ui <- function(id){
  ns <- NS(id)
  
  box(width = 12, 
      title = "Data loaded",
      collapsible = TRUE, 
      collapsed = FALSE,
      DT::DTOutput(ns("out_table"))
  )
}
    
#'  mod_out_table Server Function
#'
#' @noRd 
mod_out_table_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    output$out_table <- DT::renderDT({
      DT::datatable(
        r$final$df,
        rownames = FALSE,
        class = "row-border",
        options = list(
          scrollX = TRUE,
          pageLength = 8,
          dom = "rtip"
        )
      )
    })
    
    })
}