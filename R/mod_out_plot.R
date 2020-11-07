#' mod_out_plot UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_out_plot_ui <- function(id){
  ns <- NS(id)
  box(
    width = 12,
    plotOutput(ns("out_ggplot"))
  )
}
    
#'  mod_out_plot Server Function
#'
#' @noRd 
mod_out_plot_server <- function(id, plot) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    output$out_ggplot <- renderPlot(plot())
    })
}