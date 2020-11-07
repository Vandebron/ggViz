#' mod_download_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_download_plot_ui <- function(id){
  ns <- NS(id)
  box(width = 4,
      title = "Download",
      collapsible = TRUE, 
      collapsed = TRUE,
      sliderInput(ns("fig_height_download"), 
                  "Plot height", 
                  min = 3, max = 30, value = 14, 
                  step = 1, post = " cm"),
      sliderInput(ns("fig_width_download"),
                  "Plot width", 
                  min = 3, max = 30, value = 14, 
                  step = 1, post = " cm"),
      downloadButton(
        ns("download_plot_pdf"), "Download"
      )
  )
}
    
#'  mod_download_plot Server Function
#'
#' @noRd 
mod_download_plot_server <- function(id, plot) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    output$download_plot_pdf <- downloadHandler(
      filename = function(){
        paste("figure_ggplotVIZ_", Sys.time(), ".pdf", sep = "")
      },
      content = function(file){
        ggsave(file, 
               plot(), 
               width = input$fig_width_download,
               height = input$fig_height_download, 
               units = "cm")
      },
      contentType = "application/pdf"
    )
    
    })
}