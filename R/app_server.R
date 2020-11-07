#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  r_data <- reactiveValues(
    initial = NULL, 
    final = NULL
  )
  
  
  # INPUT ------------------------------------------------------------------------
  mod_import_data_server("x", r = r_data)
  
  mod_vars_cont_fcts_server("x", r = r_data)
  

  
  # BUILD CODE ----------------------------------------------------------
  
  code_graph <- mod_gg_layers_server("x", r = r_data)
  
  
  # OUTPUT -----------------------------------------------------------------------
  # table -------
  
  mod_out_table_server("x", r = r_data)
  
  
  # graph -------
  
  plot_to_output <- mod_ace_editor_server("x", r = r_data, code = code_graph)
  
  mod_out_plot_server("x", plot = plot_to_output)
  mod_download_plot_server("x", plot = plot_to_output)
  
  # download pdf ------
  # output$download_plot_PDF <- downloadHandler(
  #   filename = function(){
  #     paste("figure_ggplotVIZ_", Sys.time(), ".pdf", sep = "")
  #   },
  #   content = function(file){
  #     ggsave(file, 
  #            evaluated_graph(), 
  #            width = input$fig_width_download,
  #            height = input$fig_height_download, 
  #            units = "cm")
  #   },
  #   contentType = "application/pdf"
  # )
  


}
