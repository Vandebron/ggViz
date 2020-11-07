#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  # r_data <- reactiveValues(
  #   initial = NULL, 
  #   final = NULL
  # )
  
  input_original <- mod_import_data_server("x")
  input_final <- mod_vars_cont_fcts_server("x", input_list = input_original)
  

  # BUILD CODE ----------------------------------------------------------
  
  code_graph <- mod_gg_layers_server("x", input_list = input_final)
  
  
  # OUTPUT -----------------------------------------------------------------------
  # table -------
  
  mod_out_table_server("x", input_list = input_final)
  
  
  plot_to_output <- mod_ace_editor_server("x", input_list = input_final, code = code_graph)
  
  mod_out_plot_server("x", plot = plot_to_output)
  mod_download_plot_server("x", plot = plot_to_output)

}
