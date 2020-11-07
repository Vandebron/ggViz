#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  
  input_original <- mod_import_data_server("x")
  input_final <- mod_vars_cont_fcts_server("x", input_list = input_original)
  
  unevaluated_plot_code <- mod_gg_layers_server("x", input_list = input_final)
  
  evaluated_plot <- mod_ace_editor_server("x", 
                                          input_list = input_final, 
                                          code = unevaluated_plot_code)
  
  mod_out_table_server("x", input_list = input_final)
  mod_out_plot_server("x", plot = evaluated_plot)
  mod_download_plot_server("x", plot = evaluated_plot)
}
