#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard shinyWidgets ggplot2
#' @noRd
app_ui <- function(request) {
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  # BOXES ----------------------------------------------------------------

  
  # box_code <- 
  #   box(width = 8, 
  #       title = "Code within", 
  #       collapsible = TRUE, 
  #       collapsed = TRUE,
  #       shinyAce::aceEditor("ace_graph", height = 200, mode = "r", wordWrap = TRUE)
  #   )
  
  box_download <- 
    
  
  # PUT TOGETHER -----------------------------------------------------------------
  dashboardPage(
    dashboardHeader(title = "ggVIZ"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyjs::useShinyjs(),
      fluidRow(
        column(width = 4,
               mod_import_data_ui("x"),
               mod_vars_cont_fcts_ui("x"),
               mod_gg_layers_ui("x")
        ),
        column(width = 8,
               mod_out_table_ui("x"),
               mod_out_plot_ui("x"),
               mod_ace_editor_ui("x"),
               mod_download_plot_ui("x")
        )
      )
    )
  )
}
