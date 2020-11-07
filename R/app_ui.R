#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard shinyWidgets ggplot2
#' @noRd
app_ui <- function(request) {
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  # BOXES ----------------------------------------------------------------
  box_upload <- mod_import_data_ui("x")

  box_continuous_categorical <- mod_vars_cont_fcts_ui("x")
    
  
  box_data_loaded <- 
    box(width = 12, 
        title = "Data loaded",
        collapsible = TRUE, 
        collapsed = FALSE,
        DT::DTOutput("out_table")
    )
  
  box_code <- 
    box(width = 8, 
        title = "Code within", 
        collapsible = TRUE, 
        collapsed = TRUE,
        shinyAce::aceEditor("ace_graph", height = 200, mode = "r", wordWrap = TRUE)
    )
  
  box_download <- 
    box(width = 4,
        title = "Download",
        collapsible = TRUE, 
        collapsed = TRUE,
        sliderInput("fig_height_download", 
                    "Plot height", 
                    min = 3, max = 30, value = 14, 
                    step = 1, post = " cm"),
        sliderInput("fig_width_download",
                    "Plot width", 
                    min = 3, max = 30, value = 14, 
                    step = 1, post = " cm"),
        downloadButton(
          "download_plot_PDF", "Download"
        )
    )
  
  # PUT TOGETHER -----------------------------------------------------------------
  dashboardPage(
    dashboardHeader(title = "ggplot VIZ"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyjs::useShinyjs(),
      fluidRow(
        verbatimTextOutput("text"),
        column(width = 4,
               box_upload,
               box_continuous_categorical,
               mod_gg_layers_ui("x")
        ),
        column(width = 8,
               box_data_loaded,
               box(width = 12,
                   plotOutput("out_ggplot")
               ),
               box_code,
               box_download
        )
      )
    )
  )
}
