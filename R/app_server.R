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
  output$out_table <- DT::renderDT({
    DT::datatable(
      r_data$final$df,
      rownames = FALSE,
      class = "row-border",
      options = list(
        scrollX = TRUE,
        pageLength = 8,
        dom = "rtip"
      )
    )
  })
  
  # graph -------
  evaluated_graph <- eventReactive(input$ace_graph,{
    
    req(input$ace_graph)
    
    rlang::eval_tidy(
      rlang::parse_expr(input$ace_graph), 
      data = list(df = r_data$final$df), 
      env = generate_safe_env()
    )
  })
  
  output$out_ggplot <- renderPlot(evaluated_graph())
  
  # download pdf ------
  output$download_plot_PDF <- downloadHandler(
    filename = function(){
      paste("figure_ggplotVIZ_", Sys.time(), ".pdf", sep = "")
    },
    content = function(file){
      ggsave(file, 
             evaluated_graph(), 
             width = input$fig_width_download,
             height = input$fig_height_download, 
             units = "cm")
    },
    contentType = "application/pdf"
  )
  
  # OBSERVERS --------------------------------------------------------------------
  # update initial categorical / continuous vars in `which_cat` -----

  
  # output$text <- renderText({
  #   jsonlite::toJSON(
  #     list(
  #       df_zero = r_data$df_zero,
  #       df_initial = r_data$df_inital,
  #       df_final = r_data$df_final
  #     )
  #   )
  # })
  
  # update with output of `which_cat` -----------------------------
  
  
  # update text in ACE editor -------
  observeEvent(code_graph(), {
    shinyAce::updateAceEditor(
      session = session,
      editorId = "ace_graph",
      rlang::expr_text(code_graph())
    )
  })
  
  # toggle content in UI -------------


}
