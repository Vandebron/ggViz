#' mod_vars_cont_fcts UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_vars_cont_fcts_ui <- function(id){
  ns <- NS(id)
  
  box(width = 12, 
      title = "Continuous and Categorical variables",
      collapsible = TRUE, 
      collapsed = TRUE,
      multiInput(
        inputId = ns("which_cat"), 
        label = NULL,
        choices = c(""),
        selected = NA,
        options(
          enable_search = FALSE,
          non_selected_header = 'All variables:',
          selected_header = 'Categorical:'
        )
      )
  )
}
    
#'  mod_vars_cont_fcts Server Function
#'
#' @noRd 
mod_vars_cont_fcts_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input_list()$df, {
      updateMultiInput(session, "which_cat",
                       choices = names(input_list()$df),
                       selected = input_list()$cate_vars
      )
    })
    
    eventReactive(input$which_cat, {
      
      final_cont_vars <- 
        names(input_list()$df)[!(names(input_list()$df) %in% input$which_cat)]
      final_cate_vars <- input$which_cat
      
      df_final <- dplyr::mutate_at(
        input_list()$df,
        final_cate_vars,
        as.factor
        )
      
      list(
        df = df_final,
        cont_vars = final_cont_vars,
        cate_vars = final_cate_vars
        )
      })
    })
}
