#' mod_vars_cont_fcts UI Function
#'
#' @description A shiny Module.
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
mod_vars_cont_fcts_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    observeEvent(r$initial$df, {
      updateMultiInput(session, "which_cat",
                       choices = names(r$initial$df),
                       selected = names(purrr::keep(r$initial$df, is_categorical))
      )
    })
    
    observeEvent(input$which_cat, {
      
      final_vars_cont <- names(r$initial$df)[!(names(r$initial$df) %in% input$which_cat)]
      final_vars_cate <- input$which_cat
      
      df_final <- dplyr::mutate_at(
        r$initial$df,
        vars(r$final_vars_cate), as.factor
        )
      
      r$final <- list(
        df = df_final,
        vars_cont = final_vars_cont,
        vars_cate = final_vars_cate
        )
      })
    
    })
}