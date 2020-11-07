#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom methods getGroupMembers
run_app <- function() {
  
  safe_ggplot_env <- {
    safe_base_functions_names <- c(
      getGroupMembers("Math"),
      getGroupMembers("Arith"),
      getGroupMembers("Compare"),
      getGroupMembers("Logic"),
      getGroupMembers("Summary"),
      "{", "(", "ifelse", "::", "c", "[", "[[", "$"
    )
    safe_base_functions <- 
      purrr::map(safe_base_functions_names, ~ get(., "package:base")) %>% 
      purrr::set_names(safe_base_functions_names) 
    
    pipe_function <- list(`%>%` = "%>%")
    
    ggplot_functions <- as.list(environment(ggplot2::ggplot))
    dplyr_functions <- as.list(environment(dplyr::select))
    tidyr_functions <- as.list(environment(tidyr::pivot_longer))
    
    rlang::new_environment(
      data = c(
        safe_base_functions,
        pipe_function,
        ggplot_functions,
        dplyr_functions,
        tidyr_functions
      ), 
      parent = rlang::empty_env()
    )
  }
  
  shinyApp(
    ui = app_ui,
    server = app_server
  )
}
