#' mod_ace_editor UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_ace_editor_ui <- function(id){
  ns <- NS(id)
  
  box(
    width = 8,
    title = "Code within", 
    collapsible = TRUE, 
    collapsed = TRUE,
    shinyAce::aceEditor(
      ns("ace_graph"), 
      height = 200, 
      mode = "r", 
      wordWrap = TRUE
      )
  )
}
    
#'  mod_ace_editor Server Function
#'
#' @noRd 
mod_ace_editor_server <- function(id, input_list, code) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(code(), {
      shinyAce::updateAceEditor(
        session = session,
        editorId = "ace_graph",
        rlang::expr_text(code())
      )
    })
    
    evaluated_graph <- eventReactive(input$ace_graph,{
      
      req(input$ace_graph)
      
      rlang::eval_tidy(
        rlang::parse_expr(input$ace_graph), 
        data = list(df = input_list()$df), 
        env = generate_safe_env()
      )
    })
    
    evaluated_graph
    })
}
