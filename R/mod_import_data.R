#' mod_import_data UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_import_data_ui <- function(id){
  ns <- NS(id)
  
  datasets <- config::get("datasets", file = get_inst_file("config.yml"))
  
  box(width = 12, 
      title = "Data",
      selectInput(
        ns("preload_select"), 
        "Choose preloaded dataset",
        names(datasets)
      ),
      fileInput(
        ns("file_input"),
        "Or upload file",
        accept = c(
          "text/csv", 
          "application/vnd.ms-excel", 
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          ),
        placeholder = "CSV or EXCEL file"
      )
  )
}
    
#'  mod_import_data Server Function
#'
#' @noRd 
mod_import_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session
    
    datasets <- config::get("datasets", file = get_inst_file("config.yml"))
    
    df <- reactiveVal()
    
    observeEvent(input$preload_select, {
      x <- eval(datasets[[input$preload_select]])
      df(x)
    })
    
    observeEvent(input$file_input, {
      req(input$file_input)
      file <- input$file_input$datapath
      extension <- tools::file_ext(file)
      
      x <- if (extension == "csv"){
        readr::read_csv(file)
      } else if (extension %in% c("xls", "xlsx")){
        readxl::read_excel(file)
      } else {
        showNotification(
          "Input a `.csv`, `.xls` or `.xlsx` file",
          type = "error")
      }
      
      req(extension %in% c("csv", "xls", "xlsx"))
      
      df(x)
    })
    
    reactive({
      original_cate_vars <- names(purrr::keep(df(), is_categorical))
      original_cont_vars <- names(df())[!(names(df()) %in% original_cate_vars)]
      
      list(
        df = df(),
        cont_vars = original_cont_vars,
        cate_vars = original_cate_vars
        )
      })
    
    })
}