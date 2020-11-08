#' mod_gg_layers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_gg_layers_ui <- function(id){
  ns <- NS(id)
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  tab_panel_geom <- 
    tabPanel(title = "Geometry",
             selectInput(inputId = ns("viztype"),
                         label = "Type of graph (Geom used)",
                         choices = cnf$choices$geoms
             ),
             prettySwitch(inputId = ns("alpha_05"),
                          label = "Make semitransparent",
                          value = FALSE,
                          status = "primary", slim = TRUE),
             hr(),
             selectInput(
               ns("x_var"), 
               HTML(paste("X-variable", icon("arrows-alt-h"))), 
               choices = ""
             ),
             tags$div(
               id = ns("toggle_y_var"),
               selectInput(
                 ns("y_var"), 
                 HTML(paste("Y-variable", icon("arrows-alt-v"))), 
                 choices = ""
               )
             ),
             hr(),
             selectInput(
               ns("group"),
               "Group - Colour", 
               choices = ""
               ),
             tags$div(
               id = ns("toggle_position"),
               radioGroupButtons(
                 inputId = "position",
                 label = "Geom grouping position", 
                 choices = cnf$choices$positions
               )
             ),
             tags$div(
               id = ns("toggle_size"),
               selectInput(ns("size"), "Group - Size", choices = "")
             ),
             tags$div(
               id = ns("toggle_shape"),
               selectInput(ns("shape"), "Group - Shape", choices = "")
             )
    )
  
  # stats ------------------------------------------------------------------------
  tab_panel_stats <- tabPanel(
    title = "Stats",
    tags$div(
      id = ns("toggle_regression"),
      prettySwitch(inputId = ns("show_regression_line"),
                   label = "Show regression line",
                   value = FALSE,
                   status = "primary", slim = TRUE)
    ),
    tags$div(
      id = ns("toggle_smooth_func"),
      selectInput(ns("smooth_func"), 
                  "Smoothening function",
                  choices = cnf$choices$regression)
    ),
    tags$div(
      id = ns("toggle_bins"),
      sliderInput(ns("bins"), 
                  "Number of histogram bins", 
                  min = 10, max = 100, value = 30, step = 1)
    )
    
  )
  
  # coord ------------------------------------------------------------------------
  tab_panel_coord <- tabPanel(
    title = "Coordinates",
    selectInput(ns("coord"),
                "Coordinate type", 
                choices = cnf$choices$coordinates),
    tags$div(
      id = ns("toggle_coord_thetha"),
      radioGroupButtons(
        inputId = ns("polar_coord_type"),
        label = "Polar coordinate axis", 
        choices = cnf$choices$coordinates_axis,
        selected = cnf$choices$coordinates_axis[2]
      )
    ),
    tags$div(
      id = ns("toggle_coord_origin"),
      prettySwitch(inputId = ns("include_x_origin"),
                   label = "Force inclusion of X axis origin (zero)",
                   value = FALSE,
                   status = "primary", slim = TRUE),
      prettySwitch(inputId = ns("include_y_origin"),
                   label = "Force inclusion of Y axis origin (zero)",
                   value = FALSE,
                   status = "primary", slim = TRUE)
    )
  )
  
  # facet ------------------------------------------------------------------------
  tab_panel_facet <- tabPanel(
    title = "Facet",
    radioGroupButtons(
      inputId = ns("facet_type"),
      label = "Facet Type", 
      choices = cnf$choices$facets
    ),
    selectInput(ns("facet_row"), "Facet Row", choices = ""),
    selectInput(ns("facet_col"), "Facet Column", choices = ""),
    prettySwitch(inputId = ns("facet_free_x"),
                 label = "Free X axis scale",
                 value = FALSE,
                 status = "primary", slim = TRUE),
    prettySwitch(inputId = ns("facet_free_y"),
                 label = "Free Y axis scale",
                 value = FALSE,
                 status = "primary", slim = TRUE)
  )
  
  # labels -----------------------------------------------------------------------
  tab_panel_label <- tabPanel(
    title = "Labels",
    textInput(ns("lab_title"), "Title:", value = NA),
    textInput(ns("lab_x"), "X-axis:", value = NA),
    textInput(ns("lab_y"), "Y-axis:", value = NA),
    textInput(ns("lab_caption"), "Caption:", value = NA)
    
  )
  
  # theme ------------------------------------------------------------------------
  available_palettes <- list(
    "Default" = names(cnf$palettes$custom$default),
    "Categorical" = c(
      cnf$palettes$rcolorbrewer$categorical, 
      names(cnf$palettes$custom$categorical)
    ), 
    "Continuous - Diverging" = cnf$palettes$rcolorbrewer$diverging,
    "Continuous - Sequential" = cnf$palettes$rcolorbrewer$sequential
  )
  
  tab_panel_theme <- tabPanel(
    title = "Theme",
    selectInput(ns("color_palette"), "Color palette",
                choices = available_palettes),
    selectInput(ns("theme_style"), "Theme Style",
                choices = cnf$choices$themes,
                selected = "theme_grey()"),
    selectInput(ns("pos_legend"), "Position legend",
                choices = cnf$choices$legend_positions
    ),
    prettySwitch(inputId = ns("show_gridlines"),
                 label = "Show gridlines",
                 value = TRUE,
                 status = "primary", slim = TRUE),
    prettySwitch(inputId = ns("show_x_axis"),
                 label = "Show X axis",
                 value = TRUE,
                 status = "primary", slim = TRUE),
    prettySwitch(inputId = ns("show_y_axis"),
                 label = "Show Y axis",
                 value = TRUE,
                 status = "primary", slim = TRUE)
    
  )
  
  tabBox(width = 12,
         tab_panel_geom,
         tab_panel_stats,
         tab_panel_facet,
         tab_panel_coord,
         tab_panel_label,
         tab_panel_theme
  )
}
    
#'  mod_gg_layers Server Function
#'
#' @noRd 
mod_gg_layers_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cnf <- config::get(file = get_inst_file("config.yml"))
    
    geom_is_filled <- reactive(input$viztype %in% cnf$specials$geoms$require_fill)
    
    input_viztype <- reactive(input$viztype)
    
    first_line_code <- reactive({
      aesthetics <- rlang::exprs(y = !!sym(input$y_var))
      
      if (input$x_var != "NA"){
        aesthetics$x <- expr(!!sym(input$x_var))
      } else {
        aesthetics$x <- expr(NA)
      }
      
      if (input$group != "NA") {
        if (geom_is_filled()){
          aesthetics$fill <- expr(!!sym(input$group))
        } else {
          aesthetics$color <- expr(!!sym(input$group))
        }
      }
      
      if ((input$viztype %in% cnf$specials$geoms$allow_size)
          && (input$size != "NA")){
        aesthetics$size <- expr(!!sym(input$size))
      }
      
      if ((input$viztype %in% cnf$specials$geoms$allow_shape)
          && (input$shape != "NA")){
        aesthetics$shape <- expr(!!sym(input$shape))
      }
      
      if (input$viztype %in% cnf$specials$geoms$deactivate_y){
        showNotification("Histograms do not use the Y axis")
        aesthetics$y <- NULL
      }
      
      expr(
        ggplot2::ggplot(df, aes(!!!aesthetics))
      )
    })
    
    geom_code <- reactive({
      geom_args <- rlang::exprs()
      
      if (input$viztype %in% cnf$specials$geoms$allow_bins){
        geom_args$bins <- expr(!!input$bins)
      } 
      
      if (input$viztype %in% cnf$specials$geoms$allow_position){
        geom_args$position <- expr(!!input$position)
      } 
      
      if (input$alpha_05 == TRUE) geom_args$alpha <- expr(0.5)
      
      expr(
        (!!sym(input$viztype))(!!!geom_args)
      )
    })
    
    stats_code <- reactive({
      if ((input$viztype %in% cnf$specials$geoms$allow_regression)
                        && (input$show_regression_line == TRUE)){
        expr(stat_smooth(method=!!input$smooth_func))
      }
    })
    
    facet_code <- reactive({
      if (input$facet_row != "." || input$facet_col != "."){
        
        facet_args <- rlang::exprs(!!sym(input$facet_row) ~ !!sym(input$facet_col))
        
        x_free <- input$facet_free_x
        y_free <- input$facet_free_y
        
        scales <- dplyr::case_when(
          sum(x_free, y_free) == 0 ~ "fixed",
          sum(x_free, y_free) == 2 ~ "free",
          x_free == TRUE && y_free == FALSE ~ "free_x",
          x_free == FALSE && y_free == TRUE ~ "free_y"
        )
        
        if (scales != "fixed"){
          facet_args$scales = expr(!!scales)
        }
        
        expr(
          (!!sym(input$facet_type))(!!!facet_args)
        )
      }
    })
    
    coord_code <- reactive({
      coord_args <- rlang::exprs()
      
      if (input$coord %in% cnf$specials$coord$allow_origin){
        if (input$include_x_origin == TRUE){
          coord_args$xlim <- expr(c(0,max(df[[!!input$x_var]])))
        }
        if (input$include_y_origin == TRUE){
          coord_args$ylim <- expr(c(0,max(df[[!!input$y_var]])))
        }
      }
      
      if (input$coord %in% cnf$specials$coord$allow_thetha){
        coord_args$theta <- expr(!!input$polar_coord_type)
      }
      
      if (input$coord == "coord_cartesian" && purrr::is_empty(coord_args)){
        NULL
      } else {
        expr(
          (!!sym(input$coord))(!!!coord_args)
        )
      }
    })
    
    labs_code <- reactive({
      labs_args <- rlang::exprs()
      if (input$lab_title != "") labs_args$title <- expr(!!input$lab_title)
      if (input$lab_x != "") labs_args$x <- expr(!!input$lab_x)
      if (input$lab_y != "") labs_args$y <- expr(!!input$lab_y)
      if (input$lab_caption != "") labs_args$caption <- expr(!!input$lab_caption)
      
      if (!purrr::is_empty(labs_args)){
        expr(labs(!!!labs_args))
      }
    })
    
    theme_style_code <- reactive({
      if (input$theme_style != "theme_grey") {
        expr(
          (!!sym(input$theme_style))()
        )
      }
    })
    
    theme_palette_code <- reactive({
      if (input$group != "NA" && input$color_palette != "ggplot2 default") {
        palette_args <- rlang::exprs()
        
        custom_palettes <- purrr::flatten(cnf$palettes$custom)
        
        if (input$color_palette %in% names(custom_palettes)){
          palette_args$values <- expr(
            !!(custom_palettes[[input$color_palette]])
          )
          funcion_name_suffix <- "manual"
        } else {
          palette_args$palette <- expr(!!input$color_palette)
          funcion_name_suffix <- ifelse(
            input$group %in% input_list()$cate_vars, 
            "brewer", 
            "distiller"
          )
        }
        
        function_name <- paste(
          "scale", 
          ifelse(geom_is_filled(), "fill", "color"), 
          funcion_name_suffix, 
          sep = "_"
        )
        
        expr(
          (!!sym(function_name))(!!!palette_args)
        )
      }
    })
    
    theme_elements_code <- reactive({
      theme_elements_args <- rlang::exprs()
      
      if (input$pos_legend != "right"){
        theme_elements_args$legend.position <- expr(!!input$pos_legend)
      } 
      
      if (input$show_gridlines == FALSE){
        theme_elements_args$panel.grid.major <- expr(element_blank())
        theme_elements_args$panel.grid.minor <- expr(element_blank())
      }
      
      if (input$show_x_axis == FALSE){
        theme_elements_args$axis.title.x <- expr(element_blank())
        theme_elements_args$axis.text.x <- expr(element_blank())
        theme_elements_args$axis.ticks.x <- expr(element_blank())
      }
      
      if (input$show_y_axis == FALSE){
        theme_elements_args$axis.title.y <- expr(element_blank())
        theme_elements_args$axis.text.y <- expr(element_blank())
        theme_elements_args$axis.ticks.y <- expr(element_blank())
      }
      
      if (!purrr::is_empty(theme_elements_args)){
        expr(theme(!!!theme_elements_args))
      }
    })
    
    code_graph <- reactive({

      all_layers_code <- list(
        first_line_code(),
        geom_code(),
        facet_code(),
        stats_code(),
        coord_code(),
        labs_code(),
        theme_style_code(),
        theme_palette_code(),
        theme_elements_code()
      )
      
      all_layers_code %>% 
        purrr::discard(purrr::is_empty) %>% 
        # take all non-empty parts of `all_layers_code` and put a `+` between them
        purrr::reduce(function(x,y) expr(!!enexpr(x) + !!enexpr(y)))
    })
    
    observe({
      shinyjs::toggle("toggle_y_var",
                      condition = !(input$viztype %in% cnf$specials$geoms$deactivate_y),
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_size",
                      condition = input$viztype %in% cnf$specials$geoms$allow_size,
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_shape",
                      condition = input$viztype %in% cnf$specials$geoms$allow_shape,
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_position",
                      condition = input$viztype %in% cnf$specials$geoms$allow_position,
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_regression",
                      condition = input$viztype %in% cnf$specials$geoms$allow_regression,
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_smooth_func",
                      condition = (input$viztype %in% cnf$specials$geoms$allow_regression
                                   && input$show_regression_line),
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_bins",
                      condition = input$viztype %in% cnf$specials$geoms$allow_bins,
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_coord_thetha",
                      condition = input$coord %in% cnf$specials$coord$allow_thetha,
                      anim = TRUE)
    })

    observe({
      shinyjs::toggle("toggle_coord_origin",
                      condition = input$coord %in% cnf$specials$coord$allow_origin,
                      anim = TRUE)
    })
    
    observeEvent(input_list(),{

      choices_cont <- from_vars_to_choice_list(input_list()$cont_vars)
      choices_cate <-  from_vars_to_choice_list(input_list()$cate_vars)

      updateSelectInput(session, "x_var", choices = list(
        Continuous = choices_cont,
        Categorical = choices_cate,
        `Choose no variable` = list(NA)
      ))

      updateSelectInput(
        session, "y_var",
        choices = list(Continuous = choices_cont),
        selected = ifelse(length(choices_cont) > 1, choices_cont[2], choices_cont[1])
      )

      updateSelectInput(session, "group",  choices = list(
        `Choose no variable` = list(NA),
        Continuous = choices_cont,
        Categorical = choices_cate
      ))

      updateSelectInput(session, "size",  choices = list(
        `Choose no variable` = list(NA),
        Continuous = choices_cont
      ))

      updateSelectInput(session, "shape",  choices = list(
        `Choose no variable` = list(NA),
        Categorical = choices_cate
      ))

      updateSelectInput(session, "facet_row",  choices = list(
        `Choose no variable` = list("NA" = "."),
        Categorical = choices_cate
      ))

      updateSelectInput(session, "facet_col",  choices = list(
        `Choose no variable` = list("NA" = "."),
        Categorical = choices_cate
      ))
    })
    
    code_graph
    
    })
}