#' mod_gg_layers UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_gg_layers_ui <- function(id){
  ns <- NS(id)
  
  cnf <- config::get(file = get_inst_file("config.yml"))
  
  # first line and geom -----------------------------------------------------
  tab_panel_geom <- 
    tabPanel(title = "Geometry",
             selectInput(
               inputId = ns("viztype"),
               label = "Type of graph (Geom used)",
               choices = cnf$choices$geoms
             ),
             prettySwitch(
               inputId = ns("alpha_05"),
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
                 inputId = ns("position"),
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
  
  # stats -------------------------------------------------------------------
  tab_panel_stats <- tabPanel(
    title = "Stats",
    tags$div(
      id = ns("toggle_regression"),
      prettySwitch(
        inputId = ns("show_regression_line"),
        label = "Show regression line",
        value = FALSE,
        status = "primary", slim = TRUE)
    ),
    tags$div(
      id = ns("toggle_smooth_func"),
      selectInput(
        ns("smooth_func"),
        "Smoothening function",
        choices = cnf$choices$regression)
    ),
    tags$div(
      id = ns("toggle_bins"),
      sliderInput(
        ns("bins"),
        "Number of histogram bins",
        min = 10, max = 100, value = 30, step = 1
        )
    )
    
  )
  
  # facet --------------------------------------------------------------------
  tab_panel_facet <- tabPanel(
    title = "Facet",
    radioGroupButtons(
      inputId = ns("facet_type"),
      label = "Facet Type", 
      choices = cnf$choices$facets
    ),
    selectInput(
      ns("facet_row"),
      "Facet Row",
      choices = ""
    ),
    selectInput(
      ns("facet_col"), 
      "Facet Column", 
      choices = ""
    ),
    prettySwitch(
      inputId = ns("facet_free_x"),
      label = "Free X axis scale",
      value = FALSE,
      status = "primary", 
      slim = TRUE
    ),
    prettySwitch(
      inputId = ns("facet_free_y"),
      label = "Free Y axis scale",
      value = FALSE,
      status = "primary", 
      slim = TRUE
    )
  )
  
  # coord ------------------------------------------------------------------
  tab_panel_coord <- tabPanel(
    title = "Coordinates",
    selectInput(
      ns("coord"),
      "Coordinate type",
      choices = cnf$choices$coordinates
      ),
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
      prettySwitch(
        inputId = ns("include_x_origin"),
        label = "Force inclusion of X axis origin (zero)",
        value = FALSE,
        status = "primary", slim = TRUE),
      prettySwitch(
        inputId = ns("include_y_origin"),
        label = "Force inclusion of Y axis origin (zero)",
        value = FALSE,
        status = "primary", slim = TRUE
        )
    )
  )
  

  
  # labels -----------------------------------------------------------------
  tab_panel_label <- tabPanel(
    title = "Labels",
    textInput(
      ns("lab_title"), 
      "Title:", 
      value = NA
      ),
    textInput(
      ns("lab_x"),
      "X-axis:",
      value = NA
      ),
    textInput(
      ns("lab_y"),
      "Y-axis:",
      value = NA
      ),
    textInput(
      ns("lab_caption"),
      "Caption:",
      value = NA
      )
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
    selectInput(
      ns("color_palette"),
      "Color palette",
      choices = available_palettes
      ),
    selectInput(
      ns("theme_style"),
      "Theme Style",
      choices = cnf$choices$themes,
      selected = "theme_grey()"
      ),
    selectInput(
      ns("pos_legend"),
      "Position legend",
      choices = cnf$choices$legend_positions
    ),
    prettySwitch(
      inputId = ns("show_gridlines"),
      label = "Show gridlines",
      value = TRUE,
      status = "primary",
      slim = TRUE
      ),
    prettySwitch(
      inputId = ns("show_x_axis"),
      label = "Show X axis",
      value = TRUE,
      status = "primary",
      slim = TRUE
      ),
    prettySwitch(
      inputId = ns("show_y_axis"),
      label = "Show Y axis",
      value = TRUE,
      status = "primary",
      slim = TRUE
      )
  )
  
# .. build ----------------------------------------------------------------
  tabBox(
    width = 12,
    tab_panel_geom,
    tab_panel_stats,
    tab_panel_facet,
    tab_panel_coord,
    tab_panel_label,
    tab_panel_theme
  )
}
