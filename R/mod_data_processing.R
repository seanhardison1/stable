#' data_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    plotOutput(ns("plot"))
    # plotOutput(ns("fig1"))
  )
}
    
#' data_processing Server Functions
#'
#' @noRd 
mod_data_processing_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if (is.null(df$df_data())) {
        shinyjs::hide("plot")
      } else {
        shinyjs::show("plot")
      }
    })
    output$plot <- renderPlot({
      if (!is.null(df$df_data())) {
        ggplot2::ggplot(df$df_data()) +
          ggplot2::geom_histogram(ggplot2::aes(`Atlantic croaker`))
      } 
    })
   
  })
}
    
## To be copied in the UI
# mod_data_processing_ui("data_processing_1")
    
## To be copied in the server
# mod_data_processing_server("data_processing_1")
