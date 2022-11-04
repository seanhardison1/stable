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
    plotOutput(ns("plot"))
  )
}
    
#' data_processing Server Functions
#'
#' @noRd 
mod_data_processing_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      shinipsum::random_ggplot()
    })
  })
}
    
## To be copied in the UI
# mod_data_processing_ui("data_processing_1")
    
## To be copied in the server
# mod_data_processing_server("data_processing_1")
