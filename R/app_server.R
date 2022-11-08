#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  df <- mod_data_import_server("data_import_1")
  
  mod_data_processing_server("data_processing_1", df)
  
}
