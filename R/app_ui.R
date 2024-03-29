#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    htmlTemplate(
      app_sys("app/www/template.html"), 
      first = mod_data_import_ui("data_import_1"),
      second = mod_data_processing_ui("data_processing_1"),
      third = mod_variability_partition_ui("variability_partition_1")
      # second = mod_data_processing_ui(" mod_data_processing_ui")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "stable"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
