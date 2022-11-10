#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }
### ADD THIS HERE ###
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
###To change text and background color of the `Select` box ###
                    .dataTables_length select {
                           color: #0E334A;
                           background-color: #0E334A
                           }
###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #0E334A
                           }
                    thead {
                    color: #ffffff;
                    }
                     tbody {
                    color: #a3a3a3;
                    }
                   "
    )),
  shinyjs::useShinyjs(), 
  fluidRow(
    column(4, 
           fileInput(ns("file1"), h4("1. Choose CSV file:"),
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           )
        ),
    column(8,
           DT::DTOutput(ns('dt'))
        ),
    ),
  br(),
  shiny::h4("2. Enter column names:"),
  fluidRow(
    shinydashboard::box(width = 12,
      splitLayout(cellWidths = c("50%","50%"),
          shiny::textInput(
            ns("loc_col"),
            HTML("dimensionA:<br>"),
            "grp"
          ),
          shiny::textInput(
            ns("spec_col"),
            HTML("dimensionB:<br>"),
            "common"
        )
      )
      )
    ),
  br(),
  fluidRow(
    shinydashboard::box(width = 12,
      splitLayout(cellWidths = c("50%","50%"),
        shiny::textInput(
          ns("time_col"),
          HTML("Time:<br>"),
          "year"
        ),
        shiny::textInput(
          ns("prop_col"),
          HTML("System property:<br>"),
          "est"
         )
        )
       )
      ),
  br(),
  fluidRow(
    shinydashboard::box(width = 12,
                        splitLayout(cellWidths = c("50%","50%"),
                                    shiny::textInput(
                                      ns("lat_col"),
                                      HTML("Latitude:<br>"),
                                      "latitude"
                                    ),
                                    shiny::textInput(
                                      ns("long_col"),
                                      HTML("Longitude:<br>"),
                                      "longitude"
                                    )
                        )
    )
  ),
  br(),
  fluidRow(
    shinydashboard::box(width = 6,
                        splitLayout(cellWidths = c("50%","50%"),
    shiny::actionButton(ns("act1"),
                        "Process data", 
                        class = "btn-success"),
    shinyWidgets::progressBar(
      ns("pb2"),
      value = 0,
      total = 100,
      title = ""
                    )
                  )
                )
              )
            )
          }
    
#' data_import Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    df <- reactive({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      read.csv(file$datapath)
    })

    
    output$dt <- DT::renderDT({

      # render only if there is data available
      req(df())

      # reactives are only callable inside an reactive context like render
      DT::datatable(df() %>%
                      dplyr::mutate_if(is.numeric, round, 3),
                    options = list(pageLength = 5, lengthChange = FALSE))
    })
    
    values <- reactiveValues(df_data = NULL, 
                             long_df = NULL)
    observeEvent(input$act1, {

      if (any(input$loc_col == ""|
              input$spec_col == ""|
              input$time_col == ""|
              input$prop_col == "")){
        shinyjs::alert("Missing variable name.")
      } else {
        values$df_data <- 
          df() %>%
          dplyr::select(grp =
                          input$loc_col,
                        common =
                          input$spec_col,
                        year =
                          input$time_col,
                        est =
                          input$prop_col) %>%
          {. ->> long_df} %>% 
          tidyr::spread(common, est, fill = 0) %>%
          dplyr::arrange(grp, year)
        
        values$long_df <- long_df
        
      }
    })
    
    observeEvent(input$act1, {
      for (i in 1:100) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "pb2",
          value = i, total = 100
        )
      }
    })
    
    return(list(df_data = reactive( values$df_data ),
                long_df = reactive( values$long_df ),
                prop_col_name = reactive( input$prop_col ),
                time_col_name = reactive( input$time_col ),
                spec_col_name = reactive( input$spec_col ),
                loc_col_name = reactive( input$loc_col )))
    
  })
  
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_1")
    
## To be copied in the server
# mod_data_import_server("data_import_1")
