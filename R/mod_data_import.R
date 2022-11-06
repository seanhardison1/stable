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
    
  fluidRow(
    column(4, 
           fileInput(ns("file1"), "1. Choose CSV file:",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           )
        ),
    column(8, 
           DT::DTOutput(ns('dt'))
        )
    ),
  br(),
  shiny::h6("2. Enter column names:"),
  fluidRow(
    shinydashboard::box(width = 12,
      splitLayout(cellWidths = c("50%","50%"),
          shiny::textInput(
            ns("loc_col"),
            HTML("Locations:<br>"),
            ""
          ),
          shiny::textInput(
            ns("spec_col"),
            HTML("Species:<br>"),
            ""
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
          ""
        ),
        shiny::textInput(
          ns("prop_col"),
          HTML("Ecosystem property:<br>"),
          ""
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
    
    return(
      list(
        spec_col_name = reactive({ input$spec_col }),
        patch_col_name = reactive({ input$patch_col }),
        loc_col_name = reactive({ input$loc_col }),
        eco_col_name = reactive({ input$prop_col })
      )
    )

  })
  
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_1")
    
## To be copied in the server
# mod_data_import_server("data_import_1")
