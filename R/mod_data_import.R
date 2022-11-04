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
           fileInput(ns("file1"), "Choose CSV File",
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
  fluidRow(
  shinydashboard::box(width = 10,
     shiny::splitLayout(
          shiny::textInput(
            ns("nspecies"),
            "Number of species:",
            ""
          ),
          shiny::textInput(
            ns("spec_col"),
            "Name of species column:",
            ""
          )
        )
    )
  ),
  br(),
  fluidRow(
    shinydashboard::box(width = 10,
        shiny::splitLayout(
            shiny::textInput(
             ns("npatches"),
             "Number of patches:",
             ""
           ),
           shiny::splitLayout(
             shiny::textInput(
               ns("patch_col"),
               "Name of patches column:",
               ""
             )
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
        nspecies = reactive({ input$nspecies }),
        spec_col_name = reactive({ input$spec_col }),
        npatches = reactive({ input$npatches }),
        patch_col_name = reactive({ input$patch_col })
      )
    )

  })
  
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_1")
    
## To be copied in the server
# mod_data_import_server("data_import_1")
