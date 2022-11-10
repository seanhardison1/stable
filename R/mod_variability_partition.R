#' variability_partition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_variability_partition_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    shiny::actionButton(ns("act3"),
                        HTML("A/B-level variability &rarr; B-level variability"), 
                        class = "btn-success"),
    plotly::plotlyOutput(ns("pop_plot")),
    shiny::htmlOutput(ns('text1'))
    ),
    fluidRow(
    shiny::actionButton(ns("act4"),
                        HTML("B-level variability &rarr; system-level variability"), 
                        class = "btn-success"),
    plotly::plotlyOutput(ns("pop_plot2")),
    shiny::htmlOutput(ns('text2'))
    )
  )
}
    
#' variability_partition Server Functions
#'
#' @noRd 
mod_variability_partition_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if (is.null(df$df_data())) {
        shinyjs::hide("act3")
        shinyjs::hide("pop_plot")
        shinyjs::hide("text1")
        
        shinyjs::hide("act4")
        shinyjs::hide("pop_plot2")
        shinyjs::hide("text2")
      } else {
        shinyjs::show("act3")
        shinyjs::show("act4")
      }
    })
    
    observeEvent(input$act3, {
      shinyjs::toggle("pop_plot")
      shinyjs::toggle('text1')
      
    })
    
    output$pop_plot <- plotly::renderPlotly({
      if (!is.null(df$df_data())) {
        # browser()
        ab <-
          plotly::ggplotly(
            ggplot(df$long_df() %>% 
                     dplyr::mutate(population = paste(grp, common)) ) +
              geom_line(aes(y = est, x = year, group = population,
                            color = common),
                        alpha = 0.5) +
              ggtitle("level A/B") +
              scale_x_continuous(expand = c(0.01, 0.01)) +
              scale_y_continuous(expand = c(0.01, 0.01)) +
              theme_stable() )
        
        b <- plotly::ggplotly(
          ggplot(df$long_df() %>% 
                   dplyr::group_by(year, common) %>% 
                   dplyr::summarise(est = sum(est))) +
            geom_line(aes(y = est, x = year, color = common),
                      alpha = 0.5) +
            guides(color = "none") +
            ggtitle("level B") +
            scale_x_continuous(expand = c(0.01, 0.01)) +
            scale_y_continuous(expand = c(0.01, 0.01)) +
            theme_stable())
        plotly::subplot(plotly::style(ab, showlegend = FALSE), 
                        plotly::style(b, showlegend = FALSE), nrows = 1)
      }
    })
    
    output$text1 <- 
      shiny::renderUI({
        if (!is.null(df$df_data())) {
          
        part <- stable::fct_lamy_mc_part(Y = df$df_data(),
                                         s = length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)),
                                         t = length(df$long_df() %>% dplyr::pull(year) %>% unique))
          
        HTML(paste("Average variability at level A/B:",round(part$part$CV_SL, 3),"<br>",
              "Average variability at level B:", round(part$part$CV_SR, 3), "<br>",
              "Portfolio effect between A/B and B:", round(1 - part$part$phi_S_LR, 3)))
        }
      })
    
    
    observeEvent(input$act4, {
      shinyjs::toggle("pop_plot2")
      shinyjs::toggle('text2')
    })
    
    output$pop_plot2 <- plotly::renderPlotly({
      if (!is.null(df$df_data())) {
        # browser()
          b <- plotly::ggplotly(
                  ggplot(df$long_df() %>% 
                           dplyr::group_by(year, common) %>% 
                           dplyr::summarise(est = sum(est))) +
                    geom_line(aes(y = est, x = year, color = common),
                              alpha = 0.5) +
                    guides(color = "none") +
                    scale_x_continuous(expand = c(0.01, 0.01)) +
                    scale_y_continuous(expand = c(0.01, 0.01)) +
                    theme_stable())
          system_plt <- plotly::ggplotly(
            ggplot(df$long_df() %>% 
                     dplyr::group_by(year) %>% 
                     dplyr::summarise(est = sum(est))) +
              geom_line(aes(y = est, x = year),
                        alpha = 0.5, color = "white") +
              guides(color = "none") +
              scale_x_continuous(expand = c(0.01, 0.01)) +
              scale_y_continuous(expand = c(0.01, 0.01)) +
              theme_stable())
          plotly::subplot(plotly::style(b, showlegend = FALSE),system_plt,
                          nrows = 1)
      }
    })
    
    output$text2 <- 
      shiny::renderUI({
        if (!is.null(df$df_data())) {
          
          part <- stable::fct_lamy_mc_part(Y = df$df_data(),
                                           s = length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)),
                                           t = length(df$long_df() %>% dplyr::pull(year) %>% unique))
          
          HTML(paste("Average variability at level B:", round(part$part$CV_SR, 3), "<br>",
                     "Average variability at the system level:", round(part$part$CV_CR, 3), "<br>",
                     "Portfolio effect between B and system:", round(1 - part$part$phi_SC_R, 3)))
        }
      })

    

 
  })
}
    
## To be copied in the UI
# mod_variability_partition_ui("variability_partition_1")
    
## To be copied in the server
# mod_variability_partition_server("variability_partition_1")
