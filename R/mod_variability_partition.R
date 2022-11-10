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
    ),
    fluidRow(
      shiny::actionButton(ns("act5"),
                          HTML("A/B-level variability &rarr; A-level variability"), 
                          class = "btn-success"),
      plotly::plotlyOutput(ns("pop_plot3")),
      shiny::htmlOutput(ns('text3'))
    ),
    fluidRow(
      shiny::actionButton(ns("act6"),
                          HTML("A-level variability &rarr; system-level variability"), 
                          class = "btn-success"),
      plotly::plotlyOutput(ns("pop_plot4")),
      shiny::htmlOutput(ns('text4'))
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
        
        shinyjs::hide("act5")
        shinyjs::hide("pop_plot3")
        shinyjs::hide("text3")
        
        shinyjs::hide("act6")
        shinyjs::hide("pop_plot4")
        shinyjs::hide("text4")
      } else {
        shinyjs::show("act3")
        shinyjs::show("act4")
        shinyjs::show("act5")
        shinyjs::show("act6")
      }
    })
    
    observe({
      if ( !is.null(df$df_data())){
        if (length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)) == 1){
          shinyjs::hide("act3")
          shinyjs::hide("pop_plot")
          shinyjs::hide("text1")
          
          shinyjs::hide("act5")
          shinyjs::hide("pop_plot3")
          shinyjs::hide("text3")
          
          shinyjs::hide("act6")
          shinyjs::hide("pop_plot4")
          shinyjs::hide("text4")
        } else if (length(unique(df$long_df() %>% dplyr::pull(common) %>% unique)) == 1) {
          shinyjs::hide("act3")
          shinyjs::hide("pop_plot")
          shinyjs::hide("text1")
          
          shinyjs::hide("act4")
          shinyjs::hide("pop_plot2")
          shinyjs::hide("text2")
          
          shinyjs::hide("act5")
          shinyjs::hide("pop_plot3")
          shinyjs::hide("text3")
        }
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
          
        part <- stable::fct_lamy_mc_part(Y = df$df_data() %>% 
                                           dplyr::select(-grp, -year) %>% 
                                           dplyr::select(-which(colSums(.) == 0)),
                                         s = length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)),
                                         t = length(df$long_df() %>% dplyr::pull(year) %>% unique))
          
        HTML(paste0("Average variability at level A/B (",df$loc_col_name(),"-",df$spec_col_name(),"): ",
                    round(part$part$CV_SL, 3),"<br>",
              "Average variability at level B (",df$spec_col_name(),"): ", round(part$part$CV_SR, 3), "<br>",
              "Portfolio effect between A/B and B: ", round(1 - part$part$phi_S_LR, 3)))
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
          
          part <- stable::fct_lamy_mc_part(Y = df$df_data() %>% 
                                             dplyr::select(-grp, -year) %>% 
                                             dplyr::select(-which(colSums(.) == 0)),
                                           s = length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)),
                                           t = length(df$long_df() %>% dplyr::pull(year) %>% unique))
          
          HTML(paste0("Average variability at level B (",df$spec_col_name(),"): ", round(part$part$CV_SR, 3), "<br>",
                     "Average variability at the system level: ", round(part$part$CV_CR, 3), "<br>",
                     "Portfolio effect between B and system: ", round(1 - part$part$phi_SC_R, 3)))
        }
      })

    
    observeEvent(input$act5, {
      shinyjs::toggle("pop_plot3")
      shinyjs::toggle('text3')
      
    })
    
    output$pop_plot3 <- plotly::renderPlotly({
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
        
        a <- plotly::ggplotly(
          ggplot(df$long_df() %>% 
                   dplyr::group_by(year, grp) %>% 
                   dplyr::summarise(est = sum(est))) +
            geom_line(aes(y = est, x = year, color = grp),
                      alpha = 0.5) +
            guides(color = "none") +
            ggtitle("level A") +
            scale_x_continuous(expand = c(0.01, 0.01)) +
            scale_y_continuous(expand = c(0.01, 0.01)) +
            theme_stable())
        plotly::subplot(plotly::style(ab, showlegend = FALSE), 
                        plotly::style(a, showlegend = FALSE), nrows = 1)
      }
    })
    
    output$text3 <- 
      shiny::renderUI({
        if (!is.null(df$df_data())) {
          
          part <- stable::fct_lamy_mc_part(Y = df$df_data() %>% 
                                             dplyr::select(-grp, -year) %>% 
                                             dplyr::select(-which(colSums(.) == 0)),
                                           s = length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)),
                                           t = length(df$long_df() %>% dplyr::pull(year) %>% unique))
          
          HTML(paste0("Average variability at level A/B (",df$loc_col_name(),"-",df$spec_col_name(),"): ",round(part$part$CV_SL, 3),"<br>",
                     "Average variability at level A (",df$loc_col_name(),"): ", round(part$part$CV_CL, 3), "<br>",
                     "Portfolio effect between A/B and A: ", round(1 - part$part$phi_SC_L, 3)))
        }
      })
    
    observeEvent(input$act6, {
      shinyjs::toggle("pop_plot4")
      shinyjs::toggle('text4')
    })
    
    output$pop_plot4 <- plotly::renderPlotly({
      if (!is.null(df$df_data())) {
        # browser()
        a <- plotly::ggplotly(
          ggplot(df$long_df() %>% 
                   dplyr::group_by(year, grp) %>% 
                   dplyr::summarise(est = sum(est))) +
            geom_line(aes(y = est, x = year, color = grp),
                      alpha = 0.5) +
            guides(color = "none") +
            ggtitle("level A") +
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
        
        plotly::subplot(plotly::style(a, showlegend = FALSE), 
                        plotly::style(system_plt, showlegend = FALSE), nrows = 1)
      }
    })
    
    output$text4 <- 
      shiny::renderUI({
        if (!is.null(df$df_data())) {
          
          part <- stable::fct_lamy_mc_part(Y = df$df_data() %>% 
                                             dplyr::select(-grp, -year) %>% 
                                             dplyr::select(-which(colSums(.) == 0)),
                                           s = length(unique(df$long_df() %>% dplyr::pull(grp) %>% unique)),
                                           t = length(df$long_df() %>% dplyr::pull(year) %>% unique))
          
          HTML(paste0("Average variability at level A (",df$loc_col_name(),"): ", round(part$part$CV_CL, 3), "<br>",
                      "Average variability at the system level:", round(part$part$CV_CR, 3), "<br>",
                      "Portfolio effect between A and system:", round(1 - part$part$phi_C_LR, 3)))
        }
      })

 
  })
}
    
## To be copied in the UI
# mod_variability_partition_ui("variability_partition_1")
    
## To be copied in the server
# mod_variability_partition_server("variability_partition_1")
