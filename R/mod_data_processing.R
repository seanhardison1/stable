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
    fluidRow(
      shiny::actionButton(ns("act2"),
                          "Figure 1 (aggregated time series)", 
                          class = "btn-success")
    ),
    br(),
    plotOutput(ns("plot")),
    br()
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
        shinyjs::hide("act2")
      } else {
        shinyjs::show("act2")
      }
    })
    
    observeEvent(input$act2, {
        shinyjs::toggle("plot")
    })
    
    
    output$plot <- renderPlot({
      if (!is.null(df$df_data())) {
        mp_df <- df$long_df() %>% 
          dplyr::group_by(year, common) %>% 
          dplyr::summarise(est = sum(est)) %>% 
          dplyr::group_by(year) %>% 
          dplyr::mutate(total = sum(est),
                        perc = est/total) 
        
        tg_df <- df$long_df() %>% 
          dplyr::group_by(year, grp) %>% 
          dplyr::summarise(est = sum(est)) %>% 
          dplyr::group_by(year) %>% 
          dplyr::mutate(total = sum(est),
                        perc = est/total) 
        mp_plt <-
          ggplot(mp_df) +
          geom_area(aes(y = est, x = year,
                                          group = common,
                                          fill = common),
                    show.legend = F) +
          ggsci::scale_fill_igv() +
          labs(y = df$prop_col_name(),
               x = df$time_col_name()) +
          guides(fill = guide_legend(nrow = 3)) +
          scale_x_continuous(expand = c(0.01, 0.01)) +
          scale_y_continuous(expand = c(0.01, 0.01)) +
          theme_stable() +
          theme(legend.title = element_blank())

        tg_plt <-
          ggplot(tg_df) +
          geom_area(aes(y = est, x = year,
                                          group = grp,
                                          fill = grp),
                    show.legend = F) +
          ggsci::scale_fill_material() +
          labs(y = df$prop_col_name(),
               x = df$time_col_name(),
               fill = df$loc_col_name()) +
          scale_x_continuous(expand = c(0.01, 0.01)) +
          scale_y_continuous(expand = c(0.01, 0.01)) +
          theme_stable() +
          theme(axis.title.y = element_blank(),
                axis.text.y = element_blank())

        mp_plt_perc <-
          ggplot(mp_df) +
          geom_area(aes(y = perc, x = year,
                        group = common,
                        fill = common)) +
          ggsci::scale_fill_igv() +
          labs(y = df$prop_col_name(),
               x = df$time_col_name()) +
          guides(fill = guide_legend(nrow = 3)) +
          scale_x_continuous(expand = c(0.01, 0.01)) +
          scale_y_continuous(expand = c(0.01, 0.01)) +
          theme_stable() +
          labs(y = "Proportion") +
          theme(legend.title = element_blank())

        tg_plt_perc <-
          ggplot(tg_df) +
          geom_area(aes(y = perc, x = year,
                        group = grp,
                        fill = grp)) +
          ggsci::scale_fill_material() +
          labs(y = df$prop_col_name(),
               x = df$time_col_name(),
               fill = df$loc_col_name()) +
          labs(y = "Proportion") +
          scale_x_continuous(expand = c(0.01, 0.01)) +
          scale_y_continuous(expand = c(0.01, 0.01)) +
          theme_stable() +
          theme(axis.title.y = element_blank(),
                axis.text.y = element_blank())

        # spec_corr_plt <-
        #   GGally::ggcorr(df$df_data()[,3:ncol(df$df_data())],
        #                  label = TRUE, hjust = 1, label_size = 2, size = 2) +
        #   guides(color = "none", alpha = "none", fill = "none") +
        #   theme_stable()
        #
        # grp_corr_plt <-
        #   GGally::ggcorr(grp_wide[,3:ncol(grp_wide)],
        #                  label = TRUE, hjust = 1, label_size = 2, size = 2) +
        #   guides(color = "none", alpha = "none", fill = "none")+
        #   theme_stable()
        # browser()
        
        mp_plt + tg_plt +
          mp_plt_perc + tg_plt_perc +
          patchwork::plot_layout(nrow = 2) &
          plot_annotation(theme =
                            theme(plot.background = element_rect(fill ="black")))
      } 
    })
   
  })
}
    
## To be copied in the UI
# mod_data_processing_ui("data_processing_1")
    
## To be copied in the server
# mod_data_processing_server("data_processing_1")
