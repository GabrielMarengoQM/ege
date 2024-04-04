# Load packages and their specific functions used
box::use(
  shiny[fluidRow, column, moduleServer, NS, tagList, h5, req, renderText, textOutput, observe,
        uiOutput, renderUI, downloadHandler, downloadButton],
  plotly[plotlyOutput, renderPlotly, plot_ly],
  fst[read.fst],
  shinyjs[show, hide, useShinyjs],
  htmltools[HTML],
  bslib[navset_card_underline, nav_panel, nav_spacer, card],
  reactable[reactableOutput, renderReactable, reactable]
)

# Import helper functions
box::use(
  app/logic/plots_logic
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    # IMPC PLOTS UI----
    fluidRow(
      fluidRow(
        textOutput(ns("plots_msg")),
        uiOutput(ns("mouse_plots"))
        )
    ),
    # MGI PLOTS UI----
    # fluidRow(
    #   column(
    #     width = 12,
    #     h5("Disease visualisations")
    #   )
    # ),
    # OMIM PLOTS UI----
    fluidRow(
      uiOutput(ns("disease_plots"))
    ),
    # DDG2P PLOTS UI----
    # fluidRow(
    #   column(
    #     width = 3,
    #     plotlyOutput(ns("ddg2p_allelic_requirement_plot"))
    #   ),
    #   column(
    #     width = 3,
    #     plotlyOutput(ns("ddg2p_organ_specificity_plot"))
    #   )
    # ),
    # SEQUENCING METRICS UI----
    fluidRow(
      uiOutput(ns("constraint_plots"))
    ),
    # fluidRow(
    #   column(
    #     width = 12,
    #     h5("Intolerance to variation (sequencing) visualisations")
    #   ),
    # ),
    # fluidRow(
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("gnomad_lof_upper_90_ci_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("mean_am_pathogenicity_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("shet_rgcme_mean_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("shet_post_mean_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("domino_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("scones_plot"))
    #   )
    # ),
    # CELL LINES METRICS UI----
    # fluidRow(
    #   column(
    #     width = 12,
    #     h5("Intolerance to variation (cell lines) visualisations")
    #   ),
    # ),
    # fluidRow(
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("mean_score_all_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("bf_lam_plot"))
    #   ),
    #   column(
    #     width = 2,
    #     plotlyOutput(ns("bf_mef_plot"))
    #   )
    # )
  )
  
}

#' @export
server <- function(id, filters_data, data_list) {
  moduleServer(id, function(input, output, session) {
    
    impc_data <- data_list[["impc_data"]]
    mgi_data <- data_list[["mgi_data"]]
    omim_data <- data_list[["omim_data"]]
    ddg2p_data <- data_list[["ddg2p_data"]]
    constraint_data <- data_list[["constraint_data"]]
    
    output$mouse_plot_title <- renderText({HTML("PLOTS TITLE")})
    output$save_genes_msg <- renderText({HTML("SAVE LIST BEFORE VEIWING")})
    
    # Show/hide message
    observe({
      if (length(filters_data$gene_lists()) == 0) {
        shinyjs::show("plots_msg")
        shinyjs::hide("mouse_plots")
        shinyjs::hide("disease_plots")
        shinyjs::hide("constraint_plots")
        
      } else {
        shinyjs::hide("plots_msg")
        shinyjs::show("mouse_plots")
        shinyjs::show("disease_plots")
        shinyjs::show("constraint_plots")
        
        }
    })
    
    output$plots_msg <- renderText({
      HTML("To view plots, first save a gene list using the 'Filter' tab ")
    })
    
    output$mouse_plots <- renderUI({
      tagList(
        navset_card_underline(
          nav_panel("IMPC",
                    fluidRow(
                      column(
                        width = 3,
                        card(plotlyOutput(session$ns("impc_viability_plot")),
                             full_screen = TRUE)
                      ),
                      column(
                        width = 3,
                        card(plotlyOutput(session$ns("impc_wol_plot")),
                             full_screen = TRUE)
                      ),
                      column(
                        width = 3,
                        card(plotlyOutput(session$ns("impc_zygosity_plot")),
                             full_screen = TRUE)
                      ),
                      column(
                        width = 3,
                        card(plotlyOutput(session$ns("ortholog_mapping_plot")),
                             full_screen = TRUE)
                      )
                    )
          ),
        nav_panel("MGI",
                  fluidRow(
                    column(
                      width = 3,
                      card(plotlyOutput(session$ns("mgi_viability_plot")),
                           full_screen = TRUE)
                    )
                  )
                ),
        nav_spacer(),
        nav_panel("Data table",
                  fluidRow(
                    reactableOutput(session$ns("impc_table"))
                    )
                  # ,
                  # fluidRow(
                  #   downloadButton(session$ns("download_impc_table"))
                  # )
                  ),
          header = "Mouse knockouts"
        
        )
      )
    })
    
    output$disease_plots <- renderUI({
      navset_card_underline(
        nav_panel("OMIM",
                  fluidRow(
                    column(
                      width =3,
                      card(plotlyOutput(session$ns("omim_disease_gene_lethal_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width =3,
                      card(plotlyOutput(session$ns("omim_earliest_lethality_category_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width =3,
                      card(plotlyOutput(session$ns("omim_moi_plot")),
                           full_screen = TRUE)
                    )
                  )
                  
                ),
        nav_panel("DDG2P",
                  fluidRow(
                    column(
                      width = 3,
                      card(plotlyOutput(session$ns("ddg2p_allelic_requirement_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 3,
                      card(plotlyOutput(session$ns("ddg2p_organ_specificity_plot")),
                           full_screen = TRUE)
                    )
                  )
                )
      )
    })
    
    output$constraint_plots <- renderUI({
      navset_card_underline(
        nav_panel("Cell line metrics",
                  fluidRow(
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("mean_score_all_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("bf_lam_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("bf_mef_plot")),
                           full_screen = TRUE)
                    )
                  )
                  
        ),
        nav_panel("Population sequencing metrics",
                  fluidRow(
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("gnomad_lof_upper_90_ci_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("mean_am_pathogenicity_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("shet_rgcme_mean_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("shet_post_mean_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("domino_plot")),
                           full_screen = TRUE)
                    ),
                    column(
                      width = 2,
                      card(plotlyOutput(session$ns("scones_plot")),
                           full_screen = TRUE)
                    )
                  ),
        )
      )
    })
    
    
    # IMPC PLOTS SERVER----
    output$impc_viability_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), impc_data, "impc_viability", "IMPC viability")
    })
    
    output$impc_wol_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), impc_data, "wol", "Window of Lethality")
    })
    
    output$impc_zygosity_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), impc_data, "impc_zygosity", "IMPC zygosity")
    })
    
    output$ortholog_mapping_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), impc_data, "ortholog_mapping", "Ortholog mapping")
    })
    
    output$impc_table <- renderReactable({
      reactable(impc_data)
    })
    
    # output$download_impc_table <- downloadHandler(
    #   filename = function() {
    #     "gene_list.txt"
    #   },
    #   content = function(file) {
    #     write.csv(impc_data, file, quote = FALSE, row.names = FALSE)
    #   }
    # )
    
    # MGI PLOTS SERVER----
    output$mgi_viability_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), mgi_data, "mgi_viability", "MGI viability")
    })
    
    # OMIM PLOTS SERVER----
    output$omim_disease_gene_lethal_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), omim_data, "disease_gene_lethal", "OMIM lethal phenotypes")
    })
    
    output$omim_earliest_lethality_category_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), omim_data, "earliest_lethality_category", "OMIM earliest lethality category")
    })
    
    output$omim_moi_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), omim_data, "moi", "OMIM Mode of inheritance")
    })
    
    # DDG2P PLOTS SERVER----
    output$ddg2p_allelic_requirement_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), ddg2p_data, "allelic_requirement", "DDG2P allelic requirement")
    })
    
    output$ddg2p_organ_specificity_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$barChart(filters_data$gene_lists(), ddg2p_data, "organ_specificity", "DDG2P organ specificity")
    })
    
    # SEQUENCING METRICS SERVER----
    output$gnomad_lof_upper_90_ci_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "gnomad_lof_upper_90_ci", filters_data$gene_lists(), NULL, 0.35, FALSE, "gnomAD LOEUF score")
    })
    
    output$mean_am_pathogenicity_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "mean_am_pathogenicity", filters_data$gene_lists(), NULL, 0.9, FALSE, "Alpha Missense mean score")
    })
    
    output$shet_rgcme_mean_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "shet_rgcme_mean", filters_data$gene_lists(), NULL, 0.075, FALSE, "Shet (Sun et al. 2023)")
    })
    
    output$shet_post_mean_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "shet_post_mean", filters_data$gene_lists(), NULL, 0.1, FALSE, "Shet (Zeng et al. 2023)")
    })
    
    output$domino_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "domino", filters_data$gene_lists(), NULL, NULL, FALSE, "DOMINO score")
    })
    
    output$scones_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "scones", filters_data$gene_lists(), NULL, c(0.25, 0.75), FALSE, "SCoNeS score")
    })
    
    # CELL LINES METRICS SERVER----
    output$mean_score_all_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "mean_score_all", filters_data$gene_lists(), NULL, c(-0.5, -1), FALSE, "DepMap mean gene effect score")
    })
    
    output$bf_lam_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "bf_lam", filters_data$gene_lists(), NULL, 5, FALSE, "Bayes Factor (laminin grown hPSCs)")
    })
    
    output$bf_mef_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "bf_mef", filters_data$gene_lists(), NULL, 5, FALSE, "Bayes Factor (MEF grown hPSCs)")
    })
  })
}
