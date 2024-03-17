# Load packages and their specific functions used
box::use(
  shiny[fluidRow, column, moduleServer, NS, tagList, h5, req],
  plotly[plotlyOutput, renderPlotly, plot_ly],
  fst[read.fst]
)

# Import helper functions
box::use(
  app/logic/plots_logic
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        offset = 4,
        h5("Mouse models visualisations")
      )
    ),
    # IMPC PLOTS UI----
    fluidRow(
      column(
        width = 3,
        plotlyOutput(ns("impc_viability_plot"))
      ),
      column(
        width = 3,
        plotlyOutput(ns("impc_wol_plot"))
      ),
      column(
        width = 3,
        plotlyOutput(ns("impc_zygosity_plot"))
      ),
      column(
        width = 3,
        plotlyOutput(ns("ortholog_mapping_plot"))
      )
    ),
    # MGI PLOTS UI----
    fluidRow(
      column(
        width = 3,
        plotlyOutput(ns("mgi_viability_plot"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        h5("Disease visualisations")
      ),
    ),
    # OMIM PLOTS UI----
    fluidRow(
      column(
        width =3,
        plotlyOutput(ns("omim_disease_gene_lethal_plot"))
      ),
      column(
        width =3,
        plotlyOutput(ns("omim_earliest_lethality_category_plot"))
      ),
      column(
        width =3,
        plotlyOutput(ns("omim_moi_plot"))
      )
    ),
    # DDG2P PLOTS UI----
    fluidRow(
      column(
        width = 3,
        plotlyOutput(ns("ddg2p_allelic_requirement_plot"))
      ),
      column(
        width = 3,
        plotlyOutput(ns("ddg2p_organ_specificity_plot"))
      )
    ),
    # SEQUENCING METRICS UI----
    fluidRow(
      column(
        width = 12,
        h5("Intolerance to variation (sequencing) visualisations")
      ),
    ),
    fluidRow(
      column(
        width = 2,
        plotlyOutput(ns("gnomad_lof_upper_90_ci_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("mean_am_pathogenicity_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("shet_rgcme_mean_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("shet_post_mean_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("domino_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("scones_plot"))
      )
    ),
    # CELL LINES METRICS UI----
    fluidRow(
      column(
        width = 12,
        h5("Intolerance to variation (cell lines) visualisations")
      ),
    ),
    fluidRow(
      column(
        width = 2,
        plotlyOutput(ns("mean_score_all_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("bf_lam_plot"))
      ),
      column(
        width = 2,
        plotlyOutput(ns("bf_mef_plot"))
      )
    )
  )
  
}

#' @export
server <- function(id, filters_data, data_list) {
  moduleServer(id, function(input, output, session) {
    # constraint_data <- read.fst("./data/constraint_metrics.fst")
    # impc_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/test_impc.fst")
    # mgi_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.viability.mgi.fst")
    # omim_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/omim_data.fst")
    # ddg2p_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/ddg2p.fst")
    impc_data <- data_list[["impc_data"]]
    mgi_data <- data_list[["mgi_data"]]
    omim_data <- data_list[["omim_data"]]
    ddg2p_data <- data_list[["ddg2p_data"]]
    constraint_data <- data_list[["constraint_data"]]
    
    
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
      plots_logic$renderViolinPlot(constraint_data, "gnomad_lof_upper_90_ci", filters_data$gene_lists(), NULL, NULL, FALSE, "gnomAD LOEUF score")
    })
    
    output$mean_am_pathogenicity_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "mean_am_pathogenicity", filters_data$gene_lists(), NULL, NULL, FALSE, "Alpha Missense mean score")
    })
    
    output$shet_rgcme_mean_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "shet_rgcme_mean", filters_data$gene_lists(), NULL, NULL, FALSE, "Shet (Sun et al. 2023)")
    })
    
    output$shet_post_mean_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "shet_post_mean", filters_data$gene_lists(), NULL, NULL, FALSE, "Shet (Zeng et al. 2023)")
    })
    
    output$domino_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "domino", filters_data$gene_lists(), NULL, NULL, FALSE, "DOMINO score")
    })
    
    output$scones_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "scones", filters_data$gene_lists(), NULL, NULL, FALSE, "SCoNeS score")
    })
    
    # CELL LINES METRICS SERVER----
    output$mean_score_all_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "mean_score_all", filters_data$gene_lists(), NULL, NULL, FALSE, "DepMap mean gene effect score")
    })
    
    output$bf_lam_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "bf_lam", filters_data$gene_lists(), NULL, NULL, FALSE, "Bayes Factor (laminin grown hPSCs)")
    })
    
    output$bf_mef_plot <- renderPlotly({
      req(length(filters_data$gene_lists()) > 0)
      plots_logic$renderViolinPlot(constraint_data, "bf_mef", filters_data$gene_lists(), NULL, NULL, FALSE, "Bayes Factor (MEF grown hPSCs)")
    })
  })
}
