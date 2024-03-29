# Load packages and their specific functions used
box::use(
  shiny[fluidRow, column, moduleServer, NS, tagList, h5, req, observe, actionButton,
        observeEvent, reactiveVal, withProgress, incProgress, numericInput, p, plotOutput, renderPlot],
  shinyWidgets[pickerInput, updatePickerInput, materialSwitch],
  plotly[plotlyOutput, renderPlotly, plot_ly, ggplotly],
  fst[read.fst],
  htmltools[HTML],
  reactable[reactable, reactableOutput, renderReactable],
  bslib[navset_card_underline, nav_panel]
)

# Import helper functions
box::use(
  app/logic/enrichment_logic
)

#' @export
ui <- function(id, filters_data) {
  ns <- NS(id)
  
  navset_card_underline(
    # Panel with plot ----
    nav_panel("Semantic similarity analysis (GO)",
              plotlyOutput(ns("semnatic_similarity_plot")),
              reactableOutput(ns("enriched_terms_table")),
              p("Options"),
              fluidRow(
                column(
                  width = 6,
                  pickerInput(ns("semantic_similarity_gene_list_picker"), "select gene list",
                              choices = NULL),
                  pickerInput(ns("semantic_similarity_ontology_picker"), "select GO ontology",
                              choices = c("BP", "MF", "CC")),
                  numericInput(ns("slice_enriched_terms"), "Top n% enriched terms to retain", 25)
                ),
                column(
                  width = 6,
                  numericInput(ns("p_val_input"), "p value cutoff", 0.01),
                  numericInput(ns("q_val_input"), "q value cutoff", 0.05),
                  numericInput(ns("similarity_score"), HTML("Similarity score:<br>
                                               0.9 (large), 0.7 (medium), 0.5 (small), 0.4 (tiny)"), 0.7)
                )
              ),
              materialSwitch(ns("show_legend"), "Show legend"),
              actionButton(ns("get_go_plot"), "Analyse")
              ),
    
    # Panel with summary ----
    nav_panel("Pathway enrichment analysis (Reactome)",
              plotOutput(ns("reactome_plot")),
              reactableOutput(ns("reactome_table")),
              p("Options"),
              pickerInput(ns("reactome_gene_list_picker"), "select gene list",
                          choices = NULL),
              numericInput(ns("reactome_p_val"), "p value for enrichment analysis", 0.05),
              numericInput(ns("num_shown_pathways"), "Number of pathways displayed", 10),
              actionButton(ns("get_reactome_plot"), "Analyse")
              
              )
  )
}

#' @export
server <- function(id, filters_data, data_list) {
  moduleServer(id, function(input, output, session) {
    
    # GO ----
    # Update gene list selector
    observe({
      updatePickerInput(
        session = session,
        "semantic_similarity_gene_list_picker",
        selected = NULL,
        choices = names(filters_data$gene_lists())
      )
    })
    
    # Get enriched terms
    enriched_terms_val <- reactiveVal(NULL)
    observeEvent(input$get_go_plot, {
      # Get enriched terms
      withProgress(message="Calculating enrichment, this may take a while...", value=0, { 
        gene_list <- filters_data$gene_lists()[[input$semantic_similarity_gene_list_picker]]
        background <- data_list[["pcg_data"]]
        background <- background$gene_symbol
        ontology <- input$semantic_similarity_ontology_picker
        
        p_val <- input$p_val_input
        q_val <- input$q_val_input
       
        enriched_terms <- enrichment_logic$getEnrichedGoTerms(gene_list, background, ontology, p_val, q_val)
      })
      # Get plot
      withProgress(message="Calculating similarity, this may take a while...", value=0, {
        slice_enriched_terms <- input$slice_enriched_terms
        similarity_score <- input$similarity_score
        
        plot <- enrichment_logic$generateGoSemanticSimilarityPlot(enriched_terms, ontology, slice_enriched_terms, similarity_score)
      })
      
      enriched_terms_val(list(enriched_terms =  enriched_terms, plot = plot))
    })
    
    # Generate plot
    output$semnatic_similarity_plot <- renderPlotly({
      req(!is.null(enriched_terms_val()))
      data <- enriched_terms_val()
      plot <- data[["plot"]]
      show_legend <- input$show_legend
      
      plot <- enrichment_logic$showLegendGO(plot, show_legend)
      ggplotly(plot)
    })
    
    output$enriched_terms_table <- renderReactable({
      req(!is.null(enriched_terms_val()))
      data <- enriched_terms_val()[["enriched_terms"]]
      table <- enrichment_logic$getEnirchedTermsTable(data)
      reactable(
        table
      )
    })
    
    # Reactome ----
    # Update gene list selector
    observe({
      updatePickerInput(
        session = session,
        "reactome_gene_list_picker",
        selected = NULL,
        choices = names(filters_data$gene_lists())
      )
    })
    
    reactome_enrichment_output <- reactiveVal(NULL)
    
    observeEvent(input$get_reactome_plot, {
      pval <- input$reactome_p_val
      num_shown_pathways <- input$num_shown_pathways
      gene_list <- filters_data$gene_lists()[[input$reactome_gene_list_picker]]
      background <- data_list[["pcg_data"]]
      withProgress(message="Calculating enrichment, this may take a while...", value=0, {
        enrichment_result <- enrichment_logic$reactomeEnrichment(gene_list, background, pval, num_shown_pathways)
      })
      reactome_enrichment_output(enrichment_result)
    })
    
    output$reactome_plot <- renderPlot({
      req(!is.null(reactome_enrichment_output()))
      reactome_enrichment_output()[["plot"]]
    })
    
    output$reactome_table <- renderReactable({
      req(!is.null(reactome_enrichment_output()))
      table <- reactome_enrichment_output()[["enriched_pathway_table"]]
      reactable(table)
    })
    
  })
}
