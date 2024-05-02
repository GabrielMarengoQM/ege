# Load packages and their specific functions used
box::use(
  shiny[fluidRow, column, moduleServer, NS, tagList, h5, req, observe, actionButton,
        observeEvent, reactiveVal, withProgress, incProgress, numericInput, p, plotOutput, renderPlot,
        renderText, textOutput, htmlOutput, renderUI],
  shinyWidgets[pickerInput, updatePickerInput, materialSwitch],
  plotly[plotlyOutput, renderPlotly, plot_ly, ggplotly],
  fst[read.fst],
  htmltools[HTML],
  reactable[reactable, reactableOutput, renderReactable],
  bslib[navset_card_underline, nav_panel],
  promises[...],
  future[...]
)

# Import helper functions
box::use(
  app/logic/enrichment_logic
)

#' @export
ui <- function(id, filters_data) {
  ns <- NS(id)
  
  navset_card_underline(
    # GO ui ----
    nav_panel("Semantic similarity analysis (GO)",
              #htmlOutput(ns("go_plots_msg_1")),
              htmlOutput(ns("go_plots_msg_2")),
              htmlOutput(ns("go_error_msg")),
              plotlyOutput(ns("semantic_similarity_plot")),
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
    
    # Reactome ui ----
    nav_panel("Pathway enrichment analysis (Reactome)",
              #htmlOutput(ns("reactome_plots_msg_1")),
              htmlOutput(ns("reactome_plots_msg_2")),
              htmlOutput(ns("reactome_error_msg")),
              plotOutput(ns("reactome_plot")),
              reactableOutput(ns("reactome_table")),
              p("Options"),
              pickerInput(ns("reactome_gene_list_picker"), "select gene list",
                          choices = NULL),
              numericInput(ns("reactome_p_val"), "p value for enrichment analysis", 0.05),
              numericInput(ns("num_shown_pathways"), "Number of pathways displayed", 10),
              actionButton(ns("get_reactome_plot"), "Analyse")
              ),
    # OR ui ----
    nav_panel("Odds ratio analysis (Disease, Mouse knockout, Essentiality)",
              fluidRow(
                pickerInput(ns("OR_dataset_picker"), "select data set for Odds Ratios Analysis",
                            choices = c("OMIM Disease genes", "IMPC Lethal genes", "DepMap Essential genes")),
                pickerInput(ns("OR_gene_list_picker"), "select gene list",
                            choices = NULL),
                reactableOutput(ns("OR_table")),
                actionButton(ns("get_OR_table"), "Analyse")
              )
              )
  )
}

#' @export
server <- function(id, filters_data, data_list) {
  moduleServer(id, function(input, output, session) {
    
    pcg_data <- data_list[["pcg_data"]]
    impc_data <- data_list[["impc_data"]]
    omim_data <- data_list[["omim_data"]]
    constraint_data <- data_list[["constraint_data"]]
    
    
    # GO server ----
    output$go_plots_msg_1 <- renderUI({
      HTML("To perform enrichment analysis, first save a gene list using the 'Filter' tab. <br>
           Then select your gene list and parameters of choice then click 'Analyse'")
    })
    
    output$go_plots_msg_2 <- renderUI({
      HTML("To perform enrichment analysis, select your gene list and parameters of choice then click 'Analyse'")
    })
    
    output$reactome_plots_msg_1 <- renderUI({
      HTML("To perform enrichment analysis, first save a gene list using the 'Filter' tab.<br>
           Then select your gene list and parameters of choice then click 'Analyse'")
    })
    
    output$reactome_plots_msg_2 <- renderUI({
      HTML("To perform enrichment analysis, select your gene list and parameters of choice then click 'Analyse'")
    })
    
    
    output$reactome_error_msg <- renderUI({
      HTML("Error in enrichment analysis: No enriched terms found.<br>Please try again with a different gene list")
    })
    
    output$go_error_msg <- renderUI({
      HTML("Error in enrichment analysis: No enriched terms found.<br>Please try again with a different gene list")
    })
    
    # Show/hide message
    observe({
      # If add_gene_list_name is empty, disable the add_gene_list button
      if (length(filters_data$gene_lists()) == 0) {
        #shinyjs::show("go_plots_msg_1")
        #shinyjs::hide("go_plots_msg_2")
        
        #shinyjs::show("reactome_plots_msg_1")
        #shinyjs::hide("reactome_plots_msg_2")
        
        # shinyjs::hide("semantic_similarity_plot")
        # shinyjs::hide("reactome_plot")
        shinyjs::disable("get_reactome_plot")        
        shinyjs::disable("get_go_plot")

      } else {
        #shinyjs::hide("go_plots_msg_1")
        #shinyjs::show("go_plots_msg_2")
        
        #shinyjs::hide("reactome_plots_msg_1")
        #shinyjs::show("reactome_plots_msg_2")
        
        # shinyjs::show("semantic_similarity_plot")
        # shinyjs::show("reactome_plot")
        shinyjs::enable("get_reactome_plot")        
        shinyjs::enable("get_go_plot")
      }
      
      if (is.null(enriched_terms_val())) {
        shinyjs::show("go_plots_msg_2")
        shinyjs::hide("semantic_similarity_plot")
        
        
      } else {
        shinyjs::hide("go_plots_msg_2")
        shinyjs::show("semantic_similarity_plot")
        
      }
      
      if (is.null(reactome_enrichment_output())) {
        shinyjs::show("reactome_plots_msg_2")
        shinyjs::hide("reactome_plot")
        
      } else {
        shinyjs::hide("reactome_plots_msg_2")
        shinyjs::show("reactome_plot")
        
      }
    })
    
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
    go_error_msg_val <- reactiveVal(NULL)
    observeEvent(input$get_go_plot, {
      # Get enriched terms
      withProgress(message="Calculating enrichment, this may take a while...", value=0, {
        gene_list <- filters_data$gene_lists()[[input$semantic_similarity_gene_list_picker]]
        background <- data_list[["pcg_data"]]
        background <- background$gene_symbol
        ontology <- input$semantic_similarity_ontology_picker

        p_val <- input$p_val_input
        q_val <- input$q_val_input

        tryCatch({
          enriched_terms <- enrichment_logic$getEnrichedGoTerms(gene_list, background, ontology, p_val, q_val)
          # Get plot
          withProgress(message="Calculating similarity, this may take a while...", value=0, {
            slice_enriched_terms <- input$slice_enriched_terms
            similarity_score <- input$similarity_score

            plot <- enrichment_logic$generateGoSemanticSimilarityPlot(enriched_terms, ontology, slice_enriched_terms, similarity_score)
            go_error_msg_val(NULL)
          })
        }, error = function(e) {
          go_error_msg_val(TRUE)
          return(paste("Error:", e$message))
        })
      })
      req(is.null(go_error_msg_val()))

      enriched_terms_val(list(enriched_terms =  enriched_terms, plot = plot))
    })
    
    # testing async ----
   
    
    
    
    observe({
      if (is.null(go_error_msg_val())) {
        shinyjs::hide("go_error_msg")
        shinyjs::show("semantic_similarity_plot")
        shinyjs::show("enriched_terms_table")
      } else {
        shinyjs::show("go_error_msg")
        shinyjs::hide("semantic_similarity_plot")
        shinyjs::hide("enriched_terms_table")
        enriched_terms_val(NULL)
      }
    })
    
    # Generate plot
    output$semantic_similarity_plot <- renderPlotly({
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
    
    # Reactome server ----
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
    reactome_error_msg_val <- reactiveVal(NULL)
    observeEvent(input$get_reactome_plot, {
      pval <- input$reactome_p_val
      num_shown_pathways <- input$num_shown_pathways
      gene_list <- filters_data$gene_lists()[[input$reactome_gene_list_picker]]
      background <- data_list[["pcg_data"]]
      withProgress(message="Calculating enrichment, this may take a while...", value=0, {
        tryCatch({
          enrichment_result <- enrichment_logic$reactomeEnrichment(gene_list, background, pval, num_shown_pathways)
          reactome_error_msg_val(NULL)
        }, error = function(e) {
          reactome_error_msg_val(TRUE)
          return(paste("Error:", e$message))
        })
      })
      req(is.null(reactome_error_msg_val()))
      reactome_enrichment_output(enrichment_result)
    })
    
    observe({
      if (is.null(reactome_error_msg_val())) {
        shinyjs::hide("reactome_error_msg")
        shinyjs::show("reactome_plot")
        shinyjs::show("reactome_table")
      } else {
        shinyjs::show("reactome_error_msg")
        shinyjs::hide("reactome_plot")
        shinyjs::hide("reactome_table")
        reactome_enrichment_output(NULL)
      }
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
    
    # OR server ----
    observe({
      updatePickerInput(
        session = session,
        "OR_gene_list_picker",
        selected = NULL,
        choices = names(filters_data$gene_lists())
      )
    })
    
    OR_table_val <- shiny::reactiveVal(NULL)
    observeEvent(input$get_OR_table, {
      compare_list_option <- input$OR_dataset_picker
      my_list <-filters_data$gene_lists()[[input$OR_gene_list_picker]]

      # Selecting the dataset based on compare_list_option
      if (compare_list_option == "OMIM Disease genes") {
        dataset <- data_list[["omim_data"]]
      } else if (compare_list_option == "IMPC Lethal genes") {
        dataset <- data_list[["impc_data"]]
      } else if (compare_list_option == "DepMap Essential genes") {
        dataset <- data_list[["constraint_data"]]
      }
      
      tbl <- enrichment_logic$oddsRatioPlot(compare_list_option, my_list, pcg_data, dataset)
      OR_table_val(tbl)
    })
    
    output$OR_table <- renderReactable({
      req(!is.null(OR_table_val()))
      table <- OR_table_val()
      reactable(table)
    })
    
  })
}
