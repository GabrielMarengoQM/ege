box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, renderText, textOutput, tagList,
        fluidRow, column, actionButton, observe, observeEvent, numericInput, sliderInput, textAreaInput,
        fileInput, hr, downloadHandler, downloadButton, textInput, req, reactive, tabsetPanel, tabPanel,
        icon, h5, p, reactiveVal, reactiveValues],
  fst[read.fst],
  reactable[reactable, reactableOutput, renderReactable],
  shinyWidgets[pickerInput, progressBar, updateProgressBar, pickerOptions, materialSwitch, awesomeCheckbox,
               multiInput, updateMultiInput, dropdownButton, tooltipOptions, useShinydashboard],
  dplyr[...],
  stats[na.omit, setNames],
  bslib[page_navbar, nav_panel, card, card_header, card_body, tooltip],
  bsicons[bs_icon],
  shinydashboard[box],
  htmltools[br, includeScript, HTML],
  stringr[str_detect],
  tidyr[separate_rows],
  utils[read.delim, write.csv],
  shinyjs[reset, useShinyjs, disable, show, hide],
  shinydashboard[...]
)

box::use(
  app/logic/filters_logic
)

#' @export
ui <- function(id, data_list) {
  ns <- NS(id)
  
  # Data ----
  pcg_data <- data_list[["pcg_data"]]
  impc_data <- data_list[["impc_data"]]
  mgi_data <- data_list[["mgi_data"]]
  omim_data <- data_list[["omim_data"]]
  ddg2p_data <- data_list[["ddg2p_data"]]
  constraint_data <- data_list[["constraint_data"]]
  
  tagList(
    useShinyjs(),
    useShinydashboard(),
    #includeScript(path = "app.js"),
    # Filters UI ----
    fluidRow(
      column(
        width = 3,
        div(class = "filterHeader", # Add a row container
            div(class = "col-auto h5-column", # Define a column for the h5 element
                h5("Filters")
            ),
            div(class = "col-auto actionButton-column", # Define a column for the bs_icon
                actionButton(ns("reset_filters"), "Reset")
            )
        ),
        p("Click + to expand filters"),
        # MOUSE UI ----
        box(title = tooltip(
          trigger = list(
            "Mouse models"
          ),
          HTML("Knockout mouse data from the International Mouse Phenotyping Consortium (IMPC) and the
               Mouse Genome Informatics (MGI) databases")
          ),
        collapsible = TRUE, collapsed = TRUE, width = "100%",
        awesomeCheckbox(
          ns("impc_annotations_checkbox"),
          label = "Enable IMPC filters", 
          value = FALSE
        ),
        # F1
        fluidRow(
          column(
            width=8,
            pickerInput(
              ns("impc_viability_filter"),
              label = "Viability after knockout",
              choices = sort(na.omit(unique(impc_data$impc_viability))),
              selected = sort(na.omit(unique(impc_data$impc_viability))),
              multiple = TRUE
            )
          ),
          column(
            width=4,
            materialSwitch(
              ns("impc_viability_na_switch"),
              "NA switch",
              value = TRUE
            ) |>
              tooltip("Switch On/Off to include/exclude missing values")
          )
        ),
        # F2
        fluidRow(
          column(
            width=8,
            pickerInput(
              ns("impc_zygosity_filter"),
              "Zygosity",
              choices = sort(na.omit(unique(impc_data$impc_zygosity))),
              selected = sort(na.omit(unique(impc_data$impc_zygosity))),
              multiple = TRUE
            )
          ),
          column(
            width=4,
            materialSwitch(
              ns("impc_zygosity_na_switch"),
              "NA switch",
              value = TRUE
            )
          )
        ),
        # F3
        fluidRow(
          column(
            width=8,
            pickerInput(
              ns("impc_wol_filter"),
              "Window of Lethality",
              choices = sort(na.omit(unique(impc_data$wol))),
              selected = sort(na.omit(unique(impc_data$wol))),
              multiple = TRUE
            )
          ),
          column(
            width=4,
            materialSwitch(
              ns("impc_wol_na_switch"),
              "NA switch",
              value = TRUE
            )
          )
        ),
        # F4
        fluidRow(
          column(
            width=8,
            pickerInput(
              ns("impc_ortholog_filter"),
              "Ortholog mapping",
              choices = sort(na.omit(unique(impc_data$ortholog_mapping))),
              selected = sort(na.omit(unique(impc_data$ortholog_mapping))),
              multiple = TRUE
            )
          ),
          column(
            width=4,
            materialSwitch(
              ns("impc_ortholog_na_switch"),
              "NA switch",
              value = TRUE
            )
          )
        ),
        # F5
        fluidRow(
          textAreaInput(
            ns("impc_phenotypes_filter"),
            "Filter by Mammalian Phenotype Ontology phenotype term or ID (use ; as separator)"
          
        ),
          awesomeCheckbox(
            ns("impc_phenotypes_checkbox"),
            label = "Filter for all genes with an associated phenotype", 
            value = FALSE
          ),
        awesomeCheckbox(
          ns("impc_phenotypes_checkbox2"),
          label = "Filter for all genes without an associated phenotype", 
          value = FALSE
        )
        ),
        hr(),
        awesomeCheckbox(
          ns("mgi_annotations_checkbox"),
          label = "Enable MGI filters", 
          value = FALSE
        ),
        # F1
        fluidRow(
          column(
            width=8,
            pickerInput(
              ns("mgi_viability_filter"),
              "Viability after knockout",
              choices = sort(na.omit(unique(mgi_data$mgi_viability))),
              selected = sort(na.omit(unique(mgi_data$mgi_viability))),
              multiple = TRUE
            )
          ),
          column(
            width=4,
            materialSwitch(
              ns("mgi_viability_na_switch"),
              "NA switch",
              value = TRUE
            )
          )
        )
        ),
        # OMIM UI ----
        box(title = tooltip(
          trigger = list(
            "Human disease"
          ),
          "Human disease data from the Online Mendelian Inheritance in Man (OMIM)
          and the Developmental Disorders (DD) panel in the Gene2Phenotype (G2P) database (DDG2P)"
        ), 
        collapsible = TRUE, collapsed = TRUE, width = "100%",
            awesomeCheckbox(
              ns("omim_annotations_checkbox"),
              label = "Enable OMIM filters", 
              value = FALSE
            ),
            fluidRow(
              column(
                width = 8,
              ),
              column(
                width = 4,
              )
            ),
            fluidRow(
              column(
                width = 8,
                pickerInput(
                  ns("omim_lethality_filter"),
                  "Lethal phenotypes",
                  choices = sort(na.omit(unique(omim_data$disease_gene_lethal))),
                  selected = sort(na.omit(unique(omim_data$disease_gene_lethal))),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                materialSwitch(
                  ns("omim_lethality_na_switch"),
                  "NA switch",
                  value = TRUE
                ) |>
                  tooltip("Switch On/Off to include/exclude missing values")
              )
            ),
            fluidRow(
              column(
                width = 8,
                pickerInput(
                  ns("omim_earliest_lethality_filter"),
                  "Earliest lethal phenotype",
                  choices = sort(na.omit(unique(omim_data$earliest_lethality_category))),
                  selected = sort(na.omit(unique(omim_data$earliest_lethality_category))),
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE
                  )
                )
              ),
              column(
                width = 4,
                materialSwitch(
                  ns("omim_earliest_lethality_na_switch"),
                  "NA switch",
                  value = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
                pickerInput(
                  ns("omim_number_filter"),
                  "Phenotype molecular basis",
                  choices = sort(na.omit(unique(omim_data$number_key))),
                  selected = sort(na.omit(unique(omim_data$number_key))),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                materialSwitch(
                  ns("omim_number_na_switch"),
                  "NA switch",
                  value = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
                pickerInput(
                  ns("omim_moi_filter"),
                  "Phenotype mode of inheritance",
                  choices = sort(na.omit(unique(omim_data$moi))),
                  selected = sort(na.omit(unique(omim_data$moi))),
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE
                  )
                )
              ),
              column(
                width = 4,
                materialSwitch(
                  ns("omim_moi_na_switch"),
                  "NA switch",
                  value = TRUE
                ),
              )
            ),
            fluidRow(
              textAreaInput(
                ns("omim_phenotypes_filter"),
                "Filter by OMIM phenotypes or MIM IDs (use ; as separator)"
              ),
              awesomeCheckbox(
                ns("omim_phenotypes_checkbox"),
                label = "Filter for all genes with an associated phenotype", 
                value = FALSE
              ),
              awesomeCheckbox(
                ns("omim_phenotypes_checkbox2"),
                label = "Filter for all genes without an associated phenotype", 
                value = FALSE
              )
            ),
        hr(),
        awesomeCheckbox(
          ns("ddg2p_annotations_checkbox"),
          label = "Enable DDG2P filters", 
          value = FALSE
        ),
        fluidRow(
          column(
            width = 8,
            pickerInput(
              ns("ddg2p_allelic_requirement_filter"),
              "Phenotype allelic requirement",
              choices = sort(na.omit(unique(ddg2p_data$allelic_requirement))),
              selected = sort(na.omit(unique(ddg2p_data$allelic_requirement))),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE
              )
            )
          ),
          column(
            width = 4,
            materialSwitch(
              ns("ddg2p_allelic_requirement_na_switch"),
              "NA switch",
              value = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            pickerInput(
              ns("ddg2p_organ_specificity_filter"),
              "Affected organ(s)",
              choices = sort(na.omit(unique(ddg2p_data$organ_specificity))),
              selected = sort(na.omit(unique(ddg2p_data$organ_specificity))),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE
              )
            )
          ),
          column(
            width = 4,
            materialSwitch(
              ns("ddg2p_organ_specificity_na_switch"),
              "NA switch",
              value = TRUE
            )
          )
        ),
        fluidRow(
          textAreaInput(
            ns("ddg2p_disease_name_filter"),
            "Filter by phenotypes in DD PanelApp (use ; as separator)"
          ),
        ),
        fluidRow(
          awesomeCheckbox(
            ns("ddg2p_disease_name_checkbox"),
            label = "Check to filter for all genes with an associated phenotype", 
            value = FALSE
          )
        )
      ),
        # CONSTRAINT UI ----
        box(title = tooltip(
          trigger = list(
            "Constraint scores"
          ),
          HTML("Constraint scores derived from human cell lines and population sequencing studies")
        ),
            collapsible = TRUE, collapsed = TRUE, width = "100%",
            awesomeCheckbox(
              ns("constraint_annotations_checkbox"),
              label = "Enable all constraint filters", 
              value = FALSE
            ),
            # F1
        p("Cell lines scores"),
            sliderInput(ns('mean_score_all_filter'),
                        label = 'Gene effect score - mean across all DepMap cancer lines',
                        value = c(min(constraint_data$mean_score_all, na.rm = TRUE), max(constraint_data$mean_score_all, na.rm = TRUE)),
                        min = min(constraint_data$mean_score_all, na.rm = TRUE),
                        max(constraint_data$mean_score_all, na.rm = TRUE),
                        round = 2,
                        width = "100%",
                        step = 0.01
            ),
            materialSwitch(
              ns("mean_score_all_na_switch"),
              "NA switch",
              value = TRUE
            ) |>
          tooltip("Switch On/Off to include/exclude missing values"),
            # F2
            sliderInput(ns('bf_lam_filter'),
                        label = 'Bayes Factor (Laminin substrate)',
                        value = c(min(constraint_data$bf_lam, na.rm = TRUE), max(constraint_data$bf_lam, na.rm = TRUE)),
                        min = min(constraint_data$bf_lam, na.rm = TRUE),
                        max(constraint_data$bf_lam, na.rm = TRUE),
                        round = 2,
                        width = "100%",
                        step = 0.01
            ),
            materialSwitch(
              ns("bf_lam_na_switch"),
              "NA switch",
              value = TRUE
            ),
            # F3
            sliderInput(ns('bf_mef_filter'),
                        label = 'Bayes Factor (Laminin substrate)',
                        value = c(min(constraint_data$bf_mef, na.rm = TRUE), max(constraint_data$bf_mef, na.rm = TRUE)),
                        min = min(constraint_data$bf_mef, na.rm = TRUE),
                        max(constraint_data$bf_mef, na.rm = TRUE),
                        round = 2,
                        width = "100%",
                        step = 0.01
            ),
            materialSwitch(
              ns("bf_mef_na_switch"),
              "NA switch",
              value = TRUE
            ),
        hr(),
        p("Population Sequencing scores"),
        sliderInput(ns('gnomad_lof_upper_90_ci_filter'),
                    label = 'gnomAD LOEUF score',
                    value = c(min(constraint_data$gnomad_lof_upper_90_ci, na.rm = TRUE), max(constraint_data$gnomad_lof_upper_90_ci, na.rm = TRUE)),
                    min = min(constraint_data$gnomad_lof_upper_90_ci, na.rm = TRUE),
                    max(constraint_data$gnomad_lof_upper_90_ci, na.rm = TRUE),
                    round = 2,
                    width = "100%",
                    step = 0.01
        ),
        materialSwitch(
          ns("gnomad_lof_upper_90_ci_na_switch"),
          "NA switch",
          value = TRUE
        ),
        # F2
        sliderInput(ns('mean_am_pathogenicity_filter'),
                    label = 'Alpha Missense pathogenicity prediction (mean)',
                    value = c(min(constraint_data$mean_am_pathogenicity, na.rm = TRUE), max(constraint_data$mean_am_pathogenicity, na.rm = TRUE)),
                    min = min(constraint_data$mean_am_pathogenicity, na.rm = TRUE),
                    max(constraint_data$mean_am_pathogenicity, na.rm = TRUE),
                    round = 2,
                    width = "100%",
                    step = 0.01
        ),
        materialSwitch(
          ns("mean_am_pathogenicity_na_switch"),
          "NA switch",
          value = TRUE
        ),
        # F3
        sliderInput(ns('shet_rgcme_mean_filter'),
                    label = 'Shet (Sun et al. 2023)',
                    value = c(min(constraint_data$shet_rgcme_mean, na.rm = TRUE), max(constraint_data$shet_rgcme_mean, na.rm = TRUE)),
                    min = min(constraint_data$shet_rgcme_mean, na.rm = TRUE),
                    max(constraint_data$shet_rgcme_mean, na.rm = TRUE),
                    round = 2,
                    width = "100%",
                    step = 0.01
        ),
        materialSwitch(
          ns("shet_rgcme_mean_na_switch"),
          "NA switch",
          value = TRUE
        ),
        # F4
        sliderInput(ns('shet_post_mean_filter'),
                    label = 'Shet (Zeng et al. 2023)',
                    value = c(min(constraint_data$shet_post_mean, na.rm = TRUE), max(constraint_data$shet_post_mean, na.rm = TRUE)),
                    min = min(constraint_data$shet_post_mean, na.rm = TRUE),
                    max(constraint_data$shet_post_mean, na.rm = TRUE),
                    round = 2,
                    width = "100%",
                    step = 0.01
        ),
        materialSwitch(
          ns("shet_post_mean_na_switch"),
          "NA switch",
          value = TRUE
        ),
        # F5
        sliderInput(ns('domino_filter'),
                    label = 'DOMINO',
                    value = c(min(constraint_data$domino, na.rm = TRUE), max(constraint_data$domino, na.rm = TRUE)),
                    min = min(constraint_data$domino, na.rm = TRUE),
                    max(constraint_data$domino, na.rm = TRUE),
                    round = 2,
                    width = "100%",
                    step = 0.01
        ),
        materialSwitch(
          ns("domino_na_switch"),
          "NA switch",
          value = TRUE
        ),
        # F6
        sliderInput(ns('scones_filter'),
                    label = 'SCoNeS',
                    value = c(min(constraint_data$scones, na.rm = TRUE), max(constraint_data$scones, na.rm = TRUE)),
                    min = min(constraint_data$scones, na.rm = TRUE),
                    max(constraint_data$scones, na.rm = TRUE),
                    round = 2,
                    width = "100%",
                    step = 0.01
        ),
        materialSwitch(
          ns("scones_na_switch"),
          "NA switch",
          value = TRUE
        )
        ),

       # F1

     # F1
    # GENES BOX FILTER ----
     box(title = tooltip(
       trigger = list(
         "Gene list upload/entry"
       ),
       HTML("Filter by gene symbol or gene ID")
     ), collapsible = TRUE, collapsed = TRUE, width = "100%",
         textAreaInput(
           ns("gene_list_filter"), 
           "Filter by gene symbol or ID (HGNC, Ensembl, Entrez)  (use ; as separator)"
           ),
     p("Files uploaded will automatically be saved"),
     fileInput(
       ns("user_files_upload"),
       "Upload gene list(s) as csv or tsv",
       multiple = TRUE,
       accept = NULL,
       width = "100%",
       buttonLabel = "Browse...",
       placeholder = "No file selected"
     ),
     fluidRow(uiOutput(ns("input_checker_output")))
    
      )
     ),
     # Save filtered lists ----
      column(
        width = 6,
        # Current filtered list ----
        progressBar(ns("progress_bar"), value = nrow(pcg_data), total = nrow(pcg_data), title = "Number of genes after filtering", display_pct = TRUE),
        reactableOutput(ns("gene_table")),
        downloadButton(ns("downloadGenes"), "Download table")
      ),
     column(
       width = 3,
       p("Save filtered gene list"),
       p("To edit a list, select filters and save under the same name"),
       fluidRow(
         column(
           width = 6,
           textInput(ns("add_gene_list_name"), label = NULL, placeholder = "Add list name")
         ),
         column(
           width = 3,
           actionButton(ns("add_gene_list"), "Save")
         )
       ),
       fluidRow(
         column(
           width = 6,
           reactableOutput(ns("saved_lists_table"))
         ),
         column(
           width = 3,
           actionButton(ns("clear_saved_lists"), "Clear")
         )
       ),
       fluidRow(
         p("Currently applied filters:"),
         fluidRow(
  
           #p("Mouse models"),
           uiOutput(ns("applied_filters_box_impc")),
           #p("MGI"),
           uiOutput(ns("applied_filters_box_mgi")),
           
      
           #p("Diseases"),
           #p("OMIM"),
           uiOutput(ns("applied_filters_box_omim")),
           #p("DDG2P"),
           uiOutput(ns("applied_filters_box_ddg2p")),
         
     
           #p("Constraint metrics"),
           #p("Cell lines"),
           uiOutput(ns("applied_filters_box_cell_lines")),
           #p("Sequencing"),
           uiOutput(ns("applied_filters_box_sequencing"))
           
         )
       )
     )
    )
    )
}


#' @export
server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    
    # Data ----
    pcg_data <- data_list[["pcg_data"]]
    impc_data <- data_list[["impc_data"]]
    mgi_data <- data_list[["mgi_data"]]
    omim_data <- data_list[["omim_data"]]
    ddg2p_data <- data_list[["ddg2p_data"]]
    constraint_data <- data_list[["constraint_data"]]
    tbl_all <- data_list[["tbl_all"]]
    
    # Filters ----
    # applied_mouse----
    # output$applied_filters_box_impc <- renderUI({
    #   output_html <- character()
    #   output_html <- c(output_html, HTML(paste("<u>","IMPC","</u>")))
    #   
    #   if (input$impc_annotations_checkbox == TRUE) {
    #     if (length(unique(impc_data$impc_viability[!is.na(impc_data$impc_viability)])) > length(input$impc_viability_filter)) {
    #       output <- paste("Viability: ", paste(input$impc_viability_filter, collapse = ", "))
    #       output_html <- c(output_html, HTML(output))
    #     } 
    #     
    #     if (length(unique(impc_data$impc_zygosity[!is.na(impc_data$impc_zygosity)])) > length(input$impc_zygosity_filter)) {
    #       output <- paste("Zygosity: ", paste(input$impc_zygosity_filter, collapse = ", "))
    #       output_html <- c(output_html, HTML(output))
    #     } 
    #     
    #     if (length(unique(impc_data$wol[!is.na(impc_data$wol)])) > length(input$impc_wol_filter)) {
    #       output <- paste("Window of Lethality: ", paste(input$impc_wol_filter, collapse = ", "))
    #       output_html <- c(output_html, HTML(output))
    #     } 
    #     
    #     if (length(unique(impc_data$ortholog_mapping[!is.na(impc_data$ortholog_mapping)])) > length(input$impc_ortholog_filter)) {
    #       output <- paste("Orthologs: ", paste(input$impc_ortholog_filter, collapse = ", "))
    #       output_html <- c(output_html, HTML(output))
    #     }
    #     
    #     if(input$impc_phenotypes_checkbox == TRUE) {
    #       output_html <- c(output_html, HTML("All genes with an associated IMPC phenotype"))
    #     }
    #     
    #     if (nchar(input$impc_phenotypes_filter) > 0) {
    #       output_html <- c(output_html, HTML("Custom IMPC phenotypes list"))
    #     }
    #     
    #   } else {
    #     output_html <- c(output_html ,"None")
    #     
    #   }
    # 
    #   # Concatenate the HTML outputs with line breaks
    #   output_html <- paste(output_html, collapse = "<br>")
    #   
    #   return(HTML(output_html))
    #   
    # })
    na_ON <- "Missing values included"
    na_OFF <- "Missing values excluded"
    
    output$applied_filters_box_impc <- renderUI({
      
      output_html <- character()
      output_html <- c(output_html, HTML(paste("<u>","IMPC","</u>")))
      if (input$impc_annotations_checkbox == TRUE) {
        if (length(unique(impc_data$impc_viability[!is.na(impc_data$impc_viability)])) != length(input$impc_viability_filter) & input$impc_viability_na_switch == TRUE) {
          output <- paste("Viability: ", paste(input$impc_viability_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$impc_viability_na_switch == FALSE) {
          output <- paste("Viability: ", paste(input$impc_viability_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(impc_data$impc_viability[!is.na(impc_data$impc_viability)])) == length(input$impc_viability_filter) & input$impc_viability_na_switch == TRUE) {
          NULL
        }
        
        if (length(unique(impc_data$impc_zygosity[!is.na(impc_data$impc_zygosity)])) > length(input$impc_zygosity_filter) & input$impc_zygosity_na_switch == TRUE) {
          output <- paste("Zygosity: ", paste(input$impc_zygosity_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        }  else if (input$impc_zygosity_na_switch == FALSE) {
          output <- paste("Zygosity: ", paste(input$impc_zygosity_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(impc_data$impc_zygosity[!is.na(impc_data$impc_zygosity)])) == length(input$impc_zygosity_filter) & input$impc_zygosity_na_switch == TRUE) {
          NULL
        }
        
        if (length(unique(impc_data$wol[!is.na(impc_data$wol)])) > length(input$impc_wol_filter) & input$impc_wol_na_switch == TRUE) {
          output <- paste("Window of Lethality: ", paste(input$impc_wol_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        }  else if (input$impc_wol_na_switch == FALSE) {
          output <- paste("Window of Lethality: ", paste(input$impc_wol_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(impc_data$wol[!is.na(impc_data$wol)])) == length(input$impc_wol_filter) & input$impc_wol_na_switch == TRUE) {
          NULL
        }
        
        if (length(unique(impc_data$ortholog_mapping[!is.na(impc_data$ortholog_mapping)])) > length(input$impc_ortholog_filter) & input$impc_ortholog_na_switch == TRUE) {
          output <- paste("Orthologs: ", paste(input$impc_ortholog_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$impc_ortholog_na_switch == FALSE) {
          output <- paste("Orthologs: ", paste(input$impc_ortholog_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(impc_data$ortholog_mapping[!is.na(impc_data$ortholog_mapping)])) == length(input$impc_ortholog_filter) & input$impc_ortholog_na_switch == TRUE) {
          NULL
        }
        
        if (input$impc_phenotypes_checkbox == TRUE) {
          output_html <- c(output_html, HTML("All genes with an associated IMPC phenotype"))
          shinyjs::disable("impc_phenotypes_checkbox2")
        } else {
          shinyjs::enable("impc_phenotypes_checkbox2")
        }
        
        if (input$impc_phenotypes_checkbox2 == TRUE) {
          output_html <- c(output_html, HTML("All genes without an associated IMPC phenotype"))
          shinyjs::disable("impc_phenotypes_checkbox")
        } else {
          shinyjs::enable("impc_phenotypes_checkbox")
        }
        
        if (nchar(input$impc_phenotypes_filter) > 0) {
          output_html <- c(output_html, HTML("Custom IMPC phenotypes list"))
        }
      } else if (input$impc_annotations_checkbox == FALSE) {
        output_html <- c(output_html ,"None")
      }
      # Concatenate the HTML outputs with line breaks
      output_html <- paste(output_html, collapse = "<br>")
      
      return(HTML(output_html))
      
    })
    
    output$applied_filters_box_mgi <- renderUI({
      output_html <- character()
      output_html <- c(output_html, HTML(paste("<u>","MGI","</u>")))
      
      if (input$mgi_annotations_checkbox == TRUE) {
        if (length(unique(mgi_data$mgi_viability[!is.na(mgi_data$mgi_viability)])) > length(input$mgi_viability_filter) & input$mgi_viability_na_switch == TRUE) {
          output <- paste("Viability: ", paste(input$mgi_viability_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$mgi_viability_na_switch == FALSE) {
          output <- paste("Viability: ", paste(input$mgi_viability_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(mgi_data$mgi_viability[!is.na(mgi_data$mgi_viability)])) == length(input$mgi_viability_filter) & input$mgi_viability_na_switch == TRUE) {
          NULL
        }
        
      } else {
        output_html <- c(output_html ,"None")
        
      }
      
      # Concatenate the HTML outputs with line breaks
      output_html <- paste(output_html, collapse = "<br>")
      
      return(HTML(output_html))
      
    })
    
    # applied_disease----
    output$applied_filters_box_omim <- renderUI({
      output_html <- character()
      output_html <- c(output_html, HTML(paste("<u>","OMIM","</u>")))
      
      if (input$omim_annotations_checkbox == TRUE) {
        if (length(unique(omim_data$disease_gene_lethal[!is.na(omim_data$disease_gene_lethal)])) > length(input$omim_lethality_filter) & input$omim_lethality_na_switch == TRUE) {
          output <- paste("Lethality: ", paste(input$omim_lethality_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$omim_lethality_na_switch == FALSE) {
          output <- paste("Lethality: ", paste(input$omim_lethality_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(omim_data$disease_gene_lethal[!is.na(omim_data$disease_gene_lethal)])) == length(input$omim_lethality_filter) & input$omim_lethality_na_switch == TRUE) {
          NULL
        }
        
        if (length(unique(omim_data$earliest_lethality_category[!is.na(omim_data$earliest_lethality_category)])) > length(input$omim_earliest_lethality_filter) & input$omim_earliest_lethality_na_switch == TRUE) {
          output <- paste("Earliest Lethality: ", paste(input$omim_earliest_lethality_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$omim_earliest_lethality_na_switch == FALSE) {
          output <- paste("Earliest Lethality: ", paste(input$omim_earliest_lethality_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(omim_data$earliest_lethality_category[!is.na(omim_data$earliest_lethality_category)])) == length(input$omim_earliest_lethality_filter) & input$omim_earliest_lethality_na_switch == TRUE) {
          NULL
        }
        
        if (length(unique(omim_data$number_key[!is.na(omim_data$number_key)])) > length(input$omim_number_filter) & input$omim_number_na_switch == TRUE) {
          output <- paste("Molecular basis: ", paste(input$omim_number_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$omim_number_na_switch == FALSE) {
          output <- paste("Molecular basis: ", paste(input$omim_number_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(omim_data$number_key[!is.na(omim_data$number_key)])) == length(input$omim_number_filter) & input$omim_number_na_switch == TRUE) {
          NULL
        }
        
        if (length(unique(omim_data$moi[!is.na(omim_data$moi)])) > length(input$omim_moi_filter) & input$omim_moi_na_switch == TRUE) {
          output <- paste("Mode of inheritance: ", paste(input$omim_moi_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$omim_moi_na_switch == FALSE) {
          output <- paste("Mode of inheritance: ", paste(input$omim_moi_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (length(unique(omim_data$moi[!is.na(omim_data$moi)])) == length(input$omim_moi_filter) & input$omim_moi_na_switch == TRUE) {
          NULL
        }
        
        if(input$omim_phenotypes_checkbox == TRUE) {
          output_html <- c(output_html, HTML("All genes with an associated OMIM phenotype"))
          shinyjs::disable("omim_phenotypes_checkbox2")
        } else {
          shinyjs::enable("omim_phenotypes_checkbox2")
        }
        
        if(input$omim_phenotypes_checkbox2 == TRUE) {
          output_html <- c(output_html, HTML("All genes without an associated OMIM phenotype"))
          shinyjs::disable("omim_phenotypes_checkbox")
        } else {
          shinyjs::enable("omim_phenotypes_checkbox")
        }
        
        if (nchar(input$omim_phenotypes_filter) > 0) {
          output_html <- c(output_html, HTML("Custom OMIM phenotypes list"))
        }
        
      } else {
        output_html <- c(output_html ,"None")
        
      }
      
      # Concatenate the HTML outputs with line breaks
      output_html <- paste(output_html, collapse = "<br>")
      
      return(HTML(output_html))
      
    })
    
    output$applied_filters_box_ddg2p <- renderUI({
      output_html <- character()
      output_html <- c(output_html, HTML(paste("<u>","DDG2P","</u>")))
      
      if (input$ddg2p_annotations_checkbox == TRUE) {
        if (length(unique(ddg2p_data$allelic_requirement[!is.na(ddg2p_data$allelic_requirement)])) > length(input$ddg2p_allelic_requirement_filter)) {
          output <- paste("Allelic requirement: ", paste(input$ddg2p_allelic_requirement_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output))
        } 
        
        if (length(unique(ddg2p_data$organ_specificity[!is.na(ddg2p_data$organ_specificity)])) > length(input$ddg2p_organ_specificity_filter)) {
          output <- paste("Affected organ(s): ", paste(input$ddg2p_organ_specificity_filter, collapse = ", "))
          output_html <- c(output_html, HTML(output))
        } 
        
        if(input$ddg2p_disease_name_checkbox == TRUE) {
          output_html <- c(output_html, HTML("All genes with an associated DDG2P phenotype"))
        }
        
        if (nchar(input$ddg2p_disease_name_filter) > 0) {
          output_html <- c(output_html, HTML("Custom DDG2P phenotypes list"))
        }
        
      } else {
        output_html <- c(output_html ,"None")
        
      }
      
      # Concatenate the HTML outputs with line breaks
      output_html <- paste(output_html, collapse = "<br>")
      
      return(HTML(output_html))
      
    })
    
    # applied_constraint----
    output$applied_filters_box_cell_lines <- renderUI({
      output_html <- character()
      output_html <- c(output_html, HTML(paste("<u>","Cell Lines","</u>")))
      
      if (input$constraint_annotations_checkbox == TRUE) {
        
        # if (min(constraint_data$mean_score_all, na.rm = TRUE) != input$mean_score_all_filter[1] | max(constraint_data$mean_score_all, na.rm = TRUE) != input$mean_score_all_filter[2]) {
        #   output <- paste("Achilles score: ", paste("min: ", input$mean_score_all_filter[1], ", ", "max: ",  input$mean_score_all_filter[2]))
        #   output_html <- c(output_html, HTML(output))
        # } 
        if (min(constraint_data$mean_score_all, na.rm = TRUE) != input$mean_score_all_filter[1] & input$mean_score_all_na_switch == FALSE | max(constraint_data$mean_score_all, na.rm = TRUE) != input$mean_score_all_filter[2] & input$mean_score_all_na_switch == FALSE) {
          output <- paste("Achilles score: ", paste("min: ", input$mean_score_all_filter[1], ", ", "max: ",  input$mean_score_all_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (min(constraint_data$mean_score_all, na.rm = TRUE) != input$mean_score_all_filter[1] & input$mean_score_all_na_switch == TRUE | max(constraint_data$mean_score_all, na.rm = TRUE) != input$mean_score_all_filter[2] & input$mean_score_all_na_switch == TRUE) {
          output <- paste("Achilles score: ", paste("min: ", input$mean_score_all_filter[1], ", ", "max: ",  input$mean_score_all_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (min(constraint_data$mean_score_all, na.rm = TRUE) == input$mean_score_all_filter[1] & input$mean_score_all_na_switch == FALSE | max(constraint_data$mean_score_all, na.rm = TRUE) == input$mean_score_all_filter[2] & input$mean_score_all_na_switch == FALSE) {
          output <- paste("Achilles score: ", paste("min: ", input$mean_score_all_filter[1], ", ", "max: ",  input$mean_score_all_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        
        if (min(constraint_data$bf_lam, na.rm = TRUE) != input$bf_lam_filter[1] & input$bf_lam_na_switch == FALSE | max(constraint_data$bf_lam, na.rm = TRUE) != input$bf_lam_filter[2] & input$bf_lam_na_switch == FALSE) {
          output <- paste("Bayes Factor (laminin): ", paste("min: ", input$bf_lam_filter[1], ", ", "max: ",  input$bf_lam_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (min(constraint_data$bf_lam, na.rm = TRUE) != input$bf_lam_filter[1] & input$bf_lam_na_switch == TRUE | max(constraint_data$bf_lam, na.rm = TRUE) != input$bf_lam_filter[2] & input$bf_lam_na_switch == TRUE) {
          output <- paste("Bayes Factor (laminin): ", paste("min: ", input$bf_lam_filter[1], ", ", "max: ",  input$bf_lam_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (min(constraint_data$bf_lam, na.rm = TRUE) == input$bf_lam_filter[1] & input$bf_lam_na_switch == FALSE | max(constraint_data$bf_lam, na.rm = TRUE) == input$bf_lam_filter[2] & input$bf_lam_na_switch == FALSE) {
          output <- paste("Bayes Factor (laminin): ", paste("min: ", input$bf_lam_filter[1], ", ", "max: ",  input$bf_lam_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        if (min(constraint_data$bf_mef, na.rm = TRUE) != input$bf_mef_filter[1] & input$bf_mef_na_switch == FALSE | max(constraint_data$bf_mef, na.rm = TRUE) != input$bf_mef_filter[2] & input$bf_mef_na_switch == FALSE ) {
          output <- paste("Bayes Factor (mef): ", paste("min: ", input$bf_mef_filter[1], ", ", "max: ",  input$bf_mef_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        } else if (min(constraint_data$bf_mef, na.rm = TRUE) != input$bf_mef_filter[1] & input$bf_mef_na_switch == TRUE | max(constraint_data$bf_mef, na.rm = TRUE) != input$bf_mef_filter[2] & input$bf_mef_na_switch == TRUE ) {
          output <- paste("Bayes Factor (mef): ", paste("min: ", input$bf_mef_filter[1], ", ", "max: ",  input$bf_mef_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (min(constraint_data$bf_mef, na.rm = TRUE) == input$bf_mef_filter[1] & input$bf_mef_na_switch == FALSE | max(constraint_data$bf_mef, na.rm = TRUE) == input$bf_mef_filter[2] & input$bf_mef_na_switch == FALSE ) {
          output <- paste("Bayes Factor (mef): ", paste("min: ", input$bf_mef_filter[1], ", ", "max: ",  input$bf_mef_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        
      } else {
        output_html <- c(output_html ,"None")
      }
      
      # Concatenate the HTML outputs with line breaks
      output_html <- paste(output_html, collapse = "<br>")
      
      return(HTML(output_html))
      
    })
    
    output$applied_filters_box_sequencing <- renderUI({
      output_html <- character()
      output_html <- c(output_html, HTML(paste("<u>","Population Sequencing","</u>")))
      
      if (input$constraint_annotations_checkbox == TRUE) {
        if (min(constraint_data$gnomad_lof_upper_90_ci, na.rm = TRUE) != input$gnomad_lof_upper_90_ci_filter[1] & input$gnomad_lof_upper_90_ci_na_switch == TRUE | max(constraint_data$gnomad_lof_upper_90_ci, na.rm = TRUE) != input$gnomad_lof_upper_90_ci_filter[2] & input$gnomad_lof_upper_90_ci_na_switch == TRUE ) {
          output <- paste("LOEUF score: ", paste("min: ", input$gnomad_lof_upper_90_ci_filter[1], ", ", "max: ",  input$gnomad_lof_upper_90_ci_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$gnomad_lof_upper_90_ci_na_switch == FALSE) {
          output <- paste("LOEUF score: ", paste("min: ", input$gnomad_lof_upper_90_ci_filter[1], ", ", "max: ",  input$gnomad_lof_upper_90_ci_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        if (min(constraint_data$mean_am_pathogenicity, na.rm = TRUE) != input$mean_am_pathogenicity_filter[1]  & input$mean_am_pathogenicity_na_switch == TRUE | max(constraint_data$mean_am_pathogenicity, na.rm = TRUE) != input$mean_am_pathogenicity_filter[2]  & input$mean_am_pathogenicity_na_switch == TRUE) {
          output <- paste("AlphaMissense score: ", paste("min: ", input$mean_am_pathogenicity_filter[1], ", ", "max: ",  input$mean_am_pathogenicity_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$mean_am_pathogenicity_na_switch == FALSE) {
          output <- paste("AlphaMissense score: ", paste("min: ", input$mean_am_pathogenicity_filter[1], ", ", "max: ",  input$mean_am_pathogenicity_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        if (min(constraint_data$shet_rgcme_mean, na.rm = TRUE) != input$shet_rgcme_mean_filter[1] & input$shet_rgcme_mean_na_switch == TRUE | max(constraint_data$shet_rgcme_mean, na.rm = TRUE) != input$shet_rgcme_mean_filter[2] & input$shet_rgcme_mean_na_switch == TRUE) {
          output <- paste("Shet score (RGC-ME): ", paste("min: ", input$shet_rgcme_mean_filter[1], ", ", "max: ",  input$shet_rgcme_mean_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$shet_rgcme_mean_na_switch == FALSE) {
          output <- paste("Shet score (RGC-ME): ", paste("min: ", input$shet_rgcme_mean_filter[1], ", ", "max: ",  input$shet_rgcme_mean_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        if (min(constraint_data$shet_post_mean, na.rm = TRUE) != input$shet_post_mean_filter[1] & input$shet_post_mean_na_switch == TRUE | max(constraint_data$shet_post_mean, na.rm = TRUE) != input$shet_post_mean_filter[2]& input$shet_post_mean_na_switch == TRUE) {
          output <- paste("Shet score (gnomAD): ", paste("min: ", input$shet_post_mean_filter[1], ", ", "max: ",  input$shet_post_mean_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$shet_post_mean_na_switch == FALSE) {
          output <- paste("Shet score (gnomAD): ", paste("min: ", input$shet_post_mean_filter[1], ", ", "max: ",  input$shet_post_mean_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        if (min(constraint_data$domino, na.rm = TRUE) != input$domino_filter[1] & input$domino_na_switch == TRUE | max(constraint_data$domino, na.rm = TRUE) != input$domino_filter[2] & input$domino_na_switch == TRUE) {
          output <- paste("DOMINO score: ", paste("min: ", input$domino_filter[1], ", ", "max: ",  input$domino_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$domino_na_switch == FALSE) {
          output <- paste("DOMINO score: ", paste("min: ", input$domino_filter[1], ", ", "max: ",  input$domino_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        }
        
        if (min(constraint_data$scones, na.rm = TRUE) != input$scones_filter[1] & input$scones_na_switch == TRUE | max(constraint_data$scones, na.rm = TRUE) != input$scones_filter[2] & input$scones_na_switch == TRUE) {
          output <- paste("SCoNeS score: ", paste("min: ", input$scones_filter[1], ", ", "max: ",  input$scones_filter[2]))
          output_html <- c(output_html, HTML(output), na_ON)
        } else if (input$scones_na_switch == FALSE) {
          output <- paste("SCoNeS score: ", paste("min: ", input$scones_filter[1], ", ", "max: ",  input$scones_filter[2]))
          output_html <- c(output_html, HTML(output), na_OFF)
        } 
        
        
        
      } else {
        output_html <- c(output_html, "None")
      }
      
      # Concatenate the HTML outputs with line breaks
      output_html <- paste(output_html, collapse = "<br>")
      
      return(HTML(output_html))
      
    })
    
    # IMPC FILTERS SERVER ----
    filtered_table_impc <- shiny::reactiveVal()
    
    shiny::observe({
      if (input$impc_annotations_checkbox == TRUE) {
        if (input$impc_viability_na_switch == TRUE) {
          filtered_table1 <- impc_data %>%
            dplyr::filter(impc_viability %in% input$impc_viability_filter | is.na(impc_viability))
        } else if (input$impc_viability_na_switch == FALSE) {
          filtered_table1 <- impc_data %>%
            dplyr::filter(impc_viability %in% input$impc_viability_filter & !is.na(impc_viability))
        }
        
        if (input$impc_zygosity_na_switch == TRUE) {
          filtered_table2 <- filtered_table1 %>%
            dplyr::filter(impc_zygosity %in% input$impc_zygosity_filter | is.na(impc_zygosity))
        } else if (input$impc_zygosity_na_switch == FALSE) {
          filtered_table2 <- filtered_table1 %>%
            dplyr::filter(impc_zygosity %in% input$impc_zygosity_filter & !is.na(impc_zygosity))
        }
        
        if (input$impc_wol_na_switch == TRUE) {
          filtered_table3 <- filtered_table2 %>%
            dplyr::filter(wol %in% input$impc_wol_filter | is.na(wol))
        } else if (input$impc_wol_na_switch == FALSE) {
          filtered_table3 <- filtered_table2 %>%
            dplyr::filter(wol %in% input$impc_wol_filter & !is.na(wol))
        }
        
        if (input$impc_ortholog_na_switch == TRUE) {
          filtered_table4 <- filtered_table3 %>%
            dplyr::filter(ortholog_mapping %in% input$impc_ortholog_filter | is.na(ortholog_mapping))
        } else if (input$impc_ortholog_na_switch == FALSE) {
          filtered_table4 <- filtered_table3 %>%
            dplyr::filter(ortholog_mapping %in% input$impc_ortholog_filter & !is.na(ortholog_mapping))
        }
        
        if (input$impc_phenotypes_checkbox == TRUE) {
          
          filtered_table5 <- filtered_table4 %>%
            dplyr::filter(!is.na(impc_phenotypes))
          
        } else if (input$impc_phenotypes_checkbox == FALSE) {
          if (nchar(input$impc_phenotypes_filter) > 0) {
            # Split the input text by semicolons to get individual entries
            input_entries <- unlist(strsplit(input$impc_phenotypes_filter, ";"))
            
            # Trim leading and trailing whitespace from each entry
            input_entries <- trimws(input_entries)
            
            # Identify if the first entry is an ID (6 digits)
            first_entry_is_id <- grepl("^MP:", input_entries[1])
            
            if (first_entry_is_id) {
              # Search the phenotype_id column
              filtered_table5 <- filtered_table4 %>%
                filter(mp_id %in% input_entries)
            } else {
              # Search the phenotypes column
              filtered_table5 <- filtered_table4 %>%
                filter(impc_phenotypes %in% input_entries)
            }
          } else {
            # No text entered in the textarea, so keep the DataFrame as is
            filtered_table5 <- filtered_table4
          }
        }
        
        if (input$impc_phenotypes_checkbox2 == TRUE) {
          
          filtered_table5 <- filtered_table4 %>%
            dplyr::filter(is.na(impc_phenotypes))
          
        }
        
        # To prevent the bar moving from 100% when no filters are applied yet
        if (nrow(filtered_table5) == nrow(impc_data)) {
          filtered_table5 <- NULL
        }
        
      } else if (input$impc_annotations_checkbox == FALSE) {
        filtered_table5 <- NULL
        
      }
      
      filtered_table_impc(filtered_table5)
    })
    
    # MGI FILTERS SERVER ----
    filtered_table_mgi <- shiny::reactiveVal()
    
    shiny::observe({
      if (input$mgi_annotations_checkbox == TRUE) {
        if (input$mgi_viability_na_switch == TRUE) {
          filtered_table1 <- mgi_data %>%
            dplyr::filter(mgi_viability %in% input$mgi_viability_filter | is.na(mgi_viability))
        } else if (input$mgi_viability_na_switch == FALSE) {
          filtered_table1 <- mgi_data %>%
            dplyr::filter(mgi_viability %in% input$mgi_viability_filter & !is.na(mgi_viability))
        }
        
        if (nrow(filtered_table1) == nrow(mgi_data)) {
          filtered_table1 <- NULL
        }
        
      } else if (input$mgi_annotations_checkbox == FALSE) {
        filtered_table1 <- NULL
        
      }
      
      filtered_table_mgi(filtered_table1)
    })
    
    # OMIM FILTERS SERVER ----
    filtered_table_omim <- shiny::reactiveVal()
    
    shiny::observe({
      
      if (input$omim_annotations_checkbox == TRUE) {
        
        if (input$omim_lethality_na_switch == TRUE) {
          # filtered_table1 <- omim_data %>%
          #   dplyr::filter(disease_gene_lethal %in% input$omim_lethality_filter  | is.na(disease_gene_lethal)) %>%
          #   dplyr::select(gene_symbol, disease_gene_lethal) %>%
          #   dplyr::distinct() %>%
          #   dplyr::count(gene_symbol) %>%
          #   dplyr::filter(n == 1)
          
          omim_data2 <- omim_data %>%
            dplyr::select(gene_symbol, disease_gene_lethal) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- omim_data %>%
            dplyr::filter(disease_gene_lethal %in% input$omim_lethality_filter | is.na(disease_gene_lethal)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table1 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4)

        } else if (input$omim_lethality_na_switch == FALSE) {
          # filtered_table1 <- omim_data %>%
          #   dplyr::filter(disease_gene_lethal %in% input$omim_lethality_filter  & !is.na(disease_gene_lethal)) %>%
          #   dplyr::select(gene_symbol, disease_gene_lethal) %>%
          #   dplyr::distinct() %>%
          #   dplyr::count(gene_symbol) %>%
          #   dplyr::filter(n == 1)
          
          omim_data2 <- omim_data %>%
            dplyr::select(gene_symbol, disease_gene_lethal) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- omim_data %>%
            dplyr::filter(disease_gene_lethal %in% input$omim_lethality_filter & !is.na(disease_gene_lethal)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table1 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4 )
        }
        
        if (input$omim_phenotypes_checkbox == TRUE) {
          
          filtered_table2 <- filtered_table1 %>%
            dplyr::filter(!is.na(phenotypes))
          
        } else if (input$omim_phenotypes_checkbox == FALSE) {
          if (nchar(input$omim_phenotypes_filter) > 0) {
            # Split the input text by semicolons to get individual entries
            input_entries <- unlist(strsplit(input$omim_phenotypes_filter, ";"))
            # Trim leading and trailing whitespace from each entry
            input_entries <- trimws(input_entries)
            
            # Identify if the first entry is an ID (6 digits)
            first_entry_is_id <- grepl("^\\d{6}$", input_entries[1])
            
            if (first_entry_is_id) {
              # Search the phenotype_id column
              filtered_table2 <- filtered_table1 %>%
                filter(phenotype_id %in% input_entries)
            } else {
              # Search the phenotypes column
              filtered_table2 <- filtered_table1 %>%
                filter(phenotypes %in% input_entries)
            }
          } else {
            # No text entered in the textarea, so keep the DataFrame as is
            filtered_table2 <- filtered_table1
          }
        }
        
        if (input$omim_phenotypes_checkbox2 == TRUE) {
          
          filtered_table2 <- filtered_table1 %>%
            dplyr::filter(is.na(phenotypes))
          
        }
        
        if (input$omim_moi_na_switch == TRUE) {
          # filtered_table3 <- filtered_table2 %>%
          #   dplyr::filter(moi %in% input$omim_moi_filter  | is.na(moi))
          omim_data2 <- filtered_table2 %>%
            dplyr::select(gene_symbol, moi) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- filtered_table2 %>%
            dplyr::filter(moi %in% input$omim_moi_filter | is.na(moi)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table3 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4)
        } else if (input$omim_moi_na_switch == FALSE) {
          # filtered_table3 <- filtered_table2 %>%
          #   dplyr::filter(moi %in% input$omim_moi_filter  & !is.na(moi))
          omim_data2 <- filtered_table2 %>%
            dplyr::select(gene_symbol, moi) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- filtered_table2 %>%
            dplyr::filter(moi %in% input$omim_moi_filter & !is.na(moi)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table3 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4)
        }
        
        if (input$omim_earliest_lethality_na_switch == TRUE) {
          # filtered_table4 <- filtered_table3 %>%
          #   dplyr::filter(earliest_lethality_category %in% input$omim_earliest_lethality_filter  | is.na(earliest_lethality_category))
          omim_data2 <- filtered_table3 %>%
            dplyr::select(gene_symbol, earliest_lethality_category) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- filtered_table3 %>%
            dplyr::filter(earliest_lethality_category %in% input$omim_earliest_lethality_filter | is.na(earliest_lethality_category)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table4 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4 )
        } else if (input$omim_earliest_lethality_na_switch == FALSE) {
          # filtered_table4 <- filtered_table3 %>%
          #   dplyr::filter(earliest_lethality_category %in% input$omim_earliest_lethality_filter  & !is.na(earliest_lethality_category))
          omim_data2 <- filtered_table3 %>%
            dplyr::select(gene_symbol, earliest_lethality_category) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- filtered_table3 %>%
            dplyr::filter(earliest_lethality_category %in% input$omim_earliest_lethality_filter & !is.na(earliest_lethality_category)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table4 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4 )
        }
        
        
        if (input$omim_number_na_switch == TRUE) {
          # filtered_table5 <- filtered_table4 %>%
          #   dplyr::filter(number_key %in% input$omim_number_filter  | is.na(number_key))
          omim_data2 <- filtered_table4 %>%
            dplyr::select(gene_symbol, number_key) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- filtered_table4 %>%
            dplyr::filter(number_key %in% input$omim_number_filter | is.na(number_key)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table5 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4)
        } else if (input$omim_number_na_switch == FALSE) {
          # filtered_table5 <- filtered_table4 %>%
          #   dplyr::filter(number_key %in% input$omim_number_filter  & !is.na(number_key))
          omim_data2 <- filtered_table4 %>%
            dplyr::select(gene_symbol, number_key) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          omim_data3 <- filtered_table4 %>%
            dplyr::filter(number_key %in% input$omim_number_filter & !is.na(number_key)) %>%
            dplyr::pull(gene_symbol)
          
          omim_data4 <- intersect(omim_data2, omim_data3)
          
          filtered_table5 <- omim_data %>%
            dplyr::filter(gene_symbol %in% omim_data4)
        }
        
        # To prevent the bar moving from 100% when no filters are applied yet
        if (nrow(filtered_table5) == nrow(omim_data)) {
          filtered_table5 <- NULL
        }
        
      } else if (input$omim_annotations_checkbox == FALSE) {
        
        filtered_table5 <- NULL
        
      }
      
      filtered_table_omim(filtered_table5)
    })
    
    # DDG2P FILTER SERVER ----
    filtered_table_ddg2p <- shiny::reactiveVal()
    
    shiny::observe({
      
      if (input$ddg2p_annotations_checkbox == TRUE) {
        
        if (input$ddg2p_disease_name_checkbox == TRUE) {
          
          filtered_table1 <- ddg2p_data %>%
            dplyr::filter(!is.na(disease_name))
          
        } else if (input$ddg2p_disease_name_checkbox == FALSE) {
          if (nchar(input$ddg2p_disease_name_filter) > 0) {
            # Split the input text by semicolons to get individual entries
            input_entries <- unlist(strsplit(input$ddg2p_disease_name_filter, ";"))
            # Trim leading and trailing whitespace from each entry
            input_entries <- trimws(input_entries)
            
            # Search the phenotypes column
            filtered_table1 <- ddg2p_data %>%
              filter(disease_name %in% input_entries)
            
          } else {
            # No text entered in the textarea, so keep the DataFrame as is
            filtered_table1 <- ddg2p_data
          }
        }
        
        if (input$ddg2p_allelic_requirement_na_switch == TRUE) {
          # filtered_table2 <- filtered_table1 %>%
          #   dplyr::filter(allelic_requirement %in% input$ddg2p_allelic_requirement_filter  | is.na(allelic_requirement))
          ddg2p_data2 <- filtered_table1 %>%
            dplyr::select(gene_symbol, allelic_requirement) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data3 <- filtered_table1 %>%
            dplyr::filter(allelic_requirement %in% input$ddg2p_allelic_requirement_filter | is.na(allelic_requirement)) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data4 <- intersect(ddg2p_data2, ddg2p_data3)
          
          filtered_table2 <- ddg2p_data %>%
            dplyr::filter(gene_symbol %in% ddg2p_data4)
        } else if (input$ddg2p_allelic_requirement_na_switch == FALSE) {
          # filtered_table2 <- filtered_table1 %>%
          #   dplyr::filter(allelic_requirement %in% input$ddg2p_allelic_requirement_filter  & !is.na(allelic_requirement))
          ddg2p_data2 <- filtered_table1 %>%
            dplyr::select(gene_symbol, allelic_requirement) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data3 <- filtered_table1 %>%
            dplyr::filter(allelic_requirement %in% input$ddg2p_allelic_requirement_filter & !is.na(allelic_requirement)) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data4 <- intersect(ddg2p_data2, ddg2p_data3)
          
          filtered_table2 <- ddg2p_data %>%
            dplyr::filter(gene_symbol %in% ddg2p_data4)
        }
        
        if (input$ddg2p_organ_specificity_na_switch == TRUE) {
          # filtered_table3 <- filtered_table2 %>%
          #   dplyr::filter(organ_specificity %in% input$ddg2p_organ_specificity_filter  | is.na(organ_specificity))
          ddg2p_data2 <- filtered_table2 %>%
            dplyr::select(gene_symbol, organ_specificity) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data3 <- filtered_table2 %>%
            dplyr::filter(organ_specificity %in% input$ddg2p_organ_specificity_filter | is.na(organ_specificity)) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data4 <- intersect(ddg2p_data2, ddg2p_data3)
          
          filtered_table3 <- ddg2p_data %>%
            dplyr::filter(gene_symbol %in% ddg2p_data4)
        } else if (input$ddg2p_organ_specificity_na_switch == FALSE) {
          # filtered_table3 <- filtered_table2 %>%
          #   dplyr::filter(organ_specificity %in% input$ddg2p_organ_specificity_filter  & !is.na(organ_specificity))
          ddg2p_data2 <- filtered_table2 %>%
            dplyr::select(gene_symbol, organ_specificity) %>%
            dplyr::distinct() %>%
            dplyr::count(gene_symbol) %>%
            dplyr::filter(n == 1) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data3 <- filtered_table2 %>%
            dplyr::filter(organ_specificity %in% input$ddg2p_organ_specificity_filter & !is.na(organ_specificity)) %>%
            dplyr::pull(gene_symbol)
          
          ddg2p_data4 <- intersect(ddg2p_data2, ddg2p_data3)
          
          filtered_table3 <- ddg2p_data %>%
            dplyr::filter(gene_symbol %in% ddg2p_data4)
        }
        
      } else if (input$ddg2p_annotations_checkbox == FALSE) {
        
        filtered_table3 <- NULL
        
      }
      
      filtered_table_ddg2p(filtered_table3)
    })
    
    
    # GENE CONSTRAINT filter----
    filtered_table_constraint <- shiny::reactiveVal()
    
    shiny::observe({
      
      if (input$constraint_annotations_checkbox == TRUE) {
        
        if (input$mean_score_all_na_switch == TRUE) {
          filtered_table1 <- constraint_data %>%
            dplyr::filter(mean_score_all >= input$mean_score_all_filter[1] &
                            mean_score_all <= input$mean_score_all_filter[2]  | is.na(mean_score_all))
        } else if (input$mean_score_all_na_switch == FALSE) {
          filtered_table1 <- constraint_data %>%
            dplyr::filter(mean_score_all >= input$mean_score_all_filter[1] &
                            mean_score_all <= input$mean_score_all_filter[2]  & !is.na(mean_score_all))
        }
        
        if (input$gnomad_lof_upper_90_ci_na_switch == TRUE) {
          filtered_table2 <- filtered_table1 %>%
            dplyr::filter(gnomad_lof_upper_90_ci >= input$gnomad_lof_upper_90_ci_filter[1] &
                            gnomad_lof_upper_90_ci <= input$gnomad_lof_upper_90_ci_filter[2]  | is.na(gnomad_lof_upper_90_ci))
        } else if (input$gnomad_lof_upper_90_ci_na_switch == FALSE) {
          filtered_table2 <- filtered_table1 %>%
            dplyr::filter(gnomad_lof_upper_90_ci >= input$gnomad_lof_upper_90_ci_filter[1] &
                            gnomad_lof_upper_90_ci <= input$gnomad_lof_upper_90_ci_filter[2]  & !is.na(gnomad_lof_upper_90_ci))
        }
        
        if (input$mean_am_pathogenicity_na_switch == TRUE) {
          filtered_table3 <- filtered_table2 %>%
            dplyr::filter(mean_am_pathogenicity >= input$mean_am_pathogenicity_filter[1] &
                            mean_am_pathogenicity <= input$mean_am_pathogenicity_filter[2]  | is.na(mean_am_pathogenicity))
        } else if (input$mean_am_pathogenicity_na_switch == FALSE) {
          filtered_table3 <- filtered_table2 %>%
            dplyr::filter(mean_am_pathogenicity >= input$mean_am_pathogenicity_filter[1] &
                            mean_am_pathogenicity <= input$mean_am_pathogenicity_filter[2]  & !is.na(mean_am_pathogenicity))
        }
        
        if (input$shet_rgcme_mean_na_switch == TRUE) {
          filtered_table4 <- filtered_table3 %>%
            dplyr::filter(shet_rgcme_mean >= input$shet_rgcme_mean_filter[1] &
                            shet_rgcme_mean <= input$shet_rgcme_mean_filter[2]  | is.na(shet_rgcme_mean))
        } else if (input$shet_rgcme_mean_na_switch == FALSE) {
          filtered_table4 <- filtered_table3 %>%
            dplyr::filter(shet_rgcme_mean >= input$shet_rgcme_mean_filter[1] &
                            shet_rgcme_mean <= input$shet_rgcme_mean_filter[2]  & !is.na(shet_rgcme_mean))
        }
        
        if (input$bf_lam_na_switch == TRUE) {
          filtered_table5 <- filtered_table4 %>%
            dplyr::filter(bf_lam >= input$bf_lam_filter[1] &
                            bf_lam <= input$bf_lam_filter[2]  | is.na(bf_lam))
        } else if (input$bf_lam_na_switch == FALSE) {
          filtered_table5 <- filtered_table4 %>%
            dplyr::filter(bf_lam >= input$bf_lam_filter[1] &
                            bf_lam <= input$bf_lam_filter[2]  & !is.na(bf_lam))
        }
        
        if (input$bf_mef_na_switch == TRUE) {
          filtered_table6 <- filtered_table5 %>%
            dplyr::filter(bf_mef >= input$bf_mef_filter[1] &
                            bf_mef <= input$bf_mef_filter[2]  | is.na(bf_mef))
        } else if (input$bf_mef_na_switch == FALSE) {
          filtered_table6 <- filtered_table5 %>%
            dplyr::filter(bf_mef >= input$bf_mef_filter[1] &
                            bf_mef <= input$bf_mef_filter[2]  & !is.na(bf_mef))
        }
        
        if (input$shet_post_mean_na_switch == TRUE) {
          filtered_table7 <- filtered_table6 %>%
            dplyr::filter(shet_post_mean >= input$shet_post_mean_filter[1] &
                            shet_post_mean <= input$shet_post_mean_filter[2]  | is.na(shet_post_mean))
        } else if (input$shet_post_mean_na_switch == FALSE) {
          filtered_table7 <- filtered_table6 %>%
            dplyr::filter(shet_post_mean >= input$shet_post_mean_filter[1] &
                            shet_post_mean <= input$shet_post_mean_filter[2]  & !is.na(shet_post_mean))
        }
        
        if (input$domino_na_switch == TRUE) {
          filtered_table8 <- filtered_table7 %>%
            dplyr::filter(domino >= input$domino_filter[1] &
                            domino <= input$domino_filter[2]  | is.na(domino))
        } else if (input$domino_na_switch == FALSE) {
          filtered_table8 <- filtered_table7 %>%
            dplyr::filter(domino >= input$domino_filter[1] &
                            domino <= input$domino_filter[2]  & !is.na(domino))
        }
        
        if (input$scones_na_switch == TRUE) {
          filtered_table9 <- filtered_table8 %>%
            dplyr::filter(scones >= input$scones_filter[1] &
                            scones <= input$scones_filter[2]  | is.na(scones))
        } else if (input$scones_na_switch == FALSE) {
          filtered_table9 <- filtered_table8 %>%
            dplyr::filter(scones >= input$scones_filter[1] &
                            scones <= input$scones_filter[2]  & !is.na(scones))
        }
        
      } else if (input$constraint_annotations_checkbox == FALSE) {
        
        filtered_table9 <- NULL
        
      }
      
      filtered_table_constraint(filtered_table9)
    })
    
    # Filtered genes list
    filtered_genes <- shiny::reactiveVal()
    
    shiny::observe({
      
      # IMPC
      if (!is.null(filtered_table_impc())) {
        filtered_genes_impc1 <- pcg_data %>%
          filter(gene_symbol %in% filtered_table_impc()$gene_symbol) %>%
          distinct()
      } else {
        filtered_genes_impc1 <- pcg_data
      }
      
      # MGI
      if (!is.null(filtered_table_mgi())) {
        filtered_genes_mgi1 <- pcg_data %>%
          filter(gene_symbol %in% filtered_table_mgi()$gene_symbol) %>%
          distinct()
      } else {
        filtered_genes_mgi1 <- pcg_data
      }
      
      # OMIM
      if (!is.null(filtered_table_omim())) {
        filtered_genes_omim1 <- pcg_data %>%
          filter(gene_symbol %in% filtered_table_omim()$gene_symbol) %>%
          dplyr::select(gene_symbol) %>%
          distinct()
      } else {
        filtered_genes_omim1 <- pcg_data
      }
      
      # DDG2P
      if (!is.null(filtered_table_ddg2p())) {
        filtered_genes_ddg2p1 <- pcg_data %>%
          filter(gene_symbol %in% filtered_table_ddg2p()$gene_symbol) %>%
          dplyr::select(gene_symbol) %>%
          distinct()
      } else {
        filtered_genes_ddg2p1 <- pcg_data
      }
      
      # GENE CONSTRAINT
      if (!is.null(filtered_table_constraint())) {
        filtered_genes_constraint1 <- pcg_data %>%
          filter(gene_symbol %in% filtered_table_constraint()$gene_symbol) %>%
          dplyr::select(gene_symbol) %>%
          distinct()
      } else {
        filtered_genes_constraint1 <- pcg_data
      }
      
      # GENE LIST
      if (nchar(input$gene_list_filter) > 0) {
        # Split the input by semicolon to handle multiple identifiers
        gene_identifiers <- unlist(strsplit(input$gene_list_filter, ";"))
        
        detect_gene_type <- function(identifiers) {
          for (identifier in identifiers) {
            if (grepl("^HGNC", identifier)) {
              return("hgnc_id")
            } else if (grepl("^ENSG", identifier)) {
              return("ensembl_gene_id")
            } else if (grepl("^[0-9]+$", identifier)) {
              return("entrez_id")
            } 
            # else if (grepl("^[A-Za-z][0-9]", identifier)) {
            #   return("uniprot_id")
            # }
          }
          # If none of the patterns match, assume gene symbol
          return("gene_symbol")
        }
        
        # Identify the type of gene identifier
        gene_type <- detect_gene_type(gene_identifiers)  # You need to implement this function
        
        # Filter pcg_data based on the identified gene type
        filtered_genes_genelist1 <- switch(gene_type,
                                           "gene_symbol"     = pcg_data %>% dplyr::filter(gene_symbol %in% gene_identifiers),
                                           "hgnc_id"         = pcg_data %>% dplyr::filter(hgnc_id %in% gene_identifiers),
                                           "ensembl_gene_id" = pcg_data %>% dplyr::filter(ensembl_gene_id %in% gene_identifiers),
                                           "entrez_id"       = pcg_data %>% dplyr::filter(entrez_id %in% gene_identifiers),
                                           "uniprot_id"      = pcg_data %>% dplyr::filter(uniprot_id %in% gene_identifiers),
                                           # Handle other cases or raise an error if gene type is not recognized
                                           stop("Unrecognized gene identifier type.")
        )
      } else {
        filtered_genes_genelist1 <- pcg_data
      }
      
      filtered_genes_overlap1 <- intersect(filtered_genes_impc1$gene_symbol, filtered_genes_omim1$gene_symbol)
      filtered_genes_overlap2 <- intersect(filtered_genes_overlap1, filtered_genes_constraint1$gene_symbol)
      filtered_genes_overlap3 <- intersect(filtered_genes_overlap2, filtered_genes_ddg2p1$gene_symbol)
      filtered_genes_overlap4 <- intersect(filtered_genes_overlap3, filtered_genes_mgi1$gene_symbol)
      filtered_genes_overlap5 <- intersect(filtered_genes_overlap4, filtered_genes_genelist1$gene_symbol)
      updateProgressBar(session = session, id = "progress_bar", value = length(filtered_genes_overlap5), total = nrow(pcg_data), 
                        title = "Number of genes after filtering")
      filtered_genes(filtered_genes_overlap5)
    })
    
    # Tables----
    output$gene_table <- renderReactable({
      # df <- as.data.frame(filtered_genes())
      # df <- setNames(df, "Current filtered gene list")
      df <- tbl_all %>%
        dplyr::filter(gene_symbol %in% filtered_genes())
      df <- setNames(df, c("Gene Symbol", "HGNC ID", "Mouse ortholog", "IMPC Viability", "DepMap mean gene effect", "gnomAD LOEUF"))
      reactable(
        df
      )
    })
    
    # Download handler
    output$downloadGenes <- downloadHandler(
      filename = function() {
        "gene_list.txt"
      },
      content = function(file) {
        df <- tbl_all %>%
          dplyr::filter(gene_symbol %in% filtered_genes())
        df <- setNames(df, c("Gene Symbol", "HGNC ID", "Mouse ortholog", "IMPC Viability", "DepMap mean gene effect", "gnomAD LOEUF")) 
        write.csv(df, file, quote = FALSE, row.names = FALSE)
      }
    )
 
    
    
    observe({
      # If add_gene_list_name is empty, disable the add_gene_list button
      if (nchar(trimws(input$add_gene_list_name)) == 0) {
        shinyjs::disable("add_gene_list")
      } else {
        shinyjs::enable("add_gene_list")
      }
    })
    
    saved_lists <- shiny::reactiveVal(list())
    gene_list_checker_info <- shiny::reactiveVal(list()) 
    # SAVED LISTS SERVER ----
    observeEvent(input$user_files_upload, {
      data <- filters_logic$userFilesUploadToList2(input$user_files_upload, pcg_data)
      lists <- data$genelist_names_list
      current_lists <- saved_lists()
      combined_lists <- c(current_lists, lists)
      saved_lists(combined_lists)
      
      gene_list_checker_info(data$other_data)
      print(data$other_data)
      })
    
    
    output$input_checker_output <- renderUI({
      # req(length(gene_list_checker_info()) > 0)
      # names <- names(gene_list_checker_info())
      # first_element <- names[[1]]
      # data <- gene_list_checker_info()
      # prev <- data[[first_element]][[1]]
      # alias <- data[[first_element]][[2]]
      # no_match <- data[[first_element]][[3]]
      tagList(
        fluidRow(
          textOutput(session$ns("title1"))
        ),
        fluidRow(
          reactableOutput(session$ns("prev_checker_tbl1")),
          reactableOutput(session$ns("alias_checker_tbl1")),
          textOutput(session$ns("no_match1"))
        ),
        fluidRow(
          textOutput(session$ns("title2"))
        ),
        fluidRow(
          reactableOutput(session$ns("prev_checker_tbl2")),
          reactableOutput(session$ns("alias_checker_tbl2")),
          textOutput(session$ns("no_match2"))
        ),
        fluidRow(
          textOutput(session$ns("title3"))
        ),
        fluidRow(
          reactableOutput(session$ns("prev_checker_tbl3")),
          reactableOutput(session$ns("alias_checker_tbl3")),
          textOutput(session$ns("no_match3"))
        ),
        fluidRow(
          textOutput(session$ns("title4"))
        ),
        fluidRow(
          reactableOutput(session$ns("prev_checker_tbl4")),
          reactableOutput(session$ns("alias_checker_tbl4")),
          textOutput(session$ns("no_match4"))
        )
      )
    })
    
    output$title1 <- renderText({
      req(length(names(gene_list_checker_info())) >= 1)
      names <- names(gene_list_checker_info())
      names[1]
    })
    output$title2 <- renderText({
      req(length(names(gene_list_checker_info())) >= 2)
      names <- names(gene_list_checker_info())
      names[2]
    })
    output$title3 <- renderText({
      req(length(names(gene_list_checker_info())) >= 3)
      names <- names(gene_list_checker_info())
      names[3]
    })
    output$title4 <- renderText({
      req(length(names(gene_list_checker_info())) >= 4)
      names <- names(gene_list_checker_info())
      names[4]
    })
    
    output$prev_checker_tbl1 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 1)
      data <- gene_list_checker_info()
      prev <- data[[1]][[1]]
      reactable(prev)
    })
    output$prev_checker_tbl2 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 2)
      data <- gene_list_checker_info()
      prev <- data[[2]][[1]]
      reactable(prev)
    })
    output$prev_checker_tbl3 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 3)
      data <- gene_list_checker_info()
      prev <- data[[3]][[1]]
      reactable(prev)
    })
    output$prev_checker_tbl4 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 4)
      data <- gene_list_checker_info()
      prev <- data[[4]][[1]]
      reactable(prev)
    })
    
    output$alias_checker_tbl1 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 1)
      data <- gene_list_checker_info()
      prev <- data[[1]][[2]]
      reactable(prev)
    })
    output$alias_checker_tbl2 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 2)
      data <- gene_list_checker_info()
      prev <- data[[2]][[2]]
      reactable(prev)
    })
    output$alias_checker_tbl3 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 3)
      data <- gene_list_checker_info()
      prev <- data[[3]][[2]]
      reactable(prev)
    })
    output$alias_checker_tbl4 <- renderReactable({
      req(length(names(gene_list_checker_info())) >= 4)
      data <- gene_list_checker_info()
      prev <- data[[4]][[2]]
      reactable(prev)
    })
    
    output$no_match1 <- renderText({
      req(length(names(gene_list_checker_info())) >= 1)
      data <- gene_list_checker_info()
      no_match <- data[[1]][[3]]
      no_match2 <- paste(no_match, collapse = ", ")
      paste("Genes not recognised:", no_match2)
    })
    output$no_match2 <- renderText({
      req(length(names(gene_list_checker_info())) >= 2)
      data <- gene_list_checker_info()
      no_match <- data[[2]][[3]]
      no_match2 <- paste(no_match, collapse = ", ")
      paste("Genes not recognised:", no_match2)
    })
    output$no_match3 <- renderText({
      req(length(names(gene_list_checker_info())) >= 3)
      data <- gene_list_checker_info()
      no_match <- data[[3]][[3]]
      no_match2 <- paste(no_match, collapse = ", ")
      paste("Genes not recognised:", no_match2)
    })
    output$no_match4 <- renderText({
      req(length(names(gene_list_checker_info())) >= 4)
      data <- gene_list_checker_info()
      no_match <- data[[4]][[3]]
      no_match2 <- paste(no_match, collapse = ", ")
      paste("Genes not recognised:", no_match2)
    })
  
    
    # Add gene list when button clicked
    observeEvent(input$add_gene_list, {
      list_name <- input$add_gene_list_name
      gene_list <- filtered_genes()  # Assuming filtered_genes() returns the gene list
      saved_lists_list <- saved_lists()  # Get the current value of saved_lists
      
      # Append the new gene list to saved_lists with the given name
      saved_lists_list[[list_name]] <- gene_list
      
      # Update the reactive value
      saved_lists(saved_lists_list)
      
      shinyjs::reset("add_gene_list_name")
    })
    
    observe({
      # If add_gene_list_name is empty, disable the add_gene_list button
      if (length(saved_lists()) == 0) {
        shinyjs::hide("clear_saved_lists")
      } else {
        shinyjs::show("clear_saved_lists")
      }
    })
    
    # Render list of gene lists as table
    output$saved_lists_table <- renderReactable({
      req(length(saved_lists()) > 0)
      saved_lists_data <- saved_lists()
      list_names <- names(saved_lists_data)
      list_lengths <- sapply(saved_lists_data, length)
      
      # Create a data frame containing list names and their lengths
      table_data <- data.frame(list_names = list_names, list_length = list_lengths,
                               row.names = NULL)
      table_data <- setNames(table_data, c("List name", "List length"))
      # Render the table using reactable
      reactable(table_data)
    })
    
    # Reset filters
    observeEvent(input$reset_filters, {
      shinyjs::reset("impc_annotations_checkbox")
      shinyjs::reset("impc_viability_filter")
      shinyjs::reset("impc_viability_na_switch")
      shinyjs::reset("impc_zygosity_filter")
      shinyjs::reset("impc_zygosity_na_switch")
      shinyjs::reset("impc_wol_filter")
      shinyjs::reset("impc_wol_na_switch")
      shinyjs::reset("impc_ortholog_filter")
      shinyjs::reset("impc_ortholog_na_switch")
      shinyjs::reset("impc_phenotypes_filter")
      shinyjs::reset("impc_phenotypes_checkbox")
      shinyjs::reset("impc_phenotypes_checkbox2")
      shinyjs::reset("mgi_annotations_checkbox")
      shinyjs::reset("mgi_viability_filter")
      shinyjs::reset("mgi_viability_na_switch")
      shinyjs::reset("omim_annotations_checkbox")
      shinyjs::reset("omim_lethality_filter")
      shinyjs::reset("omim_lethality_na_switch")
      shinyjs::reset("omim_earliest_lethality_filter")
      shinyjs::reset("omim_earliest_lethality_na_switch")
      shinyjs::reset("omim_number_filter")
      shinyjs::reset("omim_number_na_switch")
      shinyjs::reset("omim_moi_filter")
      shinyjs::reset("omim_moi_na_switch")
      shinyjs::reset("omim_phenotypes_filter")
      shinyjs::reset("omim_phenotypes_checkbox")
      shinyjs::reset("omim_phenotypes_checkbox2")
      shinyjs::reset("ddg2p_annotations_checkbox")
      shinyjs::reset("ddg2p_allelic_requirement_filter")
      shinyjs::reset("ddg2p_allelic_requirement_na_switch")
      shinyjs::reset("ddg2p_organ_specificity_filter")
      shinyjs::reset("ddg2p_organ_specificity_na_switch")
      shinyjs::reset("ddg2p_disease_name_filter")
      shinyjs::reset("ddg2p_disease_name_checkbox")
      shinyjs::reset("constraint_annotations_checkbox")
      shinyjs::reset("mean_score_all_filter")
      shinyjs::reset("mean_score_all_na_switch")
      shinyjs::reset("gnomad_lof_upper_90_ci_filter")
      shinyjs::reset("gnomad_lof_upper_90_ci_na_switch")
      shinyjs::reset("mean_am_pathogenicity_filter")
      shinyjs::reset("mean_am_pathogenicity_na_switch")
      shinyjs::reset("shet_rgcme_mean_filter")
      shinyjs::reset("shet_rgcme_mean_na_switch")
      shinyjs::reset("bf_lam_filter")
      shinyjs::reset("bf_lam_na_switch")
      shinyjs::reset("bf_mef_filter")
      shinyjs::reset("bf_mef_na_switch")
      shinyjs::reset("shet_post_mean_filter")
      shinyjs::reset("shet_post_mean_na_switch")
      shinyjs::reset("domino_filter")
      shinyjs::reset("domino_na_switch")
      shinyjs::reset("scones_filter")
      shinyjs::reset("scones_na_switch")
      shinyjs::reset("gene_list_filter")
      shinyjs::reset("user_files_upload")
    })
    
    # Reset list
    observeEvent(input$clear_saved_lists, {
      saved_lists(NULL)
      shinyjs::reset("user_files_upload")
    })
    
    # Pass lists to other modules
    list(
      gene_lists = reactive(saved_lists())
    )
    
  })
}
