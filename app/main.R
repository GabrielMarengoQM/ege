box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, renderText, textOutput, tagList,
        navbarPage, tabPanel, fluidRow, column, actionButton, p, observeEvent, updateTabsetPanel,
        tabsetPanel, updateNavbarPage],
  fst[read.fst],
  # reactable[reactable, reactableOutput, renderReactable],
  # shinyWidgets[pickerInput, progressBar, updateProgressBar, pickerOptions],
  # dplyr[...],
  # stats[na.omit],
  bslib[page_navbar, nav_panel, card, card_header, card_body]
  # shinydashboard[box],
  # htmltools[br]
)
# box::use(
#   shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, renderText, textOutput, tagList,
#         fluidRow, column, actionButton, observe, observeEvent, numericInput, sliderInput],
#   fst[read.fst],
#   reactable[reactable, reactableOutput, renderReactable],
#   shinyWidgets[pickerInput, progressBar, updateProgressBar, pickerOptions, materialSwitch, awesomeCheckbox,
#                multiInput, updateMultiInput],
#   dplyr[...],
#   stats[na.omit],
#   bslib[page_navbar, nav_panel, card, card_header, card_body],
#   shinydashboard[box],
#   htmltools[br],
#   stringr[str_detect],
#   tidyr[separate_rows]
# )

box::use(
  app/view[filters, home, plots]
)

pcg_data <- read.fst("./data/pcg.fst")
constraint_data <- read.fst("./data/constraint_metrics.fst")
impc_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/test_impc.fst")
mgi_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.viability.mgi.fst")
omim_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/omim_data.fst")
ddg2p_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/ddg2p.fst")
data_list <- list("impc_data" = impc_data, 
                  "mgi_data" = mgi_data, 
                  "omim_data" = omim_data, 
                  "ddg2p_data" = ddg2p_data, 
                  "constraint_data" = constraint_data,
                  "pcg_data" = pcg_data)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "Essential Genes Explorer",
    bg = "#C8A4FF",
    nav_panel(
      title = "Home",
      home$ui(ns("home_page"))
    ),
    nav_panel(
      title = "Filter",
      filters$ui(ns("filter_page"), data_list)
    ),
    nav_panel(
      title = "Plots",
      plots$ui(ns("plots_page"))
    ),
    nav_panel(
      title = "Enrichment analysis"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    home$server("home_page")
    
    filters_data <- filters$server("filter_page", data_list)
    plots$server("plots_page", filters_data, data_list)
  })
}
