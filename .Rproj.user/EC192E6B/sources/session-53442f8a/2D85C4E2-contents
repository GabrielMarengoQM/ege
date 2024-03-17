box::use(
  shiny[bootstrapPage, fluidRow, column, moduleServer, NS, renderUI, tags, uiOutput, textOutput, tagList, br, actionButton,
        h5, p],
  bslib[page_navbar, nav_panel, card, card_header, card_body],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    fluidRow(
      column(
        width = 12,
        align = "center",
        h5("Essential genes explorer is designed for two main purposes:"),
        p("1) Filtering gene lists (human protein-coding genes)"),
        p("2) Characterising gene lists based on metadata and metrics related to gene essentiality"),
        br(),
        h5("Data used by essential genes explorer:"),
        p("Mouse knockouts, Human diseases, Gene constraint metrics, Gene Ontologies, Pathways and Protein families"),
        br(),
        h5("How to use Essential genes explorer:"),
        p("Navigate to either the 'Filters' or 'Characterise & Compare' page"),
        p("Filters: refine an initial custom list of genes or the list of all protein coding genes"),
        p("Characterise & Compare: Upload a gene list and view/interact with the resulting plots")
      )
    ),
    
    fluidRow(
      column(
        width = 5,
        align = "center",  # Offset to push the first column to the right
        shiny::actionButton(
          ns("filter_button"),
          label = "Filter genes",
          style = "color: #FFF; background-color: #9C5CFF; border-color: #007bff;"
        )
      ),
      column(
        width = 5,
        align = "center",
        shiny::actionButton(
          ns("characterise_button"),
          label = "Characterise gene list and compare against other lists",
          style = "color: #FFF; background-color: #9C5CFF; border-color: #007bff;"
        )
      )
    ),
    
    
    fluidRow(
      column(
        width = 12,
        style = "text-align: center;",
        "Developed by Queen Mary University of London",
        br(),
        shiny::img(src = "static/qmul_logo.png", style = "height: 50px;")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
