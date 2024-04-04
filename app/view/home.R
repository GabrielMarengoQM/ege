box::use(
  shiny[bootstrapPage, fluidRow, column, moduleServer, NS, renderUI, tags, uiOutput, textOutput, tagList, br, actionButton,
        h5, h4, p, hr],
  bslib[page_navbar, nav_panel, card, card_header, card_body],
  htmltools[HTML]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    fluidRow(
      style = "text-align: center;",
      h4("Welcome to Essential Genes Explorer")
    ),
    br(),
    fluidRow(
      style = "text-align: center;",
      column(
        width = 6,
        h5("Background:"),
        p("The spectrum of gene essentiality plays a crucial role in the discovery of novel gene-disease associations. 
          Genes can be categorized based on their essentiality for an organism's survival or reproduction, ranging from indispensable to dispensable. 
          Understanding this spectrum allows us to prioritize candidate genes for disease association studies. 
          By integrating knowledge of gene essentiality with genomic and clinical data, we can identify novel gene-disease associations, facilitating targeted therapies and advancing our understanding of disease mechanisms.")
      ),
      column(
        width = 6,
        h5("Primary functionalities:"),
        p("1) Filter tab: Generate gene sets within or outside the intersections of gene essentiality data resources."),
        p("2) Plots tab: Characterise gene sets based on metadata and metrics related to gene essentiality."),
        p("3) Enrichment analysis tab: Perform GO or Pathway enrichment analysis of a gene set.")
      )
    ),
    hr(),
    fluidRow(
      style = "text-align: center;",
      column(
        width = 6,
        h5("Data resources:"),
        p(HTML("Mouse knockouts<br> Human diseases<br> Gene constraint metrics<br> Gene Ontologies<br> Pathways<br> Protein families (coming soon)"))
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 6,
            shiny::img(src = "static/impc.png", style = "height: 50px;")
          ),
          column(
            width = 6,
            shiny::img(src = "static/mgi.png", style = "height: 50px;")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shiny::img(src = "static/omim.png", style = "height: 50px;")
          ),
          column(
            width = 6,
            shiny::img(src = "static/g2p.png", style = "height: 50px;")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shiny::img(src = "static/gnomad.png", style = "height: 50px;")
          ),
          column(
            width = 6,
            shiny::img(src = "static/depmap.png", style = "height: 50px;")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shiny::img(src = "static/reactome.png", style = "height: 50px;")
          ),
          column(
            width = 6,
            shiny::img(src = "static/go.png", style = "height: 50px;")
          )
        )
      )
    ),
    hr(),
    fluidRow(
      style = "text-align: center;",
      h5("Relevant publications:"),
      p(
        HTML("<a href='https://doi.org/10.1007/s00335-023-09984-1' target='_blank'>Essential genes: a cross-species perspective.</a> Mamm Genome. 2023; doi:10.1007/s00335-023-09984-1. PMID: 3689735."
        )
      ),
      p(
        HTML("<a href='https://doi.org/10.1038/s41467-020-14284-2' target='_blank'>Human and mouse essentiality screens as a resource for disease gene discovery.</a> Nat Commun. 2020 Jan 31; doi:10.1038/s41467-020-14284-2. PMID: 32005800; PMCID: PMC6994715."
        )
      ),
      p(
        HTML("<a href='https://doi.org/10.1186/s13073-022-01118-7' target='_blank'>Mendelian gene identification through mouse embryo viability screening.</a> Genome Med. 2022 Oct 13;14(1):119. doi: 10.1186/s13073-022-01118-7. PMID: 36229886; PMCID: PMC9563108."
        )
      ),
      p(
        HTML("<a href='https://doi.org/10.1101/2024.01.12.24301168' target='_blank'>Lethal phenotypes in Mendelian disorders.</a> medRxiv. 2024 Jan 13:2024.01.12.24301168. doi: 10.1101/2024.01.12.24301168. PMID: 38260283; PMCID: PMC10802756."
        )
      )
    ),
    hr(),
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
