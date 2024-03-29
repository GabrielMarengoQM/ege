box::use(
  clusterProfiler[enrichGO],
  rrvgo[calculateSimMatrix, reduceSimMatrix, scatterPlot],
  stats[setNames, cmdscale, as.dist],
  ggplot2[...],
  dplyr[...],
  ReactomePA[enrichPathway],
  enrichplot[pairwise_termsim, emapplot]
)

# GO ----
# Add p q val args
#' @export
getEnrichedGoTerms <- function(gene_list, background, ontology, pval, qval) {
  
  go_analysis <- enrichGO(gene          = gene_list,
                          universe      = background,
                          keyType = "SYMBOL",
                          OrgDb         = "org.Hs.eg.db",
                          ont           = ontology,
                          pAdjustMethod = "BH",
                          pvalueCutoff  = pval,
                          qvalueCutoff  = qval,
                          readable      = TRUE)
}

# Add threshold and algorithm args
#' @export
generateGoSemanticSimilarityPlot <- function(go_analysis, ontology, percent_slice, threshold) {
  simMatrix <- calculateSimMatrix(go_analysis$ID,
                                  orgdb="org.Hs.eg.db",
                                  ont=ontology,
                                  method="Rel")
  
  scores <- setNames(-log10(go_analysis$qvalue), go_analysis$ID)
  reducedTerms <- reduceSimMatrix(simMatrix,
                                  scores,
                                  threshold=threshold,
                                  orgdb="org.Hs.eg.db")
  
  # Slice for top 10% of enriched GO terms to get clearer plots
  x <- 100/percent_slice
  top_x_percent_terms <- nrow(reducedTerms)/x
  
  # Generate plot
  scat_p <- scatterPlot(simMatrix, reducedTerms[1:top_x_percent_terms, ], algorithm = c("pca"), onlyParents = FALSE)
  
  # Add labels for top 5 most enriched GO terms
  x <- cmdscale(as.matrix(as.dist(1-simMatrix), eig=TRUE, k=2))
  
  df <- cbind(as.data.frame(x),
              reducedTerms[match(rownames(x), reducedTerms$go), c("term", "parent", "parentTerm", "size")])
  
  # Only show top 5 most significantly enriched terms
  p <- scat_p + geom_text(aes(label=parentTerm), data=subset(df[1:5,], parent == rownames(df[1:5,])), size=4, color="black")
  
  return(p)
}

#' @export
showLegendGO <- function(plot, show_legend) {
  if (show_legend == FALSE) {
    plot <- plot + theme(legend.position='none')
  } else {
    plot <- plot
  }
  
  return(plot)
}

#' @export
getEnirchedTermsTable <- function(enriched_terms) {
  df <- data.frame(enriched_terms, row.names = NULL) %>%
    dplyr::select(ID, Description, pvalue, qvalue)
  
  df <- setNames(df, c("GO ID", "GO Term", "p value", "q value"))
  
  return(df)
}

# Reactome ----
#' @export
reactomeEnrichment <- function(gene_list, background_set, pval, no_pathways_shown) {
  
  # Enrichment
  enriched_pathway <- background_set %>%
    dplyr::filter(gene_symbol %in% gene_list) %>%
    dplyr::pull(entrez_id) %>%
    enrichPathway(gene = ., pvalueCutoff = pval, readable = TRUE)

  # Table
  enriched_pathway_table <- data.frame(enriched_pathway, row.names = NULL) %>%
    dplyr::select(ID, Description, qvalue)

  # Plots
  edo <- pairwise_termsim(enriched_pathway)
  plot <- emapplot(edo, showCategory = no_pathways_shown)

  return(list(enriched_pathway_table = enriched_pathway_table, plot = plot))
}




