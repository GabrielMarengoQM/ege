box::use(
  clusterProfiler[enrichGO],
  rrvgo[calculateSimMatrix, reduceSimMatrix, scatterPlot],
  stats[setNames, cmdscale, as.dist],
  ggplot2[...],
  dplyr[...],
  ReactomePA[enrichPathway],
  enrichplot[pairwise_termsim, emapplot],
  epitools[...]
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

# Ods ratio ----
#' @export
oddsRatioAnalysisDisease <- function(data, input_gene_list, pcg) {
  disease_genes <- data %>%
    filter(number_key == "The molecular basis for the disorder is known; a mutation has been found in the gene") %>%
    pull(gene_symbol) %>%
    unique()
  df <- pcg %>%
    select(gene_symbol) %>%
    mutate(selected = ifelse(gene_symbol %in% input_gene_list, "y", "n")) %>%
    mutate(disease_gene = ifelse(gene_symbol %in% disease_genes, "y", "n"))
  
  return(df)
}

oddsRatioAnalysisMouseKnockout <- function(data, input_gene_list, pcg) {
  lethal_genes <- data %>%
    filter(impc_viability == "lethal") %>%
    pull(gene_symbol) %>%
    unique()
  df <- pcg %>%
    select(gene_symbol) %>%
    mutate(selected = ifelse(gene_symbol %in% input_gene_list, "y", "n")) %>%
    mutate(disease_gene = ifelse(gene_symbol %in% lethal_genes, "y", "n"))
  
  return(df)
}

#' @export
oddsRatioPlot <- function(compare_list_option, my_list, pcg, dataset) {
  print(my_list)
  if (compare_list_option == "OMIM Disease genes") {
    compare_list <- dataset %>%
      dplyr::filter(number_key == "The molecular basis for the disorder is known; a mutation has been found in the gene") %>%
      dplyr::pull(gene_symbol) %>%
      unique()
  } else if (compare_list_option == "IMPC Lethal genes") {
    compare_list <- dataset %>%
      dplyr::filter(impc_viability == "lethal") %>%
      dplyr::pull(gene_symbol) %>%
      unique()
  } else if (compare_list_option == "DepMap Essential genes") {
    compare_list <- dataset %>%
      dplyr::filter(mean_score_all < -0.5) %>%
      dplyr::pull(gene_symbol) %>%
      unique()
  }
  
  print(compare_list)
  
  df <- pcg %>%
    select(gene_symbol) %>%
    mutate(selected = ifelse(gene_symbol %in% my_list, "y", "n")) %>%
    mutate(compared = ifelse(gene_symbol %in% compare_list, "y", "n"))
  
  print(df)
  
  contigency_table <- table(df$compared, df$selected)
  
  print(contigency_table)
  
  # Can't continue with incorrect contingency table
  # Check if the dimension is 2x1
  if (all(dim(contigency_table) == c(2, 1))) {
    # Add a new column 'y' with values 0, 0
    contigency_table <- cbind(contigency_table, y = c(0, 0))
    # Printing the updated table
    print(contigency_table)
  }
  
  or_all <- oddsratio(contigency_table, method = "wald")
  # change to DP not SF
  or_all_or <- signif(or_all$measure[2], 3)
  or_all_lower <- signif(or_all$measure[4], 3)
  or_all_upper <- signif(or_all$measure[6], 3)
  or_all_pvalue <- signif(or_all$p.value[4], 3)
  or_all_df <- data.frame(or = or_all_or,
                          or_lower = or_all_lower,
                          or_upper = or_all_upper,
                          pvalue = or_all_pvalue)
  print(or_all_df)
  return(or_all_df)
}

#oddsRatioPlot("OMIM Disease genes", pcg_data[1:10000, 3], pcg_data, omim_data)

