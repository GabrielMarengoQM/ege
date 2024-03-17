box::use(
  plotly[plot_ly, layout]
)

#' @export
fromDollarToDualList <- function(data) {
  new_list <- list()
  for (i in seq_along(data)) {
    list <- data[[i]]
    name <- names(data)[[i]]
    
    new_list <- append(new_list, list(list(list, name)))
  }
  
  return(new_list)
}

x <- list("cat", "bob")
y <- list("dog", "mike")
z <- list(x, y)

gl <- as.list(c(pcg.m[1:100,3], pcg.m[100:200,3]))
gl
gl2 <- fromDollarToDualList(gl)
gl2[[2]][[2]] <- "gene_symbol2"
for (i in gl2) {
  print(i[[2]])
}
x <- generateImpcBarchart(gl2, impc_data)
x

#' @export
generateImpcBarchart <- function(gene_lists, data) {
  
  # INPUT:
  #  list of lists where i[[1]] is data and i[[2]] is associated name
  
  # PROCESS:
  # -Take human genes, get mouse genes
  # -Create a dataframe
  # -Generate plot
  
  #gene_lists <- fromDollarToDualList(gene_lists)
  
  gene_lists <- gl2
  data <- impc_data
  
  mouse_data_list <- list()
  for (i in gene_lists) {
    # get mgi ids from genes
    mgi_ids <- pcg.m %>%
      dplyr::filter(gene_symbol %in% i[[1]]) %>%
      dplyr::select(mgi_id) %>%
      dplyr::distinct()
    print(mgi_ids)
    
    # Get genes and viability score
    impc_data2 <- data[data$mgi_id %in% mgi_ids$mgi_id, c('mgi_id', 'impc_viability')]
    impc_plot_data <- impc_data2 %>%
      dplyr::filter(mgi_id != "NA") %>%
      dplyr::filter(impc_viability != "NA") %>%
      dplyr::mutate(impc_viability_2 = ifelse(!impc_viability %in% c("lethal","subviable","viable"),
                                              "conflicting", impc_viability)) %>%
      dplyr::group_by(impc_viability_2) %>%
      dplyr::tally() %>%
      dplyr::mutate(impc_viability_3 = factor(impc_viability_2,
                                              levels = c("lethal","subviable","viable"))) %>%
      dplyr::mutate(percentage = (n/sum(n)*100)) %>%
      dplyr::mutate(list_name = i[[2]])
    print(impc_plot_data)
    # Remove conflicting rows
    impc_plot_data <- impc_plot_data[impc_plot_data$impc_viability_2 != "conflicting", ]
    
    # Round the numeric columns to 3 decimal places
    impc_plot_data <- impc_plot_data %>%
      dplyr::mutate_at(vars('percentage'), list(~ round(., 3)))
    
    if (dim(impc_plot_data)[1] != 3) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c('viable', 'subviable', 'lethal')
      current_rows <- impc_plot_data$impc_viability_3
      missing_rows <- levels[!levels %in% current_rows]
      
      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(impc_viability_3 = missing_rows, n = 0, percentage = 0)
      
      # Update impc_plot_data with the missing rows
      impc_plot_data <- bind_rows(impc_plot_data, missing_data)
    }
    
    mouse_data_list <- c(mouse_data_list, list(impc_plot_data))
  }
  # Combine data frames vertically
  combined_df <- bind_rows(mouse_data_list)
  
  plot <- plot_ly(combined_df, x = ~impc_viability_3, y = ~percentage, color = ~list_name,
                  textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
  
  return(plot)
}

# Input for plots
df <- data.frame(name = c("list1", "list2"), datapath = c("/Users/gabrielm/Desktop/gene_lists_for_app_demo/gene_list_sample_1.tsv",
                                                          "/Users/gabrielm/Desktop/gene_lists_for_app_demo/gene_list_sample_2.tsv"))

#' @export
userFilesUploadToList <- function(files_data, header_option) {
  genelist_names_list <- list()
  for (i in 1:nrow(files_data)) {
    file_path <- files_data[i, "datapath"]
    file_extension <- tools::file_ext(file_path)
    
    if (file_extension == "csv") {
      file_data <- read.csv(file_path, header = header_option)
    } else if (file_extension %in% c("tsv", "tab")) {
      file_data <- read.delim(file_path, header = header_option)
    } else {
      stop(paste("Unsupported file format for file:", file_path))
    }
    
    # Remove file extension from the name
    name_without_extension <- sub(sprintf("\\.%s$", file_extension), "", files_data[i, "name"])
    
    genelist_names_list[[name_without_extension]] <- file_data[, 1]
  }
  
  return(genelist_names_list)
  print(genelist_names_list)
}

data <- userFilesUploadToList(df, TRUE)
typeof(data[1])

userFilesUploadToList_vec <- function(files_data, header_option) {
  genelist_names_list <- lapply(files_data$datapath, function(file_path) {
    file_extension <- tools::file_ext(file_path)
    
    if (file_extension == "csv") {
      file_data <- read.csv(file_path, header = header_option)
    } else if (file_extension %in% c("tsv", "tab")) {
      file_data <- read.delim(file_path, header = header_option)
    } else {
      stop(paste("Unsupported file format for file:", file_path))
    }
    
    # Extract name without extension
    name_without_extension <- sub(sprintf("\\.%s$", file_extension), "", basename(file_path))
    
    return(file_data[, 1])
  })
  
  names(genelist_names_list) <- sub("\\..*", "", files_data$name)
  
  return(genelist_names_list)
}

data_from_vec <- userFilesUploadToList_vec(df, TRUE)

#######################
######### 2.0 ######### ----
#######################
new_impc <- read.fst("./new_impc.fst")

# first filter to only have genes in gene list + get number of genes not in list and use mutate to add this row
# e.g. 
input <- data_from_vec
impc_viability_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.viability.impc.fst")

combined_dataframe <- NULL
for (i in seq_along(input)) {
  list_name <- names(input)[i]
  num <- length(setdiff(input[[i]], impc_viability_data$gene_symbol))
  new_impc2 <- impc_viability_data %>%
    filter(gene_symbol %in% input[[i]]) %>%
    count(impc_viability) %>%
    add_row(impc_viability = "no viability data", n = num) %>%
    mutate(name = list_name) %>%
    mutate(percentage = round(n/sum(n)*100, 2))
  
  combined_dataframe <- bind_rows(combined_dataframe, new_impc2)
}

print(combined_dataframe)

plot <- plot_ly(combined_dataframe, x = ~impc_viability, y = ~percentage, color = ~name,
                textposition = 'outside', text = ~percentage)
plot


# DATA
impc_viability <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.viability.impc.fst")
impc_phenotypes <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.phenotypes.impc.fst")
wol_data <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/wol.categories.fst")
orthologs <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.orthologs.fst")
mgi_viability <- read.fst("/Users/gabrielm/Desktop/gene_annotations/data/processed/mouse.viability.mgi.fst")
# TEST LIST INPUTS
input <- data_from_vec
input <- list("l1" = "C17orf49", "l2" = "C6orf118")
### vectorised
# library(purrr)
# IMPC VIABILITY BAR CHART ----
combined_dataframe <- map_dfr(seq_along(input), function(i) {
  list_name <- names(input)[i]
  num <- length(setdiff(input[[i]], impc_viability_data$gene_symbol))
  
  new_impc2 <- impc_viability_data %>%
    filter(gene_symbol %in% input[[i]]) %>%
    count(impc_viability) %>%
    mutate(percentage = round(n/sum(n)*100, 2)) %>%
    mutate(name = list_name)
})
print(combined_dataframe)
plot <- plot_ly(combined_dataframe, x = ~impc_viability, y = ~percentage, color = ~name,
                textposition = 'outside', text = ~percentage)
plot

# WOL ----
combined_dataframe <- map_dfr(seq_along(input), function(i) {
  list_name <- names(input)[i]
  num <- length(setdiff(input[[i]], wol_data$gene_symbol))
  
  new_impc3 <- wol_data %>%
    filter(gene_symbol %in% input[[i]]) %>%
    count(wol) %>%
    mutate(percentage = round(n/sum(n)*100, 2)) %>%
    mutate(name = list_name)
})
print(combined_dataframe)
# Percentage
plot <- plot_ly(combined_dataframe, x = ~wol, y = ~percentage, color = ~name,
                textposition = 'outside', text = ~percentage)
plot
# Counts
plot <- plot_ly(combined_dataframe, x = ~wol, y = ~n, color = ~name,
                textposition = 'outside', text = ~n)
plot

# ORTHOLOGS ----
combined_dataframe <- map_dfr(seq_along(input), function(i) {
  list_name <- names(input)[i]
  num <- length(setdiff(input[[i]], orthologs$gene_symbol))
  
  new_impc4 <- orthologs %>%
    filter(gene_symbol %in% input[[i]]) %>%
    count(ortholog_mapping) %>%
    mutate(percentage = round(n/sum(n)*100, 2)) %>%
    mutate(name = list_name)
})
print(combined_dataframe)
# Percentage
plot <- plot_ly(combined_dataframe, x = ~ortholog_mapping, y = ~percentage, color = ~name,
                textposition = 'outside', text = ~percentage)
plot
# Counts
plot <- plot_ly(combined_dataframe, x = ~ortholog_mapping, y = ~n, color = ~name,
                textposition = 'outside', text = ~n)
plot

# MGI VIABILITY ----
combined_dataframe <- map_dfr(seq_along(input), function(i) {
  list_name <- names(input)[i]
  num <- length(setdiff(input[[i]], mgi_viability$gene_symbol))
  
  new_impc4 <- mgi_viability %>%
    filter(gene_symbol %in% input[[i]]) %>%
    count(mp_term_lethal) %>%
    mutate(percentage = round(n/sum(n)*100, 2)) %>%
    mutate(name = list_name)
})
print(combined_dataframe)
# Percentage
plot <- plot_ly(combined_dataframe, x = ~mp_term_lethal, y = ~percentage, color = ~name,
                textposition = 'outside', text = ~percentage)
plot
# Counts
plot <- plot_ly(combined_dataframe, x = ~mp_term_lethal, y = ~n, color = ~name,
                textposition = 'outside', text = ~n)
plot

# benchmark
install.packages("microbenchmark")
library(microbenchmark)

#' @export
generateImpcBarchart <- function(gene_lists, data) {

  mouse_data_list <- list()
  for (i in gene_lists) {
    # get mgi ids from genes
    mgi_ids <- pcg.m %>%
      dplyr::filter(gene_symbol %in% i[[1]]) %>%
      dplyr::select(mgi_id) %>%
      dplyr::distinct()
    print(mgi_ids)
    
    # Get genes and viability score
    impc_data2 <- data[data$mgi_id %in% mgi_ids$mgi_id, c('mgi_id', 'impc_viability')]
    impc_plot_data <- impc_data2 %>%
      dplyr::filter(mgi_id != "NA") %>%
      dplyr::filter(impc_viability != "NA") %>%
      dplyr::mutate(impc_viability_2 = ifelse(!impc_viability %in% c("lethal","subviable","viable"),
                                              "conflicting", impc_viability)) %>%
      dplyr::group_by(impc_viability_2) %>%
      dplyr::tally() %>%
      dplyr::mutate(impc_viability_3 = factor(impc_viability_2,
                                              levels = c("lethal","subviable","viable"))) %>%
      dplyr::mutate(percentage = (n/sum(n)*100)) %>%
      dplyr::mutate(list_name = i[[2]])
    print(impc_plot_data)
    # Remove conflicting rows
    impc_plot_data <- impc_plot_data[impc_plot_data$impc_viability_2 != "conflicting", ]
    
    # Round the numeric columns to 3 decimal places
    impc_plot_data <- impc_plot_data %>%
      dplyr::mutate_at(vars('percentage'), list(~ round(., 3)))
    
    if (dim(impc_plot_data)[1] != 3) {
      # if true then one category has 0 genes and needs to be filled
      levels <- c('viable', 'subviable', 'lethal')
      current_rows <- impc_plot_data$impc_viability_3
      missing_rows <- levels[!levels %in% current_rows]
      
      # Add missing rows with a value of 0 for both 'n' and 'percentage'
      missing_data <- data.frame(impc_viability_3 = missing_rows, n = 0, percentage = 0)
      
      # Update impc_plot_data with the missing rows
      impc_plot_data <- bind_rows(impc_plot_data, missing_data)
    }
    
    mouse_data_list <- c(mouse_data_list, list(impc_plot_data))
  }
  # Combine data frames vertically
  combined_df <- bind_rows(mouse_data_list)
  
  plot <- plot_ly(combined_df, x = ~impc_viability_3, y = ~percentage, color = ~list_name,
                  textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
  
  return(plot)
}
