box::use(
  purrr[map_dfr],
  dplyr[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, toWebGL],
  stats[as.formula]
)

#' @export
barChart <- function(gene_list, data_source, data_col, custom_x_axis_title) {
  combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
    list_name <- names(gene_list)[i]
    num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
    
    new_impc2 <- data_source %>%
      filter(gene_symbol %in% gene_list[[i]]) %>%
      count(!! sym(data_col)) %>%
      mutate(percentage = round(n/sum(n)*100, 2)) %>%
      mutate(name = list_name)
    
    print(new_impc2)
  })
  print(combined_dataframe)
  plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~percentage, color = ~name,
                  textposition = 'outside', text = ~percentage, type = "bar")

  plot <- plot %>%
    layout(
      xaxis = list(title = custom_x_axis_title),
      yaxis = list(title = "Percentage of genes"),
      showlegend = TRUE
    )
}

#' @export
renderViolinPlot <- function(raw_data, column, gene_lists, genes_to_highlight, threshold_value, toggle_option, custom_y_axis_title) {
  
  data <- do.call(rbind, lapply(names(gene_lists), function(gene_list_name) {
    genes <- gene_lists[[gene_list_name]]
    metrics_data <- raw_data[raw_data$gene_symbol %in% genes, c('gene_symbol', column)]
    metrics_data$gene_list_name <- gene_list_name
    return(metrics_data)
  }))
  
  if (toggle_option == TRUE) {
    points_setting <- "all"
  } else {
    points_setting <- "none"
  }
  
  # Your existing code for creating the violin plots
  violin_plot <- plot_ly(
    data,
    y = as.formula(paste("~", column)),
    x = ~gene_list_name,
    type = "violin",
    box = list(visible = T),
    points = points_setting,
    name = ~gene_list_name,
    text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
    hoverinfo = "text"  # Include gene symbol and metric value in hover text
  )
  
  # Add highlight points for individual genes
  if (length(genes_to_highlight > 0)) {
    violin_plot <- violin_plot %>%
      highlight("plotly_selecting") %>%
      # Add points for highlighting
      add_trace(
        data = data[data$gene_symbol %in% genes_to_highlight, ],  # Only include specified genes
        type = "scatter",
        mode = "markers",
        x = ~gene_list_name,
        y = as.formula(paste("~", column)),
        text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
        marker = list(color = "black", size = 10),
        hoverinfo = "text",
        name = "Searched Genes"  # Legend entry for the added trace
      )
  }
  
  # Remove x-axis title
  violin_plot <- violin_plot %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = custom_y_axis_title),
      showlegend = TRUE
    )
  
  # hline <- function(y = 0, color = "grey") {
  #   list(
  #     type = "line",
  #     x0 = 0,
  #     x1 = 1,
  #     xref = "paper",
  #     y0 = y,
  #     y1 = y,
  #     line = list(dash = "dash", color = color)
  #   )
  # }
  # 
  # if (!is.null(threshold_value)) {
  #   violin_plot <- violin_plot %>%
  #     layout(
  #       shapes = list(hline(threshold_value))
  #     )
  # }
  
  hline <- function(y = 0, color = "grey") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(dash = "dash", color = color)
    )
  }
  
  # Modified hline function to accept multiple threshold values
  hlines <- function(y = 0, color = "grey") {
    if (length(y) == 1) {
      return(list(hline(y, color)))
    } else {
      lines <- lapply(y, function(y_val) hline(y_val, color))
      return(lines)
    }
  }
  
  # Check if threshold_value is not NULL and is a vector
  if (!is.null(threshold_value) && is.vector(threshold_value)) {
    violin_plot <- violin_plot %>%
      layout(
        shapes = hlines(threshold_value)
      )
  }
  
  violin_plot %>% toWebGL()
  
  return(violin_plot)
}
