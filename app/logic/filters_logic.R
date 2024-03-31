box::use(
  utils[read.csv, read.delim],
  tools[file_ext]
)

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