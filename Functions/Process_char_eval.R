#' Summarize Genesys Characterization/Evaluation datasets by Crop Strategy
#'
#' This function reads Excel files from a directory of crop-specific folders
#' and summarizes how many datasets and rows exist for each Crop_strategy.
#'
#' @param parent_dir Path to the top-level directory containing subfolders for each crop
#' @return A data frame summarizing dataset counts and accession rows by Crop_strategy
#' @examples
#' summarize_char_eval("G:/path/to/Characterization_and_evalution_datasets")
summarize_char_eval <- function(parent_dir) {
  folders <- list.dirs(parent_dir, recursive = FALSE)
  all_info <- list()
  
  for (folder in folders) {
    files <- list.files(folder, pattern = "\\.xlsx?$", full.names = TRUE)
    crop_name <- basename(folder)
    
    if (length(files) == 0) {
      all_info[[crop_name]] <- data.frame(
        Crop_strategy = crop_name,
        File = NA_character_,
        Rows = NA_integer_,
        stringsAsFactors = FALSE
      )
    } else {
      folder_info <- data.frame(
        Crop_strategy = crop_name,
        File = basename(files),
        Rows = sapply(files, function(file) {
          tryCatch({
            df <- readxl::read_excel(file)
            nrow(df)
          }, error = function(e) {
            warning(paste("Could not read", file, ":", e$message))
            NA_integer_
          })
        }),
        stringsAsFactors = FALSE
      )
      all_info[[crop_name]] <- folder_info
    }
  }
  
  if (length(all_info) == 0) {
    warning("No folders found or none contained readable files.")
    return(tibble::tibble(
      Crop_strategy = character(),
      char_eval_datasets_count = integer(),
      accessions_char_eval_datasets_count = integer()
    ))
  }
  
  final_df <- do.call(rbind, all_info)
  
  final_df %>%
    dplyr::group_by(Crop_strategy) %>%
    dplyr::summarize(
      char_eval_datasets_count = sum(!is.na(File)),
      accessions_char_eval_datasets_count = if (all(is.na(Rows))) NA_integer_ else sum(Rows, na.rm = TRUE),
      .groups = "drop"
    )
}