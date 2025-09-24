#' combine multiple data files into a single data frame
#'
#' @param path A string as the path to find the data files.
#' @param pattern A file pattern to filter the required data files.
#' @param add_file_name A logical value to indicate whether to add the file name as a column. Note that
#'   the added file name will affect the uniqueness of the data.
#' @param unique_only A logical value to indicate whether to remove the duplicated rows.
#' @param reader_fun A function to read the data files. Can be `read.csv`, `openxlsx::read.xlsx`, etc.
#' @param ... Other parameters passed to the `reader_fun`.
#'
#' @returns A data frame. If no data files found, return `NULL`.
#' @export
#' @examples
#' library(withr)
#' with_tempdir({
#'   write.csv(data.frame(x = 1:3, y = 4:6), "file1.csv", row.names = FALSE)
#'   write.csv(data.frame(x = 7:9, y = 10:12), "file2.csv", row.names = FALSE)
#'   dat <- combine_files(pattern = "file")
#' })
#' print(dat)
combine_files <- function(path = ".", pattern = NULL, add_file_name = FALSE, unique_only = TRUE,
                          reader_fun = read.csv, ...) {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (length(files) > 0) {
    file_list=list()
    for (f in files) {
      tmp <- reader_fun(f, ...)
      if (add_file_name) {
        base_col <- "origin_file"
        original_cols <- colnames(tmp)
        new_col_name <- base_col
        while (new_col_name %in% original_cols) {
          new_col_name <- paste0(new_col_name, "_")
        }
        tmp <- cbind(temp_col = basename(f), tmp)
        colnames(tmp)[1] <- new_col_name
      }
      file_list[[f]] <- tmp
    }
    dat = dplyr::bind_rows(file_list)
    if (unique_only) {
      dat <- unique(dat)
    }
    return(dat)
  } else {
    return(NULL)
  }
}
