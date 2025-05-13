load_all()
data(cancer, package = "survival")
cancer$age_na <- ifelse(1:nrow(cancer) <= 5, NA, cancer$age)
# interaction_p_value with NAs (ensure complete cases)
cancer_na <- cancer[!is.na(cancer$age_na), ] # Remove rows with NA in predictor
vectors=list(c("a"), c("a"))
merge_ordered_vectors <- function(vectors) {
  if (length(vectors) == 0) return(NULL)
  all_elements <- unique(unlist(vectors))

  # bubble sort all_elements based on the order of vectors
  n <- length(all_elements)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      order_result <- .calculate_order(all_elements[i], all_elements[j], vectors)
      if (order_result == -1) {
        temp <- all_elements[i]
        all_elements[i] <- all_elements[j]
        all_elements[j] <- temp
      }
    }
  }
  return(all_elements)
}
