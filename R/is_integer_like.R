#' Check if an object looks like an integer
#'
#' @description Take a vector and check to see if
#' it looks like it contains only integers.
#'
#' @param x a vector of values to check
#'
#' @return TRUE or FALSE indicating if all values
#' look like integers - if the vector is not of type
#' character or numeric it will always return FALSE
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' data <-
#'   tibble(
#'     x = c(1, 2, 3),
#'     y = c("4", "5", "6"),
#'     z = c("a", "b", "c")
#'   )
#' data %>%
#'   mutate(across(where(is_integer_like), as.integer))
is_integer_like <- function(x) {
  values <- unique(x)

  if (is.character(values)) {
    values <- suppressWarnings(as.numeric(values))

    if (all(is.na(values))) return(FALSE)

    return(rlang::is_integerish(values))
  } else {
    return(rlang::is_integerish(values))
  }
}

