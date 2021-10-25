#' Extend population Estimates
#'
#' @description Take a [tibble][tibble::tibble-package] containing population
#' estimates per year and extend this by copying the latest estimates `n`
#' times up to `max_pop_year`
#'
#' @param pops_df a [tibble][tibble::tibble-package]
#' containing population estimates
#' @param max_pop_year The latest year required
#' @param year_var The variable containing the year
#'
#' @return a [tibble][tibble::tibble-package] with extra years added
#' @importFrom rlang :=
#' @export
extend_pop_estimates <- function(pops_df, max_pop_year, year_var = "year") {

  # Get the current max pop available
  max_pop_available <- pops_df %>%
    dplyr::pull({{ year_var }}) %>%
    max()

  # Count how many years we are adding
  years_added <- 0

  # This will add on as many years as needed
  while (max(pops_df$year_var) < max_pop_year) {
    copied_pops <- pops_df %>%
      dplyr::filter({{ year_var }} == max(pops_df$year_var)) %>%
      dplyr::mutate({{ year_var }} := {{ year_var }} + 1)

    pops_df <- dplyr::bind_rows(pops_df, copied_pops)

    years_added <- years_added + 1
  }

  message(paste(
    "*** Latest population year available:",
    max_pop_available,
    "This was copied",
    years_added,
    "time(s), we now have populations up to",
    max_pop_year,
    "***"
  ))

  return(pops_df)
}
