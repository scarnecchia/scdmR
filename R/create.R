#' Create a table with basic descriptive metadata
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
create_metadata <- function(data) {
    .x <- data
    .x$tmp <- purrr::map(data$path, read_varnames)
    .x$var_names <- purrr::map(.x$tmp, names)
    .x$var_labels <- purrr::map(.x$tmp, purrr::map, get_labels)
    .x$var_type <- purrr::map(.x$tmp, purrr::map, typeof)
    .x$var_class <- purrr::map(.x$tmp, purrr::map, class)
    .x$sas_format <- purrr::map(.x$tmp, purrr::map, get_sas_format)
    .x$file_size <- fs::file_size(data$path)
    .x <- dplyr::select(.x, -tmp)
    .x <- tidyr::unnest(.x, cols = c(var_names, var_labels, var_type, var_class, sas_format), keep_empty = TRUE)
    return(.x)
}
