#' Provide a path to a directory containing SCDM tables and create a dataset with paths to each file.
#'
#' @param path A path to a directory containing SCDM tables.
#' @param .table
#'
#' @return
#' @export
#'
#' @examples
get_scdm_paths <- function(path,
                           .table = NULL) {
  .path <- fs::path_norm(path)

  if (is.null(.table)) {
    .path <- fs::dir_ls(.path, type = "file", glob = "*.sas7bdat")
  } else {
    .path <- fs::path(.path, table)
    .path <- fs::dir_ls(.path, type = "file", glob = "*.sas7bdat")
  }

  .path <- tibble::as_tibble_col(.path, column_name="path")
  .path$table <- fs::path_file(.path$path)
  .path$table <- fs::path_ext_remove(.path$table)

  .path$table <- tolower(.path$table)

  .path <- dplyr::mutate(.path, table = dplyr::case_when(
    stringr::str_detect(.path$table, "death|dth") ~ "DTH",
    stringr::str_detect(.path$table, "demographic|dem") ~ "DEM",
    stringr::str_detect(.path$table, "diagnosis|dia") ~ "DIA",
    stringr::str_detect(.path$table, "dispensing|dis") ~ "DIS",
    stringr::str_detect(.path$table, "encounter|enc") ~ "ENC",
    stringr::str_detect(.path$table, "enrollment|enr") ~ "ENR",
    stringr::str_detect(.path$table, "facility|fac") ~ "FAC",
    stringr::str_detect(.path$table, "laboratory|lab") ~ "LAB",
    stringr::str_detect(.path$table, "mother|mil") ~ "MIL",
    stringr::str_detect(.path$table, "procedure|pro") ~ "PRO",
    stringr::str_detect(.path$table, "provider|pvd") ~ "PVD",
    TRUE ~ as.character(.path$table)
  )
  )

  return(.path)
}


get_labels <- purrr::attr_getter("label")
get_sas_format <- purrr::attr_getter("format.sas")
