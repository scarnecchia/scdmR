#' Read in a dataframe with paths to
#'
#' @param data a data.frame or [tibble()] produced by [get_scdm_paths()]
#' @param table name of the table you wish to import
#'
#' @return
#' @export
#'
#' @examples
read_scdm_table <-
  function(data,
           table = c("DTH",
                     "DEM",
                     "DIA",
                     "DIS",
                     "ENC",
                     "FAC",
                     "LAB",
                     "MIL",
                     "PRO",
                     "PVD",
                     "VIT",
                     "COD",
                     "PRE")) {
    data.type <- rlang::arg_match(table)

    .x <- dplyr::filter(data, table == table)
    .x <- dplyr::pull(.x, path)
    .x <- fs::path_norm(.x)

    .x <- read_dataset(.x)
    return(.x)


  }


#' Read in a SAS dataset and lowcase variable names
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
read_dataset <- function(data) {
  .x <- haven::read_sas(data)
  .x <-  tibble::as_tibble(.x)
  names(.x) <- tolower(names(.x))

  return(.x)
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
read_varnames <- function(.x) {
  .x <- haven::read_sas(.x, n_max=0)
  .x <-  tibble::as_tibble(.x)
  names(.x) <- tolower(names(.x))

  return(.x)
}

