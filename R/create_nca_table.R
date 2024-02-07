#' Create a table for NCA results
#' 
#' Confirms with layout of table commonly used in PK reports, i.e. where
#' stats are outputted in long format using a single column.
#' 
#' @param nca_data output data from NCA, generated using `run_nca()`
#' @param parameters vector of parameters to output, using upper case CDISC
#' standard names, e.g. `c("AUCALL", "CMAX", "TMAX")`. Defaults to output all
#' available parameters.
#' @param group name of variable in dataset to group statistics by, e.g. 
#' `"ACTARM"`
#' @param path optional, path to filename to save output table to.
#' @param format output as table in wide or long format.
#' @param description include description of parameter names into table?
#' 
#' @export
create_nca_table <- function(
    nca_data,
    parameters = NULL,
    group = NULL,
    path = NULL,
    format = "long",
    description = TRUE
) {
  format <- match.arg(format, c("long", "wide"))
  dict <- utils::read.csv(file = system.file(
    package = "irxadmiral", "md/data_dictionary_nca.csv"
  ))
  if(is.null(parameters)) {
    parameters <- dict$Object[dict$Type == "parameter"]
  }
  tmp <- nca_data %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(parameters)) %>%
    dplyr::group_by(.data$name, .add = TRUE)
  if(!is.null(group)) {
    tmp <- tmp %>%
      dplyr::group_by(!! rlang::sym(group), .add = TRUE)
  }

  ## capturing statistics
  nca_table <- tmp %>%
    dplyr::summarise(
      mean = mean(.data$value, na.rm = TRUE),
      sd = stats::sd(.data$value, na.rm = TRUE),
      cv_pct = 100*.data$sd/.data$mean,
      median = stats::median(.data$value, na.rm = TRUE),
      min = min(.data$value, na.rm = TRUE),
      max = max(.data$value, na.rm = TRUE)
    ) %>%
    dplyr::mutate(dplyr::across("mean":"max", ~ round(.x, 2) )) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::rename(Parameter = "name")
  
  ## grouping and formatting output
  if(format == "long") {
    nca_table <- nca_table %>%
      tidyr::pivot_longer(cols = c("mean":"max"))
    if(!is.null(group)) {
      nca_table <- nca_table %>%
        tidyr::pivot_wider(names_from = tidyselect::all_of(group))
    }
    nca_table <- nca_table %>%
      dplyr::rename(Statistic = "name") %>%
      dplyr::group_by(.data$Parameter)
  } 
  
  ## add description of parameters
  if(description) {
    nca_table <- merge(
      nca_table, 
      dict %>% dplyr::select("Object", "Description"),
      by.x = "Parameter", by.y = "Object"
    ) %>%
      dplyr::select("Parameter", "Description", !!names(.)) %>%
      dplyr::mutate(Description = ifelse(duplicated(.data$Description), "", .data$Description))
  }
  
  nca_table <- nca_table %>%
    dplyr::mutate(Parameter = ifelse(duplicated(.data$Parameter), "", .data$Parameter))
    
  ## save to file
  if(!is.null(path)) {
    utils::write.csv(
      nca_table, 
      file = path, 
      row.names = FALSE, 
      quote = FALSE
    )
  }
  
  nca_table
}
