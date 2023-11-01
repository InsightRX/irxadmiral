#' Create a standardized table for demographics information
#'
#' @param data list of data.frames with SDTM dataset, including at least DM
#' table. 
#' @param group name of variable in dataset to group statistics by, e.g. 
#' `"ACTARM"`
#' @param path optional, path to filename to save output table to.
#' 
#' @export
create_demographics_table <- function(
    data,
    group = NULL,
    path = NULL
) {
  
  ## get data not in DM table
  vitals <- data$vs %>% 
    stats::setNames(tolower(names(.))) %>%
    dplyr::filter(vstest %in% c("Weight", "Height")) %>%
    dplyr::group_by(usubjid) %>%
    dplyr::filter(!duplicated(vstest)) %>%
    dplyr::select(usubjid, name = vstest, value = vsstresc) %>%
    tidyr::pivot_wider() %>%
    stats::setNames(tolower(names(.)))
  
  ## merge into with DM table  
  demo <- data$dm %>%
    stats::setNames(tolower(names(.))) %>%
    merge(vitals) %>%
    dplyr::group_by(usubjid) %>%
    dplyr::slice(1) %>% # make sure only 1 row per patient
    dplyr::select(usubjid, age, sex, race, ethnic, actarm, weight, height) 
  
  ## summary statistics
  categorical <- c("sex", "race", "ethnic", "actarm")
  continuous <- c("age", "weight", "height")
  cont_data <- demo %>%
    dplyr::select(continuous) %>%
    dplyr::mutate(
      dplyr::across(!!continuous, ~ as.numeric(as.character(.x)))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = continuous) %>%
    dplyr::group_by(c(name, !!group)) %>%
    dplyr::summarise(
      mean = mean(value),
      sd = stats::sd(value),
      median = stats::median(value),
      min = min(value),
      max = max(value)
    ) %>%
    dplyr::mutate(
      dplyr::across(c(mean, sd, median, min, max), ~ as.character(round(.x, 1)))
    ) %>%
    dplyr::mutate(min_max = paste0(min, " - ", max)) %>%
    dplyr::select(-min, -max) %>%
    tidyr::pivot_longer(cols = c(mean, sd, median, min_max)) %>%
    stats::setNames(c("Demographic", "Statistic", "Value"))
  
  cat_data <- lapply(categorical, function(x) {
    demo %>%
      dplyr::ungroup() %>%
      dplyr::select(!!x) %>%
      table() %>%
      as.data.frame() %>%
      dplyr::mutate(Demographic = !!x) %>%
      stats::setNames(c("Statistic", "Value", "Demographic"))
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(Demographic, Statistic, Value) %>%
    dplyr::mutate(
      dplyr::across(!!names(.), ~ as.character(.x))
    )
  
  comb_data <- dplyr::bind_rows(
    cont_data,
    cat_data
  ) %>%
    dplyr::mutate(
      Demographic = ifelse(duplicated(Demographic), "", Demographic)
    ) %>%
    dplyr::rename("Value/count" = Value)
  
  if(!is.null(path)) {
    utils::write.csv(comb_data, file = path, row.names=F, quote=F)
  }
  
  comb_data
  
}
