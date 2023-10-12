#' Parse data into dataset for NCA
#' 
#' @returns data.frame with data for IQNCA package
#' 
#' @param data list containing SDTM tables as data.frames
#' 
#' @export
#' 
parse_data_for_nca <- function(data) {
  
  ## Define metadata / settings for NCA
  ## These are things that cannot be read easily from EX dataset
  ## and are therefore hardcoded for now.

  route <- get_route_from_data(data$ex$exroute)
  md <- list(
    PROFTYPE = "SD", # single dose
    DAY = 1,
    TIMEUNIT = "hours",
    AUCMETHD = "Linear Log",
    ADM = ifelse(route == "iv", "INTRAVENOUS", "EXTRAVASCULAR"),
    ADUR = ifelse(route == "iv", NA, 0), # TODO: handle for IV drugs
    NDUR = ifelse(route == "iv", NA, 0)  # TODO: handle for IV drugs
  )
  
  ## Some cohorts we don't want to analyze in NCA, e.g. because they're not
  ## actually in the study or did not receive the drug.
  remove_cohorts <- c("Screen Failure", "Placebo")

  merged_data <- data$pc %>%
    setNames(toupper(names(.))) %>%
    merge(
      data$dm %>% # join with study-arm data
      setNames(toupper(names(.))) %>%
      dplyr::select(USUBJID, ACTARM), 
      by = "USUBJID"
    ) %>%
    merge(
      data$ex %>%  # join with dose administration data
      setNames(toupper(names(.))) %>%
      dplyr::select(USUBJID, EXDOSE, EXDOSU, EXROUTE, VISIT, VISITNUM),
      by = c("USUBJID", "VISITNUM")
    ) %>%
    dplyr::filter(!ACTARM %in% remove_cohorts) 
  arms <- unique(merged_data$ACTARM)
  
  nca_data <- merged_data %>%
    dplyr::mutate(
      ANALYTE = PCTEST,
      MATRIX = PCSPEC,
      COMPOUND = PCTEST,
      PROFILE = USUBJID,
      GROUP = ACTARM,
      GROUPN = match(ACTARM, arms),
      GROUPU = EXDOSU,
      ATIME = PCTPTNUM,
      NTIME = PCTPTNUM,
      ACONC = PCSTRESC,
      CONCUNIT = PCSTRESU,
      LLOQ = PCLLOQ,
      DOSE = EXDOSE,
      DOSEUNIT = EXDOSU,
      PROFTYPE = md$PROFTYPE, # single dose
      DAY = md$DAY,
      TIMEUNIT = md$TIMEUNIT,
      AUCMETHD = md$AUCMETHD,
      ADM = md$ADM,
      ADUR = md$ADUR,
      NDUR = md$NDUR
    )

  nca_data
}