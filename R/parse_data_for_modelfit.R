#' Parse data into NONMEM-style dataset
#'
#' @description Parse data into modeling dataset (NONMEM-style format).
#'
#' @details
#'
#' This function uses code from the admiral package, which is distributed under
#' the following license:
#'
#' Apache License
#'
#' Version 2.0, January 2004
#' http://www.apache.org/licenses/
#'
#' ### Terms and Conditions for use, reproduction, and distribution
#'
#' #### 1. Definitions
#'
#' “License” shall mean the terms and conditions for use, reproduction, and
#' distribution as defined by Sections 1 through 9 of this document.
#'
#' “Licensor” shall mean the copyright owner or entity authorized by the copyright
#' owner that is granting the License.
#'
#' “Legal Entity” shall mean the union of the acting entity and all other entities
#' that control, are controlled by, or are under common control with that entity.
#' For the purposes of this definition, “control” means **(i)** the power, direct or
#' indirect, to cause the direction or management of such entity, whether by
#' contract or otherwise, or **(ii)** ownership of fifty percent (50%) or more of the
#' outstanding shares, or **(iii)** beneficial ownership of such entity.
#'
#' “You” (or “Your”) shall mean an individual or Legal Entity exercising
#' permissions granted by this License.
#'
#' “Source” form shall mean the preferred form for making modifications, including
#' but not limited to software source code, documentation source, and configuration
#' files.
#'
#' “Object” form shall mean any form resulting from mechanical transformation or
#' translation of a Source form, including but not limited to compiled object code,
#' generated documentation, and conversions to other media types.
#'
#' “Work” shall mean the work of authorship, whether in Source or Object form, made
#' available under the License, as indicated by a copyright notice that is included
#' in or attached to the work (an example is provided in the Appendix below).
#'
#' “Derivative Works” shall mean any work, whether in Source or Object form, that
#' is based on (or derived from) the Work and for which the editorial revisions,
#' annotations, elaborations, or other modifications represent, as a whole, an
#' original work of authorship. For the purposes of this License, Derivative Works
#' shall not include works that remain separable from, or merely link (or bind by
#' name) to the interfaces of, the Work and Derivative Works thereof.
#'
#' “Contribution” shall mean any work of authorship, including the original version
#' of the Work and any modifications or additions to that Work or Derivative Works
#' thereof, that is intentionally submitted to Licensor for inclusion in the Work
#' by the copyright owner or by an individual or Legal Entity authorized to submit
#' on behalf of the copyright owner. For the purposes of this definition,
#' “submitted” means any form of electronic, verbal, or written communication sent
#' to the Licensor or its representatives, including but not limited to
#' communication on electronic mailing lists, source code control systems, and
#' issue tracking systems that are managed by, or on behalf of, the Licensor for
#' the purpose of discussing and improving the Work, but excluding communication
#' that is conspicuously marked or otherwise designated in writing by the copyright
#' owner as “Not a Contribution.”
#'
#' “Contributor” shall mean Licensor and any individual or Legal Entity on behalf
#' of whom a Contribution has been received by Licensor and subsequently
#' incorporated within the Work.
#'
#' #### 2. Grant of Copyright License
#'
#' Subject to the terms and conditions of this License, each Contributor hereby
#' grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free,
#' irrevocable copyright license to reproduce, prepare Derivative Works of,
#' publicly display, publicly perform, sublicense, and distribute the Work and such
#' Derivative Works in Source or Object form.
#'
#' #### 3. Grant of Patent License
#'
#' Subject to the terms and conditions of this License, each Contributor hereby
#' grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free,
#' irrevocable (except as stated in this section) patent license to make, have
#' made, use, offer to sell, sell, import, and otherwise transfer the Work, where
#' such license applies only to those patent claims licensable by such Contributor
#' that are necessarily infringed by their Contribution(s) alone or by combination
#' of their Contribution(s) with the Work to which such Contribution(s) was
#' submitted. If You institute patent litigation against any entity (including a
#' cross-claim or counterclaim in a lawsuit) alleging that the Work or a
#' Contribution incorporated within the Work constitutes direct or contributory
#' patent infringement, then any patent licenses granted to You under this License
#' for that Work shall terminate as of the date such litigation is filed.
#'
#' #### 4. Redistribution
#'
#' You may reproduce and distribute copies of the Work or Derivative Works thereof
#' in any medium, with or without modifications, and in Source or Object form,
#' provided that You meet the following conditions:
#'
#' * **(a)** You must give any other recipients of the Work or Derivative Works a copy of
#' this License; and
#' * **(b)** You must cause any modified files to carry prominent notices stating that You
#' changed the files; and
#' * **(c)** You must retain, in the Source form of any Derivative Works that You distribute,
#' all copyright, patent, trademark, and attribution notices from the Source form
#' of the Work, excluding those notices that do not pertain to any part of the
#' Derivative Works; and
#' * **(d)** If the Work includes a “NOTICE” text file as part of its distribution, then any
#' Derivative Works that You distribute must include a readable copy of the
#' attribution notices contained within such NOTICE file, excluding those notices
#' that do not pertain to any part of the Derivative Works, in at least one of the
#' following places: within a NOTICE text file distributed as part of the
#' Derivative Works; within the Source form or documentation, if provided along
#' with the Derivative Works; or, within a display generated by the Derivative
#' Works, if and wherever such third-party notices normally appear. The contents of
#' the NOTICE file are for informational purposes only and do not modify the
#' License. You may add Your own attribution notices within Derivative Works that
#' You distribute, alongside or as an addendum to the NOTICE text from the Work,
#' provided that such additional attribution notices cannot be construed as
#' modifying the License.
#'
#' You may add Your own copyright statement to Your modifications and may provide
#' additional or different license terms and conditions for use, reproduction, or
#' distribution of Your modifications, or for any such Derivative Works as a whole,
#' provided Your use, reproduction, and distribution of the Work otherwise complies
#' with the conditions stated in this License.
#'
#' #### 5. Submission of Contributions
#'
#' Unless You explicitly state otherwise, any Contribution intentionally submitted
#' for inclusion in the Work by You to the Licensor shall be under the terms and
#' conditions of this License, without any additional terms or conditions.
#' Notwithstanding the above, nothing herein shall supersede or modify the terms of
#' any separate license agreement you may have executed with Licensor regarding
#' such Contributions.
#'
#' #### 6. Trademarks
#'
#' This License does not grant permission to use the trade names, trademarks,
#' service marks, or product names of the Licensor, except as required for
#' reasonable and customary use in describing the origin of the Work and
#' reproducing the content of the NOTICE file.
#'
#' #### 7. Disclaimer of Warranty
#'
#' Unless required by applicable law or agreed to in writing, Licensor provides the
#' Work (and each Contributor provides its Contributions) on an “AS IS” BASIS,
#' WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied,
#' including, without limitation, any warranties or conditions of TITLE,
#' NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE. You are
#' solely responsible for determining the appropriateness of using or
#' redistributing the Work and assume any risks associated with Your exercise of
#' permissions under this License.
#'
#' #### 8. Limitation of Liability
#'
#' In no event and under no legal theory, whether in tort (including negligence),
#' contract, or otherwise, unless required by applicable law (such as deliberate
#' and grossly negligent acts) or agreed to in writing, shall any Contributor be
#' liable to You for damages, including any direct, indirect, special, incidental,
#' or consequential damages of any character arising as a result of this License or
#' out of the use or inability to use the Work (including but not limited to
#' damages for loss of goodwill, work stoppage, computer failure or malfunction, or
#' any and all other commercial damages or losses), even if such Contributor has
#' been advised of the possibility of such damages.
#'
#' #### 9. Accepting Warranty or Additional Liability
#'
#' While redistributing the Work or Derivative Works thereof, You may choose to
#' offer, and charge a fee for, acceptance of support, warranty, indemnity, or
#' other liability obligations and/or rights consistent with this License. However,
#' in accepting such obligations, You may act only on Your own behalf and on Your
#' sole responsibility, not on behalf of any other Contributor, and only if You
#' agree to indemnify, defend, and hold each Contributor harmless for any liability
#' incurred by, or claims asserted against, such Contributor by reason of your
#' accepting any such warranty or additional liability.
#'
#' _END OF TERMS AND CONDITIONS_
#'
#' ### APPENDIX: How to apply the Apache License to your work
#'
#' To apply the Apache License to your work, attach the following boilerplate
#' notice, with the fields enclosed by brackets `[]` replaced with your own
#' identifying information. (Don't include the brackets!) The text should be
#' enclosed in the appropriate comment syntax for the file format. We also
#' recommend that a file or class name and description of purpose be included on
#' the same “printed page” as the copyright notice for easier identification within
#' third-party archives.
#'
#'     Copyright 2021 F. Hoffmann-La Roche AG and GlaxoSmithKline LLC
#'
#'     Licensed under the Apache License, Version 2.0 (the "License");
#'     you may not use this file except in compliance with the License.
#'     You may obtain a copy of the License at
#'
#'       http://www.apache.org/licenses/LICENSE-2.0
#'
#'     Unless required by applicable law or agreed to in writing, software
#'     distributed under the License is distributed on an "AS IS" BASIS,
#'     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#'     See the License for the specific language governing permissions and
#'     limitations under the License.
#'
#' 
#' @returns data.frame with population PK input data in NONMEM-style
#' format. It will also add the non-standard columns ROUTE ("oral", "iv") and 
#' FORM (formulation: "tablet", "suspension", "patch", "infusion", etc.) with 
#' values for each dose and NA for observations.
#' 
#' @param data list containing SDTM tables as data.frames
#' 
#' @export
#' 
parse_data_for_modelfit <- function(data) {

  ## Parse into modeling dataset (NONMEM-style format)
  ## For the modeling analysis we need the dosing history (in this example
  ## very simple, since just a single dose), and the concentration data.
  ## For covariate analyses we also need the covariates (e.g. weight, etc).
  ## We'll pull those together in a single data file.

  ## This code was adapted from an example on the admiral package website.
  ## It was made more generic, e.g. avoid hardcoded covariates as much as
  ## possible. Still needs some work to make fully generic.

  for(key in names(data)) { # admiral package is written assuming uppercase column names
    names(data[[key]]) <- toupper(names(data[[key]]))
  }
  
  param_lookup <- data.frame(
    PCTESTCD = c("DRUGX", "DOSE"),
    PARAMCD = c("DRUGX", "DOSE"),
    PARAM = c("concentration of DrugX", "DrugX Dose"),
    PARAMN = c(1, 2)
  )
  
  # Get list of ADSL vars required for derivations
  adsl_vars <- admiral::exprs(TRTSDT, TRTSDTM, TRT01P, TRT01A)
  
  ## Concentrations
  pc_dates <- data$pc %>%
    # Join ADSL with PC (need TRTSDT for ADY derivation)
    admiral::derive_vars_merged(
      dataset_add = data$adsl,
      new_vars = adsl_vars,
      by_vars = admiral::exprs(STUDYID, USUBJID)
    ) %>%
    # Derive analysis date/time
    # Impute missing time to 00:00:00
    admiral::derive_vars_dtm(
      new_vars_prefix = "A",
      dtc = PCDTC,
      time_imputation = "00:00:00"
    ) %>%
    # Derive dates and times from date/times
    admiral::derive_vars_dtm_to_dt(admiral::exprs(ADTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(ADTM)) %>%
    # Derive event ID and nominal relative time from first dose (NFRLT)
    dplyr::mutate(
      EVID = 0,
      DRUG = PCTEST,
      NFRLT = dplyr::if_else(PCTPTNUM < 0, 0, PCTPTNUM), .after = USUBJID
    )
  
  ## Doses
  ex_dates <- data$ex %>%
    admiral::derive_vars_merged(
      dataset_add = data$adsl,
      new_vars = adsl_vars,
      by_vars = admiral::exprs(STUDYID, USUBJID)
    ) %>%
    # Keep records with nonzero dose
    dplyr::filter(EXDOSE > 0) %>%
    # Add time and set missing end date to start date
    # Impute missing time to 00:00:00
    # Note all times are missing for dosing records in this example data
    # Derive Analysis Start and End Dates
    admiral::derive_vars_dtm(
      new_vars_prefix = "AST",
      dtc = EXSTDTC,
      time_imputation = "00:00:00"
    ) %>%
    admiral::derive_vars_dtm(
      new_vars_prefix = "AEN",
      dtc = EXENDTC,
      time_imputation = "00:00:00"
    ) %>%
    # Derive event ID and nominal relative time from first dose (NFRLT)
    dplyr::mutate(
      EVID = 1,
      NFRLT = 24 * (VISITDY - 1), .after = USUBJID
    ) %>%
    # Set missing end dates to start date
    dplyr::mutate(AENDTM = dplyr::case_when(
      is.na(AENDTM) ~ ASTDTM,
      TRUE ~ AENDTM
    )) %>%
    # Derive dates from date/times
    admiral::derive_vars_dtm_to_dt(admiral::exprs(ASTDTM)) %>%
    admiral::derive_vars_dtm_to_dt(admiral::exprs(AENDTM))
  
  ex_exp <- ex_dates %>%
    admiral::create_single_dose_dataset(
      dose_freq = EXDOSFRQ,
      start_date = ASTDT,
      start_datetime = ASTDTM,
      end_date = AENDT,
      end_datetime = AENDTM,
      nominal_time = NFRLT,
      lookup_table = admiral::dose_freq_lookup,
      lookup_column = CDISC_VALUE,
      keep_source_vars = admiral::exprs(
        STUDYID, USUBJID, EVID, EXDOSFRQ, EXDOSFRM,
        NFRLT, EXDOSE, EXDOSU, EXTRT, ASTDT, ASTDTM, AENDT, AENDTM,
        VISIT, VISITNUM, VISITDY, EXROUTE, EXDOSFRM,
        TRT01A, TRT01P, DOMAIN, EXSEQ, !!!adsl_vars
      )
    ) %>%
    # Derive AVISIT based on nominal relative time
    # Derive AVISITN to nominal time in whole days using integer division
    # Define AVISIT based on nominal day
    dplyr::mutate(
      AVISITN = NFRLT %/% 24 + 1,
      AVISIT = paste("Day", AVISITN),
      ADTM = ASTDTM,
      DRUG = EXTRT
    ) %>%
    # Derive dates and times from datetimes
    admiral::derive_vars_dtm_to_dt(admiral::exprs(ADTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(ADTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(ASTDTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(AENDTM)) %>%
    # TODO: line below is necessary, because create_single_dose_dataset() messes 
    #       up the actual nominal times. Should replace with custom function?
    dplyr::filter(AVISITN == 1)

  # ---- Find first dose per treatment per subject ----
  # ---- Join with ADPPK data and keep only subjects with dosing ----
  
  adppk_first_dose <- pc_dates %>%
    admiral::derive_vars_merged(
      dataset_add = ex_exp,
      filter_add = (!is.na(ADTM)),
      new_vars = admiral::exprs(
        FANLDTM = ADTM, 
        EXDOSE_first = EXDOSE
      ),
      order = admiral::exprs(ADTM, EXSEQ),
      mode = "first",
      by_vars = admiral::exprs(STUDYID, USUBJID, DRUG)
    ) %>%
    dplyr::filter(!is.na(FANLDTM)) %>%
    # Derive AVISIT based on nominal relative time
    # Derive AVISITN to nominal time in whole days using integer division
    # Define AVISIT based on nominal day
    dplyr::mutate(
      AVISITN = NFRLT %/% 24 + 1,
      AVISIT = paste("Day", AVISITN),
    )
  
  # ---- Find previous dose  ----
  adppk_prev <- adppk_first_dose %>%
    admiral::derive_vars_joined(
      dataset_add = ex_exp,
      by_vars = admiral::exprs(USUBJID),
      order = admiral::exprs(ADTM),
      new_vars = admiral::exprs(
        ADTM_prev = ADTM, 
        EXDOSE_prev = EXDOSE, 
        AVISIT_prev = AVISIT,
        AENDTM_prev = AENDTM
      ),
      join_vars = admiral::exprs(ADTM),
      filter_add = NULL,
      filter_join = ADTM > ADTM.join,
      mode = "last",
      check_type = "none"
    )
  
  # ---- Find previous nominal dose ----
  adppk_nom_prev <- adppk_prev %>%
    admiral::derive_vars_joined(
      dataset_add = ex_exp,
      by_vars = admiral::exprs(USUBJID),
      order = admiral::exprs(NFRLT),
      new_vars = admiral::exprs(NFRLT_prev = NFRLT),
      join_vars = admiral::exprs(NFRLT),
      filter_add = NULL,
      filter_join = NFRLT > NFRLT.join,
      mode = "last",
      check_type = "none"
    )
  
  # ---- Combine ADPPK and EX data ----
  # Derive Relative Time Variables
  adppk_aprlt <- dplyr::bind_rows(adppk_nom_prev, ex_exp) %>%
    dplyr::group_by(USUBJID, DRUG) %>%
    dplyr::mutate(
      FANLDTM = min(FANLDTM, na.rm = TRUE),
      min_NFRLT = min(NFRLT, na.rm = TRUE),
      maxdate = max(ADT[EVID == 0], na.rm = TRUE), .after = USUBJID
    ) %>%
    dplyr::arrange(USUBJID, ADTM) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ADT <= maxdate) %>%
    # Derive Actual Relative Time from First Dose (AFRLT)
    admiral::derive_vars_duration(
      new_var = AFRLT,
      start_date = FANLDTM,
      end_date = ADTM,
      out_unit = "hours",
      floor_in = FALSE,
      add_one = FALSE
    ) %>%
    # Derive Actual Relative Time from Reference Dose (APRLT)
    admiral::derive_vars_duration(
      new_var = APRLT,
      start_date = ADTM_prev,
      end_date = ADTM,
      out_unit = "hours",
      floor_in = FALSE,
      add_one = FALSE
    ) %>%
    # Derive APRLT
    dplyr::mutate(
      APRLT = dplyr::case_when(
        EVID == 1 ~ 0,
        is.na(APRLT) ~ AFRLT,
        TRUE ~ APRLT
      ),
      NPRLT = dplyr::case_when(
        EVID == 1 ~ 0,
        is.na(NFRLT_prev) ~ NFRLT - min_NFRLT,
        TRUE ~ NFRLT - NFRLT_prev
      )
    ) %>%
    dplyr::mutate(
      ROUTE = EXROUTE, 
      FORM = EXDOSFRM
    )
  
  # ---- Derive Analysis Variables ----
  # Derive actual dose DOSEA and planned dose DOSEP,
  # Derive AVAL and DV
  
  adppk_aval <- adppk_aprlt %>%
    dplyr::mutate(
      # Derive Actual Dose
      DOSEA = dplyr::case_when(
        EVID == 1 ~ EXDOSE,
        is.na(EXDOSE_prev) ~ EXDOSE_first,
        TRUE ~ EXDOSE_prev
      ),
      # Derive PARAMCD
      PARAMCD = dplyr::case_when(
        EVID == 1 ~ "DOSE",
        TRUE ~ PCTESTCD
      ),
      ALLOQ = PCLLOQ,
      # Derive CMT
      CMT = dplyr::case_when(
        EVID == 1 ~ 1,
        TRUE ~ 2
      ),
      # Derive BLQFL/BLQFN
      BLQFL = dplyr::case_when(
        PCSTRESC == "<BLQ" ~ "Y",
        TRUE ~ "N"
      ),
      BLQFN = dplyr::case_when(
        PCSTRESC == "<BLQ" ~ 1,
        TRUE ~ 0
      ),
      AMT = dplyr::case_when(
        EVID == 1 ~ EXDOSE,
        TRUE ~ NA_real_
      ),
      # Derive DV and AVAL
      DV = PCSTRESN,
      AVAL = DV,
      DVL = dplyr::case_when(
        DV != 0 ~ log(DV),
        TRUE ~ NA_real_
      ),
      # Derive MDV
      MDV = dplyr::case_when(
        EVID == 1 ~ 1,
        is.na(DV) ~ 1,
        TRUE ~ 0
      ),
      AVALU = dplyr::case_when(
        EVID == 1 ~ NA_character_,
        TRUE ~ PCSTRESU
      ),
      UDTC = lubridate::format_ISO8601(ADTM),
      II = 0, # TODO: original implementation was wrong
      SS = 0, # TODO: original implementation was wrong
    )
  
  # ---- Add ASEQ ----
  
  adppk_aseq <- adppk_aval %>%
    # Calculate ASEQ
    admiral::derive_var_obs_number(
      new_var = ASEQ,
      by_vars = admiral::exprs(STUDYID, USUBJID),
      order = admiral::exprs(AFRLT, EVID),
      check_type = "error"
    ) %>%
    # Derive PARAM and PARAMN
    admiral::derive_vars_merged(dataset_add = dplyr::select(param_lookup, -PCTESTCD), by_vars = admiral::exprs(PARAMCD)) %>%
    dplyr::mutate(
      PROJID = DRUG,
      PROJIDN = 1
    ) %>%
    # Remove temporary variables
    dplyr::select(
      -DOMAIN, -tidyselect::starts_with("min"), -tidyselect::starts_with("max"), -tidyselect::starts_with("EX"),
      -tidyselect::starts_with("PC"), -tidyselect::ends_with("first"), -tidyselect::ends_with("prev"),
      -tidyselect::ends_with("DTM"), -tidyselect::ends_with("DT"), -tidyselect::ends_with("TM"), -tidyselect::starts_with("VISIT"),
      -tidyselect::starts_with("AVISIT"), -tidyselect::starts_with("PARAM"),
      -tidyselect::ends_with("TMF"), -tidyselect::starts_with("TRT"), -tidyselect::starts_with("ATPT"), -DRUG
    )
  
  #---- Derive Covariates ----
  # Include numeric values for STUDYIDN, USUBJIDN, SEXN, RACEN etc.
  covar <- data$adsl %>%
    dplyr::mutate(
      STUDYIDN = as.numeric(stringr::word(USUBJID, 1, sep = stringr::fixed("-"))),
      SITEIDN = as.numeric(stringr::word(USUBJID, 2, sep = stringr::fixed("-"))),
      USUBJIDN = as.numeric(stringr::word(USUBJID, 3, sep = stringr::fixed("-"))),
      SUBJIDN = as.numeric(SUBJID),
      SEXN = dplyr::case_when(
        SEX == "M" ~ 1,
        SEX == "F" ~ 2,
        TRUE ~ 3
      )
    )
  categorical_vars <- c("RACE", "ETHNIC", "ARM", "ACTARM", "COUNTRY")
  covar[paste0(categorical_vars, "N")] <- data.matrix(covar[, categorical_vars])
  covar <- covar %>%
    dplyr::mutate(
      COHORT = ARMN,
      COHORTC = ARM
    ) %>%
    dplyr::select(
      STUDYID, STUDYIDN, SITEID, SITEIDN, USUBJID, USUBJIDN,
      SUBJID, SUBJIDN, AGE, SEX, SEXN, COHORT, COHORTC,
      RACE, RACEN, ETHNIC, ETHNICN, COUNTRY, COUNTRYN
    )
  
  #---- Derive additional baselines from VS and LB ----
  numeric_vars <- c("CREAT", "ALT", "AST", "BILI") ## TODO: fairly generic, but might not always be available or might include others
  labsbl <- data$lb %>%
    dplyr::filter(LBBLFL == "Y" & LBTESTCD %in% numeric_vars) %>%
    dplyr::mutate(LBTESTCDB = paste0(LBTESTCD, "BL")) %>%
    dplyr::select(STUDYID, USUBJID, LBTESTCDB, LBSTRESN)
  
  covar_vslb <- covar %>%
    admiral::derive_vars_merged(
      dataset_add = data$vs,
      filter_add = VSTESTCD == "HEIGHT",
      by_vars = admiral::exprs(STUDYID, USUBJID),
      new_vars = admiral::exprs(HTBL = VSSTRESN)
    ) %>%
    admiral::derive_vars_merged(
      dataset_add = data$vs,
      filter_add = VSTESTCD == "WEIGHT" & VSBLFL == "Y",
      by_vars = admiral::exprs(STUDYID, USUBJID),
      new_vars = admiral::exprs(WTBL = VSSTRESN)
    ) %>%
    admiral::derive_vars_transposed(
      dataset_merge = labsbl,
      by_vars = admiral::exprs(STUDYID, USUBJID),
      key_var = LBTESTCDB,
      value_var = LBSTRESN
    ) %>%
    dplyr::mutate(
      BMIBL = admiral::compute_bmi(height = HTBL, weight = WTBL),
      BSABL = admiral::compute_bsa(
        height = HTBL,
        weight = WTBL,
        method = "Mosteller"
      ),
      CRCLBL = admiral::compute_egfr(
        creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
        method = "CRCL"
      ),
      EGFRBL = admiral::compute_egfr(
        creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
        method = "CKD-EPI"
      )
    ) %>%
    dplyr::rename(TBILBL = BILIBL)
  
  # Combine covariates with APPPK data
  adppk <- adppk_aseq %>%
    admiral::derive_vars_merged(
      dataset_add = covar_vslb,
      by_vars = admiral::exprs(STUDYID, USUBJID)
    ) %>%
    dplyr::arrange(STUDYIDN, USUBJIDN, AFRLT, EVID) %>%
    dplyr::mutate(RECSEQ = dplyr::row_number()) %>%
    dplyr::mutate(ROUTE = tolower(ROUTE), FORM = tolower(FORM))

  poppk_data <- adppk %>% # select the variables we need from the data
    dplyr::select(
      ID = SUBJID, TIME = NFRLT, 
      DV, MDV, EVID, SS, II,
      AMT, SEXN, AGE, WT = WTBL, 
      ROUTE, FORM, 
      COHORT = COHORTC, SITEID,
      RACE, ETHNIC, COUNTRY
    ) %>% 
    dplyr::filter(!(is.na(DV) & EVID == 0)) # filter out DV=0 at time==0

  poppk_data
  
}

