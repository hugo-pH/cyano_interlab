#' Correct by background signal in plate-reader data
#' function  to extract the background measurements from each plate and 
#' subtract it from the samples' measurements
#' 
#' @param df A tibble containing the processed plate-reader data
#' @param .var The plate-reader measurement (OD_730 or fl)
#' @param column_suffix The suffix to add to the background corrected column
#' @param dilutions A tibble containing the dilutions
#' @param correct_negative A logical value indicating whether to correct negative values by
#' adding the absolute value of the most negative value to all data points
#'
#' @return A tibble with same data as input plus an extra column containing the 
#' background corrected data
background_correction <- function(df, .var, column_suffix, dilutions, correct_negative){

  var <- enquo(.var)
  blank_var_nm <- paste0("blank_", as_label(var))
  var_corrected_nm <- paste(as_label(var), column_suffix, sep = "_")
  
  # get the mean background value for each timepoint and experiment
  df.blank <- df |>
    filter(!is.na({{.var}}), sample_type == "blank") |>
    group_by(location, experiment_id, time_h) |>
    summarise(
      !!blank_var_nm := mean({{.var}}, na.rm = T)
    ) |>
    ungroup()
  # join the summarized background values to the measurements and subtract it from them
  df.measurements <- df |>
    left_join(dilutions, by = join_by(location == location_experiment,
                                      experiment_date)) |> 
    filter(!is.na({{.var}}), sample_type == "sample") |>
    left_join(df.blank, by = c("location", "experiment_id", "time_h")) |>
    mutate(
      # subtract blank
      !!var_corrected_nm := ifelse(time_h == max(time_h),
                                   # if timepoint is last timepoint, subtract blank multiplied by the dilution factor
                                   {{.var}} - (.data[[blank_var_nm]] * dilution_factor),
                                   {{.var}} - .data[[blank_var_nm]]
      )
    ) |> 
    select(-dilution_factor) 
  if(correct_negative){
    # Remove negative values after blanking by adding the most negative value for each location and experiment
    df.measurements <- df.measurements |> 
      group_by(location, experiment_date) |>
      mutate(
        !!var_corrected_nm :=  .data[[var_corrected_nm]] + abs(min( .data[[var_corrected_nm]],
                                                                    na.rm = T))
      ) |>
      ungroup()
  }
  return(df.measurements)
}

#' Calculate relative fluorescence units (rfu) from plate-reader data
#'
#' @param df.od A tibble containing the processed plate-reader OD data
#' @param df.fl A tibble containing the processed plate-reader fluorescence data
#' @param od_col The name of the OD column
#' @param fl_col The name of the fluorescence column
#' @param background_correction Logical value indicating whether to correct for background signal
#' @param dilutions A tibble containing the dilutions
#' @param bc_suffix The suffix to add to the background corrected column
#' @param correct_neg_od A logical value indicating whether to correct negative values in the OD data
#' @param correct_neg_fl A logical value indicating whether to correct negative values in the fluorescence data
#'
#' @return A tibble with same data as input plus a column containing rfu (fl_od) and the 
#' background corrected data is background_correction = T
calculate_rfu <- function(df.od, df.fl, od_col = OD_730, fl_col = fl, 
                          background_correction = T, dilutions, bc_suffix = "bc",
                          correct_neg_od = F, correct_neg_fl = T){
  if (background_correction) {
    df.od <-  background_correction(df = df.od, .var = {{od_col}}, 
                                    column_suffix = bc_suffix, 
                                    dilutions = dilutions,
                                    correct_negative = correct_neg_od)
    df.fl <-  background_correction(df = df.fl, .var = {{fl_col}}, 
                                    column_suffix = bc_suffix, 
                                    dilutions = dilutions,
                                    correct_negative = correct_neg_fl)
    
    od_col <- str_subset(colnames(df.od), bc_suffix) |> as.name()
    fl_col <- str_subset(colnames(df.fl), bc_suffix) |> as.name()
  } 
  
  left_join(
    df.od,
    df.fl, 
    by = c("strain", "induction", "location", "tech_replicate", 
           "bio_replicate", "experiment_date", "experiment_id", "time_h", "sample_type")
  ) |>
    mutate(
      fl_od = {{fl_col}} / {{od_col}}
    )
}



#' Function to normalize rfu by a strain and induction
#'
#' @param df A tibble containing plate-reader data with rfu
#' @param .var The column where rfu is stored 
#' @param ref_strain The reference strain to normalize by
#' @param ref_induction The reference induction regime to normalize by
#' @param column_suffix The suffix to add to the column containing the normalized data
#'
#' @return A tibble with same columns as input plus a new column
#' containing the normalized by reference data
strain_normalization <- function(df, .var, ref_strain, 
                                 ref_induction, column_suffix = "norm"){
  # browser()
  var <- enquo(.var)
  strain_var_nm <- paste(ref_strain, as_label(var), sep = "_")
  var_corrected_nm <- paste(as_label(var), column_suffix, sep = "_")
  
  # get the mean value of the reference strain and induction regime for each timepoint and experiment
  df.ref.strain <- df |>
    filter(!is.na({{.var}}), strain == ref_strain, induction == ref_induction) |>
    group_by(location, bio_replicate, experiment_date, experiment_id, time_h) |>
    summarise(
      !!strain_var_nm := mean({{.var}}, na.rm = T)
    ) |>
    ungroup()
  
  # join the summarized reference values to the measurements and normalize
  df.measurements <- df |>
    filter(!is.na({{.var}}), sample_type == "sample") |>
    left_join(df.ref.strain, by = c("location", "bio_replicate", "experiment_date", 
                                    "experiment_id", "time_h")) |>
    mutate(
      # normalize by reference strain
      norm_value := {{.var}} / .data[[strain_var_nm]]
    ) |>
    ungroup() |> 
    select(-!!strain_var_nm )
  
  return(df.measurements)
}
