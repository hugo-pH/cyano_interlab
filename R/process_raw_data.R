#' Load, process and combine all data from experimental runs
#'
#' All datasets are loaded using dedicated functions. 
#' For plate-reader data, 24 timepoint values are corrected by dilution factor when appropriate.
#' In addition, plate-reader data is background corrected and normalized 
#' 
#' @param df.experiments.raw A list-column tibble containing all datasets from the experimental runs 
#' in each location. The datasets are stored in the raw_data column as a tibble.  
#' @param df.pr.dilutions A tibble containing the dilution factor applied in each lab 
#' for the 24h timepoint
#' @param correct_negative A logical value indicating whether to correct negative values by
#' adding the absolute value of the most negative value to all data points
#'
#' @return A list-column tibble with a row per dataset
process_experiments_data <- function(df.experiments.raw, df.pr.dilutions,
                                     correct_negative = T, ref_strain = "J23100",
                                     ref_induction = "-"){
  # load pr OD
  df.pr.od.raw <- load_plate_reader_od(df.experiments.raw)
  # load pr fluorescence
  df.pr.fl.raw <- load_plate_reader_fl(df.experiments.raw)
  # load spectrophotometer
  df.spectrophotometer <- load_spectrophotometer(df.experiments.raw)
  # load full spectrum
  df.full.spectrum.raw <- load_full_spectrum(df.experiments.raw)
  # load chlorophyll
  df.chl.raw <- load_chl(df.experiments.raw)  
  
  # correct dilution factors
  df.pr.od <- df.pr.od.raw |> 
    left_join(df.pr.dilutions) |> 
    mutate(
      OD_730 = ifelse(dilution_corrected == F & time_h == 24 & sample_type == "sample", 
                      OD_730 * dilution_factor, 
                      OD_730)
    ) |> 
    select(-c(dilution_factor, dilution_corrected)) 
  df.pr.fl <- df.pr.fl.raw |> 
    left_join(df.pr.dilutions) |> 
    mutate(
      fl = ifelse(dilution_corrected == F & time_h == 24  & sample_type == "sample", 
                  fl * dilution_factor, 
                  fl)
    ) |> 
    select(-c(dilution_factor, dilution_corrected)) 
  
  # perform background correction
  df.pr.bc <- calculate_rfu(
    df.od = df.pr.od,
    df.fl = df.pr.fl,
    dilutions = df.pr.dilutions,
    background_correction = correct_negative
  )  |> 
    filter(!is.infinite(fl_od)) |> 
    group_by(location, experiment_date, experiment_id, bio_replicate, 
             strain, induction, time_h, sample_type) |> 
    summarise(across(where(is.numeric), mean, na.rm = T)) |> 
    ungroup() 
  
  df.pr.norm <- strain_normalization(df.pr.bc, .var = fl_od, 
                                     ref_strain = ref_strain, ref_induction = ref_induction) 
  
  tribble(
    ~data_id, ~data,
    "pr.fl.raw", df.pr.fl.raw,
    "pr.od.raw", df.pr.od.raw,
    "pr.fl", df.pr.fl,
    "pr.od", df.pr.od,
    "pr.bc", df.pr.bc,
    "pr.norm", df.pr.norm,
    "sp.od", df.spectrophotometer,
    "sp.full.spectrum", df.full.spectrum.raw,
    "chl", df.chl.raw
  )
  
}

#' Load, process and combine all data from the reference measurements
#'
#' All datasets are loaded using dedicated functions. 
#' For plate-reader data, 48 timepoint values are corrected by dilution factor when appropriate.
#' In addition, plate-reader data is background corrected and normalized 
#' 
#' @param df.experiments.raw a list-column tibble containing all datasets from the reference measurements 
#' in each location. The datasets are stored in the raw_data column as a tibble.  
#' @param df.pr.dilutions a tibble containing the dilution factor applied in each lab 
#' for the 48h timepoint
#'
#' @return a list-column tibble with a row per dataset
process_reference_data <- function(df.normalization.raw, df.pr.dilutions) {
  # load pr OD
  df.pr.od.raw <- load_plate_reader_od(df.normalization.raw)
  # load pr fluorescence
  df.pr.fl.raw <- load_plate_reader_fl(df.normalization.raw)
  
  df.spectrophotometer.raw <- load_spectrophotometer(df.normalization.raw) 
  
  df.pr.od <- df.pr.od.raw |> 
    left_join(df.pr.dilutions) |>
    mutate(
      OD_730 = ifelse(dilution_corrected == F & time_h == 48 & sample_type == "sample", 
                      OD_730 * dilution_factor, 
                      OD_730)
    ) |> 
    select(-dilution_factor)
  
  df.pr.fl <- df.pr.fl.raw |> 
    left_join(df.pr.dilutions) |> 
    mutate(
      fl = ifelse(dilution_corrected == F & time_h == 48  & sample_type == "sample", 
                  fl * dilution_factor, 
                  fl)
    ) |> 
    select(-dilution_factor)
  
  df.pr.bc <- calculate_rfu(
    df.od = df.pr.od,
    df.fl = df.pr.fl,
    dilutions = df.pr.dilutions
  )
    
    tribble(
      ~data_id, ~data,
      "pr.bc", df.pr.bc,
      "sp.od", df.spectrophotometer.raw
    )
    
}

