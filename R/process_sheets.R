#' Load and process spectrophotometer data
#'
#' The function cleans up the raw tibble, transform to long format, unifies the time to hours,
#' and creates columns for strain and induction regime
#' @param df A list-column tibble containing data from all sheets in a excel file. 
#' Each sheet is stored as a tibble in the raw_data column.
#' The other columns contain the experiment date and ID (full file name without extension)
#' and the name of the sheet where the data was contained in the excel file.
#'
#' @return A tibble containing the processed spectrophotometer data
load_spectrophotometer <- function(df) {
  df |>
    filter(sheet == "OD 730 nm Spectrophotometer") |>
    unnest(raw_data) |>
    select(-Location) |>
    clean_names() |>
    pivot_longer(
      cols = -c(
        sheet,
        sample,
        bio_replicate,
        experiment_date,
        experiment_id,
        location
      ),
      values_to = "OD_730",
      names_to = "timepoint"
    ) |>
    mutate(time = as.numeric(str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time %in% c(24, 48),
                           time,
                           time / 60)) |>
    separate(sample,
             into = c("strain", "induction"),
             sep = " ") |>
    select(-c(sheet, timepoint, time))
}


#' Load and process plate-reader OD data
#'
#' The function cleans up the raw tibble, transform to long format, unifies the time to hours,
#' creates columns for strain and induction regime and labels well by sample type
#' @param df A list-column tibble containing data from all sheets in a excel file. 
#' Each sheet is stored as a tibble in the raw_data column.
#' The other columns contain the experiment date and ID (full file name without extension)
#' and the name of the sheet where the data was contained in the excel file.
#'
#' @return A tibble containing the processed plate-reader OD data
load_plate_reader_od <- function(df) {
  df |>
    filter(sheet == "OD Plate Reader") |>
    unnest(raw_data) |>
    select(-Location) |>
    clean_names() |>
    pivot_longer(
      cols = -c(
        sheet,
        sample,
        bio_replicate,
        tech_replicate,
        experiment_date,
        experiment_id,
        location
      ),
      values_to = "OD_730",
      names_to = "timepoint"
    ) |>
    mutate(time = as.numeric(str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time %in% c(24, 48),
                           time,
                           time / 60)) |>
    separate(sample,
             into = c("strain", "induction"),
             sep = " ") |>
    mutate(sample_type = ifelse(strain == "Blank", "blank", "sample")) |>
    select(-c(sheet, timepoint, time))
}

#' Load and process plate-reader fluorescence data
#'
#' The function cleans up the raw tibble, transform to long format, unifies the time to hours,
#' creates columns for strain and induction regime and labels well by sample type
#' @param df A list-column tibble containing data from all sheets in a excel file. 
#' Each sheet is stored as a tibble in the raw_data column.
#' The other columns contain the experiment date and ID (full file name without extension)
#' and the name of the sheet where the data was contained in the excel file.
#'
#' @return A tibble containing the processed plate-reader fluorescence data
load_plate_reader_fl <- function(df) {
  df |>
    filter(sheet == "Fluorescence Plate Reader") |>
    unnest(raw_data) |>
    select(-Location) |>
    clean_names() |>
    pivot_longer(
      cols = -c(
        sheet,
        sample,
        bio_replicate,
        tech_replicate,
        experiment_date,
        experiment_id,
        location
      ),
      values_to = "fl",
      names_to = "timepoint"
    ) |>
    mutate(time = as.numeric(str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time %in% c(24, 48),
                           time,
                           time / 60)) |>
    separate(sample,
             into = c("strain", "induction"),
             sep = " ") |>
    mutate(sample_type = ifelse(strain == "Blank", "blank", "sample")) |>
    select(-c(sheet, timepoint, time))
  
}

#' Load and process full spectra data
#'
#' The function transform to long format, unifies the time to hours,
#' and creates columns for strain and induction regime
#' @param df A list-column tibble containing data from all sheets in a excel file.
#' Each sheet is stored as a tibble in the raw_data column.
#' The other columns contain the experiment date and ID (full file name without extension)
#' and the name of the sheet where the data was contained in the excel file.
#'
#' @return A tibble containing the processed full spectra data
load_full_spectrum <- function(df) {
  df |>
    filter(sheet == "Spectrum Spectrophotometer") |>
    unnest(raw_data) |>
    pivot_longer(
      cols = -c(sheet, nm, experiment_date, experiment_id, location),
      values_to = "abs",
      names_to = "id"
    ) |>
    separate(id, into = c("strain", "id"), sep = "_") |>
    separate(id,
             into = c("timepoint", "induction"),
             sep = " ") |>
    mutate(time = as.numeric(str_remove_all(timepoint, "[[:alpha:]]")),
           time_h = ifelse(time %in% c(24, 48),
                           time,
                           time / 60)) |>
    select(-c(sheet, timepoint, time))
  
}

#' Load and process chlorophyll data
#'
#' The function  cleans up the raw tibble, transform to long format, unifies the time to hours,
#' and creates columns for strain and induction regime
#' @param df A list-column tibble containing data from all sheets in a excel file.
#' Each sheet is stored as a tibble in the raw_data column.
#' The other columns contain the experiment date and ID (full file name without extension)
#' and the name of the sheet where the data was contained in the excel file.
#'
#' @return A tibble containing the processed chlorophyll data
load_chl <- function(df) {
  df |>
    filter(sheet == "Chlorophyll 665 nm Spectrophoto") |>
    # remove Bio replicate column to avoid type clashes in the reference dataset
    mutate(
      raw_data = map(raw_data, ~select(.x, -`Bio Replicate`))
    ) |> 
    unnest(raw_data) |> 
    select(-Location) |>
    janitor::clean_names() |> 
    # remove columns where data was added accidentally (Leipzig, 20220126)
    select(-starts_with("x")) |>
    filter(!is.na(t0)) |>
    pivot_longer(
      cols = -c(
        sheet,
        sample,
        experiment_date,
        experiment_id,
        location
      ),
      names_to = "time",
      values_to = "chl_a"
    ) |>
    mutate(
      time = as.numeric(str_remove_all(time, "[a-z]")),
      time_h = ifelse(time %in% c(24, 48),
                      time,
                      time / 60)
    ) |>
    separate(sample,
             into = c("strain", "induction"),
             sep = " ") |>
    group_by(location, experiment_date, experiment_id) |>
    mutate(n = n()) |>
    group_by(location) |>
    mutate(
      n_exp = n_distinct(experiment_id),
      bio_replicate = rep(1:unique(n_exp), each = unique(n))
    ) |>
    select(!starts_with("n"), -sheet) |>
    ungroup()
  
}
