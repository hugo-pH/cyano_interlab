#' Read all datasets from experimental runs
#' 
#' The function finds all files matching a pattern in a specific directory and subdirectories.
#' Then each sheet from raw files is read and the resulting tibble is stored as list-column.
#'
#' @param path The path to directory where raw experimental run files are stored
#' @param pattern The pattern matching the names of raw data files
#'
#' @return A list-column tibble containing all datasets from the experimental runs 
#' in each location. The datasets are stored in the value column as a tibble.  
read_raw_experiment_data <- function(path, pattern) {
  # get location directories as tibble and extract location from path
  list.dirs(path, recursive = FALSE) |> 
    enframe(value = "path") |>
    select(-name) |>
    mutate(location = map_chr(path, function(x)
      str_split(x, "/") |> last() |> last())) |> 
    # list all valid files in location dirs
    mutate(
      data_file = map(path, ~list.files(., pattern, full.names = T))
    ) |> 
    unnest(data_file) |> 
    # read files
    mutate(data = map(data_file, read_experiment_file, pattern = pattern)) |>
    unnest(data) |>
    select(-c(path, data_file))
}

#' Read all datasets from reference measurements
#' 
#' The function finds all files matching a pattern in a specific directory.
#' Then each sheet from raw files is read and the resulting tibble is stored as list-column.
#'
#' @param path The path to directory where reference measurement files are stored
#' @param pattern The pattern matching the names of raw data files
#'
#' @return A list-column tibble containing all datasets from the reference measurements
#' in each location. The datasets are stored in the value column as a tibble.  
read_raw_reference_data <- function(path, pattern) {
  data.files <- list.files(path, pattern, full.names = T)
  map_dfr(data.files, read_experiment_file, pattern = pattern) |>
    mutate(location = map_chr(experiment_id, ~ sub(str_remove(pattern, ".xlsx"), "\\3", .)))
}

#' Read raw data excel file
#' 
#' The function reads all sheets from excel file
#'
#' @param path The path to an excel file containing the data from experimental runs or reference measurements
#' @param pattern The pattern matching the names of raw data files
#'
#' @return A list-column tibble containing data from all sheets in a excel file. 
#' Each sheet is stored as a tibble in the raw_data column.
#' The other columns contain the experiment date and ID (full file name without extension)
#' and the name of the sheet where the data was contained in the excel file.
read_experiment_file <- function(path, pattern) {
  # extract experiment date from path
  experiment.date <- sub(pattern, "\\2", path)
  # extract file name without extension from file path
  experiment.id <- str_split(path, "/") |>
    last() |>
    last() |>
    str_split("\\.") |>
    first() |>
    first()
  # read all sheets from excel
  path |>
    excel_sheets() |>
    set_names() |>
    map(read_excel, path = path) |>
    enframe(name = "sheet", value = "raw_data") |>
    mutate(experiment_date = experiment.date,
           experiment_id = experiment.id)
}
#' Read dilutions file
#'
#' @param path The path to an excel file containing the dilutions 
#' from experimental runs or reference measurements
#' @return A tibble 
read_dilutions <- function(path){
  read_excel(path, col_types = c(rep("text", 2), "numeric", "logical")) |> 
    rename(location_experiment = location) 
}