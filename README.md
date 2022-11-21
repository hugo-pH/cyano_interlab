Cyano Interlab study
================

This a repository to analyze the data from the Cyano Interlab study.

## Contents

- [Project structure](#project-structure)
- [Input data](#input-data)
- [Analysis](#analysis)

## Project structure

This project contains the following directories:

- `data/`: contains raw data files
- `figures/`: contains the figures for the manuscript
- `R/`: contains all functions for data loading and processing, and
  figure generation
- `reports/`: contains reports to load raw data, process it and export
  it and to create the figures for the manuscript
- `_targets`: directory created by the
  [targets](https://docs.ropensci.org/targets/) package required to run
  he workflow
- `renv`: directory created by the
  [renv](https://rstudio.github.io/renv/articles/renv.html) package
  required to manage project dependencies

## Input data

Participants submitted data in standard spreadsheet templates from
experimental runs and reference measurements. These are stored
in`data/experiments/` and `data//reference/` respectively. In addition,
two extra datasets are needed to run the analysis. `data/dilutions`
contains two spreadsheets with information about the dilutions factors
applied by each participant in the micro-titer plates.
`data/location_labels` contains a table to match the location name in
the raw files to the names that will be displayed in the figures.

## Analysis

The analysis of this project is done using a
[targets](https://docs.ropensci.org/targets/) pipeline. This pipeline
includes reading raw files, wrangling data, normalization of
plate-reader measurements, and creating the figures and statistical
analyses presented in the manuscript.

The code to run the pipeline is found in the `_targets.R` file and can
be executed by running `targets::tar_make()`. This pipeline makes use of
custom functions stored in the `R/` directory. Below, we describe every
step of the process:

### Reading data

Raw data is loaded with the functions `read_dilutions()`,
`read_raw_experiment_data()`, and `read_raw_reference_data()`. The first
function will create a regular data frame. Tast two, will create a
list-column tibble containing a row per lab, experimental run and
dataset, stored in the `raw_data` column. These tibbles can be obtained
by providing the corresponding path and file name pattern to each
function or by using `targets::tar_load()` after running the pipeline:

``` r
targets::tar_load(data.experiments.raw)
data.experiments.raw
```

    ## # A tibble: 170 × 5
    ##    location  sheet                           raw_data            exper…¹ exper…²
    ##    <chr>     <chr>                           <list>              <chr>   <chr>  
    ##  1 Amsterdam Fluorescence Plate Reader       <tibble [28 × 11]>  202111… 202111…
    ##  2 Amsterdam OD Plate Reader                 <tibble [28 × 11]>  202111… 202111…
    ##  3 Amsterdam OD 730 nm Spectrophotometer     <tibble [8 × 10]>   202111… 202111…
    ##  4 Amsterdam Chlorophyll 665 nm Spectrophoto <tibble [8 × 6]>    202111… 202111…
    ##  5 Amsterdam Spectrum Spectrophotometer      <tibble [351 × 21]> 202111… 202111…
    ##  6 Amsterdam Fluorescence Plate Reader       <tibble [28 × 11]>  202111… 202111…
    ##  7 Amsterdam OD Plate Reader                 <tibble [28 × 11]>  202111… 202111…
    ##  8 Amsterdam OD 730 nm Spectrophotometer     <tibble [8 × 10]>   202111… 202111…
    ##  9 Amsterdam Chlorophyll 665 nm Spectrophoto <tibble [8 × 6]>    202111… 202111…
    ## 10 Amsterdam Spectrum Spectrophotometer      <tibble [351 × 21]> 202111… 202111…
    ## # … with 160 more rows, and abbreviated variable names ¹​experiment_date,
    ## #   ²​experiment_id

### Processing raw data

Two functions (`process_experiments_data()` and
`process_reference_data()`) are used to process the tibbles generated in
the previous step. These functions will wrangle the raw data into the
right format and process the plate-reader (background-signal correction
and reference-strain normalization of relative fluorescence units). The
output is a list-column tibble, containing a dataset per row. These
tibbles can be obtained by providing the corresponding datasets to each
function or by using `targets::tar_load()` after running the pipeline:

``` r
targets::tar_load(data.experiments.processed)
data.experiments.processed
```

    ## # A tibble: 9 × 2
    ##   data_id          data                  
    ##   <chr>            <list>                
    ## 1 pr.fl.raw        <tibble [6,664 × 10]> 
    ## 2 pr.od.raw        <tibble [6,727 × 10]> 
    ## 3 pr.fl            <tibble [8,232 × 11]> 
    ## 4 pr.od            <tibble [8,295 × 11]> 
    ## 5 pr.bc            <tibble [1,904 × 16]> 
    ## 6 pr.norm          <tibble [1,904 × 17]> 
    ## 7 sp.od            <tibble [1,904 × 8]>  
    ## 8 sp.full.spectrum <tibble [279,180 × 8]>
    ## 9 chl              <tibble [360 × 9]>

The table shows the content of each dataset:

| Dataset            | Description                                                                                                |
|--------------------|------------------------------------------------------------------------------------------------------------|
| `pr.fl.raw`        | Long-format plate-reader fluorescence                                                                      |
| `pr.od.raw`        | Long-format plate-reader OD                                                                                |
| `pr.fl`            | Long-format, dilution corrected, plate-reader fluorescence                                                 |
| `pr.od`            | Long-format, dilution corrected, plate-reader OD                                                           |
| `pr.bc`            | Long-format, dilution corrected, background-signal corrected, plate-reader fluorescence                    |
| `pr.norm`          | Long-format, dilution corrected, background-signal corrected, reference-strain normalized plate-reader RFU |
| `sp.od`            | Long-format spectrophotometer OD                                                                           |
| `sp.full.spectrum` | Long-format spectrophotometer full spectrum                                                                |
| `chl`              | Long-format spectrophotometer chlorophyll content                                                          |

Processed datasets can be accessed with:

``` r
targets::tar_load(data.experiments.processed)
# example to extract spectrophotometer OD
data.experiments.processed |> 
      dplyr::filter(data_id == "sp.od") |> 
      tidyr::unnest(data) |> 
      dplyr::select(-data_id)
```

    ## # A tibble: 1,904 × 8
    ##    location  strain induction bio_replicate experiment_d…¹ exper…² OD_730 time_h
    ##    <chr>     <chr>  <chr>             <dbl> <chr>          <chr>    <dbl>  <dbl>
    ##  1 Amsterdam EVC    -                     1 20211118       202111…  0.532      0
    ##  2 Amsterdam EVC    -                     1 20211118       202111…  0.501      2
    ##  3 Amsterdam EVC    -                     1 20211118       202111…  0.574      4
    ##  4 Amsterdam EVC    -                     1 20211118       202111…  0.584      5
    ##  5 Amsterdam EVC    -                     1 20211118       202111…  0.627      6
    ##  6 Amsterdam EVC    -                     1 20211118       202111…  0.658      7
    ##  7 Amsterdam EVC    -                     1 20211118       202111…  1.89      24
    ##  8 Amsterdam EVC    +                     1 20211118       202111…  0.532      0
    ##  9 Amsterdam EVC    +                     1 20211118       202111…  0.512      2
    ## 10 Amsterdam EVC    +                     1 20211118       202111…  0.542      4
    ## # … with 1,894 more rows, and abbreviated variable names ¹​experiment_date,
    ## #   ²​experiment_id

### Figures

The code to produce each figure in the manuscript can be found in the
`R/` directory. Each figure is created with a custom function stored in
a separate file. The `targets` pipeline includes these functions and the
output is stored in the `figures/` directory (.tiff files are not
include in this repository due to file size).

### Dependencies

We provide [`renv`](https://rstudio.github.io/renv/articles/renv.html)
files to facilitate dependency management.
