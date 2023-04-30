# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "readxl", "janitor", "ggforce", 
               "ggthemes", "broom", "agricolae", "patchwork"), 
  format = "rds" 
)

# Run the R scripts in the R/ folder with custom functions:
tar_source()

list(
  #####################
  ### Load raw data ###
  #####################
  # load location labels
  tar_target(location.label.file, file.path(here::here(),
                                            "data", 
                                            "location_labels", 
                                            "location_labels.csv"),
             format = "file"),
  tar_target(data.location.labels, read.csv(location.label.file)),
  # load raw data from experimental runs
  tar_target(experiments.dir, file.path(here::here(), "data", "experiments"),
             format = "file"),
  tar_target(pattern, "(.*/)?([0-9]{8})_([[:alnum:]]+)_([[:alnum:]]+).xlsx"),
  tar_target(data.experiments.raw, 
             read_raw_experiment_data(experiments.dir, pattern)),
  tar_target(experiments.dilutions.file, file.path(here::here(),
                                                   "data", 
                                                   "dilutions", 
                                                   "experiment_plate_reader_dilutions.xlsx"), 
             format = "file"),
  tar_target(data.experiments.dilutions, read_dilutions(experiments.dilutions.file)),
  # load raw data from reference measurements (normalization dataset)
  tar_target(reference.dir, file.path(here::here(), "data", "reference"),
             format = "file"),
  tar_target(data.reference.raw, 
             read_raw_reference_data(reference.dir, pattern)),
  tar_target(reference.dilutions.file, file.path(here::here(),
                                                 "data", 
                                                 "dilutions", 
                                                 "normalization_plate_reader_dilutions.xlsx"), 
             format = "file"),
  tar_target(data.reference.dilutions, read_dilutions(reference.dilutions.file)),
  ########################
  ### Process raw data ###
  ########################
  # process experiment data; combine all datasets and normalize plate-reader data
  tar_target(data.experiments.processed, 
             process_experiments_data(data.experiments.raw, data.experiments.dilutions, norm_var = fl_od)),
  tar_target(data.experiments.processed.nFU, 
             process_experiments_data(data.experiments.raw, data.experiments.dilutions, norm_var = fl_bc)),
  # process reference data; combine all datasets and normalize plate-reader data
  tar_target(data.reference.processed, 
             process_reference_data(data.reference.raw, data.reference.dilutions)),
  ######################
  ### Create figures ###
  ######################
  # set figure default settings
  tar_target(figures.dir, file.path(here::here(), "figures")),
  tar_target(figures.dpi, 600),
  tar_target(figures.font.size, 14),
  tar_target(figure.font.family, "Serif"),
  # create figure 1
  tar_target(figure.1, plot_figure_1(data.experiments.processed, 
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     figure.font.family = figure.font.family)),
  # create figure 2
  tar_target(figure.2, plot_figure_2(data.experiments.processed,
                                     data.location.labels,
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     scaling = 1.1,
                                     figure.font.family = figure.font.family)),
  # create figure 3
  tar_target(figure.3, plot_figure_3(data.experiments.processed,
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     figure.font.family = figure.font.family)),
  # create figure 4
  tar_target(figure.4, plot_figure_4(data.experiments.processed,
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     figure.font.family = figure.font.family)),
  # create figure 5
  tar_target(figure.5, plot_figure_5(data.experiments.processed,
                                     data.location.labels,
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     figure.font.family = figure.font.family)),
  # create figure S2
  tar_target(figure.S2, plot_figure_S2(data.experiments.processed,
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     figure.font.family = figure.font.family)),
  # create figure S3
  tar_target(figure.S3, plot_figure_S3(data.experiments.processed,
                                     data.reference.processed,
                                     data.location.labels,
                                     figures.dir = figures.dir,
                                     dpi = figures.dpi,
                                     font.size = figures.font.size, 
                                     scaling = 2,
                                     figure.font.family = figure.font.family)),
  # create figure S4
  tar_target(figure.S4, plot_figure_S4(data.experiments.processed,
                                       figures.dir = figures.dir,
                                       dpi = figures.dpi,
                                       font.size = figures.font.size, 
                                       figure.font.family = figure.font.family)),
  # create figure S5
  tar_target(figure.S5, plot_figure_S5(data.experiments.processed,
                                       data.location.labels,
                                       figures.dir = figures.dir,
                                       dpi = figures.dpi,
                                       font.size = figures.font.size, 
                                       figure.font.family = figure.font.family)),
  # create figure S6
  tar_target(figure.S6, plot_figure_S6(data.experiments.processed,
                                       data.experiments.processed.nFU,
                                       data.location.labels,
                                       figures.dir = figures.dir,
                                       dpi = figures.dpi,
                                       font.size = figures.font.size, 
                                       figure.font.family = figure.font.family))
)
