#' Create figure S3 of interlab manuscript 
#' 
#'
#' @param all.datasets A list-column tibble with all datasets from experimental runs
#' @param df.location.labels A tibble containing the labs labels for plotting
#' @param figures.dir The output directory
#' @param figure.file.name The name for the figure 
#' @param width The width of the output figure
#' @param height The height of the output figure
#' @param dpi The resolution of the output figure
#' @param font.size The font size for text elements in the figure
#' @param figure.font.family The font family for text elements in the figure
#'
#' @return Plot object and files (png, tiff and pdf) in the output directory
plot_figure_S3 <-
  function(all.datasets.norm.nRFU,
           all.datasets.norm.nFU,
           figures.dir,
           figure.file.name = "figure_S3",
           height = 7.2,
           width = 18.5,
           dpi,
           scaling = 1,
           font.size, 
           figure.font.family) {
    
    # browser()
    df.pr.norm.nRFU <- all.datasets.norm.nRFU |> 
      filter(data_id == "pr.norm") |> 
      unnest(data) |> 
      select(-data_id) |> 
      filter(time_h < 24)
    
    df.pr.norm.nFU <- all.datasets.norm.nFU |> 
      filter(data_id == "pr.norm") |> 
      unnest(data) |> 
      select(-data_id) |> 
      filter(time_h < 24)
    
    df.pr.norm <- bind_rows(
      df.pr.norm.nFU |> 
        select(location, experiment_date, experiment_id,
               bio_replicate, strain, induction, time_h,
               sample_type, tech_replicate, norm_value) |> 
        mutate(normalization = "nFU"),
      df.pr.norm.nRFU |> 
        select(location, experiment_date, experiment_id,
               bio_replicate, strain, induction, time_h,
               sample_type, tech_replicate, norm_value) |> 
        mutate(normalization = "nRFU")
    ) |> 
      bind_rows(
        df.pr.norm.nRFU |> 
          select(location, experiment_date, experiment_id,
                 bio_replicate, strain, induction, time_h,
                 sample_type, tech_replicate, norm_value = fl_bc) |> 
          mutate(normalization = "FU_bc")
      ) |> 
      bind_rows(
        df.pr.norm.nRFU |> 
          select(location, experiment_date, experiment_id,
                 bio_replicate, strain, induction, time_h,
                 sample_type, tech_replicate, norm_value = fl_od) |> 
          mutate(normalization = "RFU")
      ) 
    
    df.cv.pr.fl.od.norm.intra <- df.pr.norm |>
      filter(strain != "J23100") |>
      group_by(normalization, location, strain, induction, time_h) |>
      summarise(
        mean = mean(norm_value),
        sd = sd(norm_value),
        CV = sd / mean
      ) |>
      group_by(normalization) |> 
      mutate(percentile = percent_rank(CV),
             variation = "Intralab") |> 
      ungroup() 
    
    df.cv.pr.fl.od.norm.inter <- df.pr.norm |>
      filter(strain != "J23100") |>
      group_by(normalization, location, strain, induction, time_h) |>
      summarise(norm_value = mean(norm_value)) |>
      group_by(normalization, strain, induction, time_h) |>
      summarise(
        mean = mean(norm_value),
        sd = sd(norm_value),
        CV = sd / mean
      ) |>
      group_by(normalization) |> 
      mutate(
        percentile = percent_rank(CV),
        percentile2 = ntile(CV, 100),
        variation = "Interlab"
      ) |> ungroup() 
    
    
    df.figure.cv <- bind_rows(
      df.cv.pr.fl.od.norm.intra,
      df.cv.pr.fl.od.norm.inter
    ) |> 
      mutate(
        normalization = fct_relevel(normalization, "FU_bc", "nFU", "RFU", "nRFU")
      )
    
    methods.labels <- c("nRFU" = "nRFU",
                        "nFU" = "nFU", 
                        "RFU" = "RFU",
                        "FU_bc" = expression(FU["bc"]))
    
    p <- df.figure.cv |>
      ggplot() +
      geom_violin(aes(x = normalization, y = CV, fill = normalization),
                  alpha = 0.1) +
      ggforce::geom_sina(
        aes(x = normalization, y = CV),
        alpha = 0.1,
        size = 3,
        seed = 1989
      ) +
      geom_boxplot(
        aes(x = normalization, y = CV, fill = normalization),
        alpha = 0.3,
        outlier.alpha = 0
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        labels = scales::percent_format(),
        expand = expansion(mult = c(0, 0.2)),
      ) +
      scale_x_discrete(
        labels = methods.labels
      ) +
      scale_fill_viridis_d(option = "A") +
      labs(x = "Normalization method",
           y = "CV (%)",
           fill = "") +
      theme_bw(font.size, base_family = figure.font.family) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none"
      ) +
      facet_wrap( ~ variation)
    
    save_figures_manuscript(
      p,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    
    p
  }