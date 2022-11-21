#' Create figure 3 of interlab manuscript 
#' 
#' Summarizes normalized rfu by calculating mean and confidence interval
#'
#' @param all.datasets A list-column tibble with all datasets from experimental runs
#' @param figures.dir The output directory
#' @param figure.file.name The name for the figure 
#' @param width The width of the output figure
#' @param height The height of the output figure
#' @param dpi The resolution of the output figure
#' @param font.size The font size for text elements in the figure
#' @param figure.font.family The font family for text elements in the figure
#'
#' @return Plot object and files (png, tiff and pdf) in the output directory
plot_figure_3 <-
  function(all.datasets,
           figures.dir,
           figure.file.name = "figure_3",
           height = 19,
           width = 18.5,
           dpi,
           scaling = 1,
           font.size, 
           figure.font.family) {
   
    
    df.pr.norm <- all.datasets |> 
      filter(data_id == "pr.norm") |> 
      unnest(data) |> 
      select(-data_id) |> 
      filter(time_h < 24)
    
    # define strains labels for plotting
    df.strain.labels <- tribble(
      ~strain, ~strain_label,
      "EVC", "EVC",
      "J23100", "P[J23100]",
      "petE", "P[petE]",
      "prha", "P[rhaBAD]"
    )
    
    
    se <- function(x, mult = 1) {
      x <- x[!is.na(x)]
      mult * sqrt(stats::var(x)/length(x))
    } 
    
    df.pr.norm.sum <- df.pr.norm |> 
      group_by(location, strain, induction, time_h) |>
      summarise(
        fl_od_norm_ci = se(fl_od_norm, mult = 1.96),
        fl_od_norm = mean(fl_od_norm, na.rm = T)
      ) 
    
    p.time.course <- df.pr.norm |> 
      group_by(location, strain, induction, time_h) |>
      summarise(
        fl_od_norm = mean(fl_od_norm, na.rm = T)
      ) |> 
      left_join(df.strain.labels, by = "strain") |>
      ggplot(aes(x = time_h, y = fl_od_norm, colour = induction)) +
      geom_point(position = position_jitter(seed = 1989, width = 0.1),
                 alpha = 0.2) +
      stat_summary(fun = "mean", geom = "line",
                   alpha = 0.8, size = 1) +
      stat_summary(fun.data = "mean_se", geom = "errorbar",
                   alpha = 0.8, size = 1, width = 0.1, fun.args = list(mult = 1.96)) +
      stat_summary(fun = "mean", geom = "point",
                   alpha = 0.8, size = 2.5) +
      facet_wrap(~strain_label, labeller = label_parsed) +
      scale_color_colorblind() +
      theme(
        panel.grid.major.x = element_blank() 
      ) +
      labs(
        x = bquote("Time"~(h)),
        y = "nRFU",
        colour = "Induction regime"
      ) +
      theme_light(font.size, base_family = figure.font.family) +
      theme(legend.position = "bottom")
    
    save_figures_manuscript(
      p.time.course,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    
    p.time.course
  }