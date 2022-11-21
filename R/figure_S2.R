#' Create figure S2 of interlab manuscript 
#' 
#' Calculates inter- and intralab coefficient of variation (CV) for plate-reader normalized rfu
#' and spectrophotometer OD
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
plot_figure_S2 <-
  function(all.datasets,
           figures.dir,
           figure.file.name = "figure_S2",
           height = 10,
           width = 18.5,
           dpi,
           scaling = 1,
           font.size, 
           figure.font.family) {
   
    
    
    df.spectrophotometer <- all.datasets |> 
      filter(data_id == "sp.od") |> 
      unnest(data) |> 
      select(-data_id) |> 
      filter(time_h < 24)
    
    df.pr.norm <- all.datasets |> 
      filter(data_id == "pr.norm") |> 
      unnest(data) |> 
      select(-data_id) |> 
      filter(time_h < 24)
    
    df.cv.sp.intra <- df.spectrophotometer |> 
      group_by(location, strain, induction, time_h) |> 
      summarise(
        mean = mean(OD_730),
        sd = sd(OD_730),
        CV = sd / mean
      ) |> 
      ungroup() |> 
      mutate(
        percentile = percent_rank(CV),
        variation = "Intralab"
      )
    
    df.cv.sp.inter <- df.spectrophotometer |> 
      group_by(location, strain, induction, time_h) |> 
      summarise(
        OD_730 = mean(OD_730)
      ) |> 
      group_by(strain, induction, time_h) |> 
      summarise(
        mean = mean(OD_730),
        sd = sd(OD_730),
        CV = sd / mean
      ) |> 
      ungroup() |> 
      mutate(
        percentile = percent_rank(CV),
        variation = "Interlab"
      )
    
    
    df.cv.pr.fl.od.norm.intra <- df.pr.norm |> 
      filter(strain != "J23100") |> 
      group_by(location, strain, induction, time_h) |> 
      summarise(
        mean = mean(fl_od_norm),
        sd = sd(fl_od_norm),
        CV = sd / mean
      ) |> 
      ungroup() |> 
      mutate(
        percentile = percent_rank(CV),
        variation = "Intralab"
      )
    
    
    df.cv.pr.fl.od.norm.inter <- df.pr.norm |> 
      filter(strain != "J23100") |> 
      group_by(location, strain, induction, time_h) |> 
      summarise(
        fl_od_norm = mean(fl_od_norm)
      ) |> 
      group_by(strain, induction, time_h) |> 
      summarise(
        mean = mean(fl_od_norm),
        sd = sd(fl_od_norm),
        CV = sd / mean
      ) |> 
      ungroup() |> 
      mutate(
        percentile = percent_rank(CV),
        percentile2 = ntile(CV, 100),
        variation = "Interlab"
      )
    
    df.figure.cv <- bind_rows(
      df.cv.pr.fl.od.norm.intra |> mutate(variable = "Plate~reader~nRFU"),
      df.cv.pr.fl.od.norm.inter|> mutate(variable = "Plate~reader~nRFU"),
      df.cv.sp.inter |> mutate(variable = "Bench~spectrophotometer~OD[730]"),
      df.cv.sp.intra |> mutate(variable = "Bench~spectrophotometer~OD[730]")
    )  |> 
      mutate(
        variable = fct_relevel(variable, rev)
      )
    
    p.cv.all.perc <- df.figure.cv |> 
      ggplot(aes(x = percentile, y = CV, colour = variation)) +
      geom_point() +
      scale_y_continuous(
        limits = c(0, NA),
        labels = scales::percent_format(),
        expand = expansion(mult = c(0, 0.2))) +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c(0, 0.25, 0.5, 0.75, 1)
      ) +
      scale_color_manual(values = colorblind_pal()(8)[c(2, 4)]) +
      facet_wrap(~variable, labeller = label_parsed) +
      labs(
        x = "Percentile",
        y = "CV (%)",
        colour = "Variation source"
      ) +
      theme_light(font.size, base_family = figure.font.family) +
      theme(
        legend.position = "bottom"
      )
    
    save_figures_manuscript(
      p.cv.all.perc,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    
    p.cv.all.perc
  }