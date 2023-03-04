#' Create figure S4 of interlab manuscript 
#' 
#' Calculate interlab CV of rfu (not normalized) for induced and uninduced cultures
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
plot_figure_S4 <-
  function(all.datasets,
           figures.dir,
           figure.file.name = "figure_S4",
           height = 10,
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
    
    
    df.cv.pr.fl.od.norm.intra <- df.pr.norm |> 
      group_by(location, strain, induction, time_h) |> 
      summarise(
        mean_RFU = mean(fl_od),
        mean_nRFU = mean(fl_od_norm),
        sd = sd(fl_od),
        CV = sd / mean_RFU
      ) |> 
      group_by(strain, induction) |> 
      mutate(
        percentile = percent_rank(CV)
      )
  
    strain.labels <- c("EVC" = "EVC",  "J23100" = expression(P["J23100"]), 
                       "petE" = expression(P["petE"]), "prha" = expression(P["rhaBAD"]))
    
    p.cv.strains <- df.cv.pr.fl.od.norm.intra |> 
      ggplot(aes(x = percentile, y = CV, colour = strain)) +
      geom_point(alpha = 0.7) +
      scale_y_continuous(
        limits = c(0, NA),
        labels = scales::percent_format(),
        expand = expansion(mult = c(0, 0.2))) +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c(0, 0.25, 0.5, 0.75, 1)
      ) +
      scale_color_manual(
        values = colorblind_pal()(8)[c(4:8)],
        labels = strain.labels
        
      ) +
      facet_wrap(~induction) +
      labs(
        x = "Percentile",
        y = "CV (%)",
        colour = "Strain"
      ) +
      theme_bw(font.size, 
               base_family = figure.font.family)  +
      theme(
        legend.position = "bottom"
      )
    
    save_figures_manuscript(
      p.cv.strains,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    p.cv.strains
  }