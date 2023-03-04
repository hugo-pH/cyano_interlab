#' Create figure 4 of interlab manuscript 
#' 
#' Summarizes normalized rfu by calculating mean and confidence interval
#'
#' @param all.datasets A list-column tibble with all datasets from experimental runs
#' @param figures.dir The output directory
#' @param figure.file.name The name for the figure 
#' @param width The width of the output figure
#' @param height The height of the output figure
#' @param dpi The resolution of the output figure
#' @param scaling Scaling value for ggsave
#' @param font.size The font size for text elements in the figure
#' @param figure.font.family The font family for text elements in the figure
#'
#' @return Plot object and files (png, tiff and pdf) in the output directory
plot_figure_4 <-
  function(all.datasets,
           figures.dir,
           figure.file.name = "figure_4",
           height = 16.2,
           width = 18.5,
           dpi,
           scaling = 1,
           font.size, 
           figure.font.family
           ) {
   
    
    
    df.pr.norm <- all.datasets |> 
      filter(data_id == "pr.norm") |> 
      unnest(data) |> 
      select(-data_id) |> 
      filter(time_h < 24)
    
    strain.labels <- c("EVC" = "EVC",  "J23100" = expression(P["J23100"]), 
                       "petE" = expression(P["petE"]), "prha" = expression(P["rhaBAD"]))
    
    
    
    # summarize data and calculate CV
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
    
    df.cv.strains <- bind_rows(
      df.cv.pr.fl.od.norm.intra,
      df.cv.pr.fl.od.norm.inter
    ) |> 
      filter(strain %in% c("petE", "prha")) |> 
      mutate(
        variation = str_to_title(variation)
      ) |> 
      group_by(variation, strain, induction) |> 
      mutate(
        percentile = percent_rank(CV)
      ) 
   
    # plot
    p.cv.strains <- df.cv.strains |> 
      ggplot() +
      facet_grid(variation~induction, scales = "free_y") +
      geom_violin(
        aes(x = strain, y = CV, fill = variation), 
        alpha = 0.3,
        position = position_dodge(width = .8)
      ) +
      geom_sina(
        aes(x = strain, y = CV, fill = variation), 
        alpha = 0.3,
        position = position_dodge(width = .8),
        size = 3, seed = 1989
      ) +
      geom_boxplot(
        aes(x = strain, y = CV, fill = variation), 
        alpha = 0.3, outlier.alpha = 0, 
        position = position_dodge(width = .8)
      ) +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = expansion(mult = c(0.03, 0.2)),
        limits = c(0, NA)
      ) +
      scale_x_discrete(
        labels = strain.labels
      ) +
      scale_fill_manual(values = colorblind_pal()(8)[c(2, 4)]) +
      theme(legend.position = "bottom") +
      labs(
        x = "Strain",
        y = "CV (%)",
        fill = ""
      ) +
      theme_bw(font.size, base_family = figure.font.family) +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
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