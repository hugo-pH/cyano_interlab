#' Create figure 5 of interlab manuscript 
#' 
#' Summarizes normalized rfu by calculating mean and confidence interval
#'
#' @param all.datasets A list-column tibble with a all datasets from experimental runs
#' @param figures.dir The output directory
#' @param figure.file.name The name for the figure 
#' @param width The width of the output figure
#' @param height The height of the output figure
#' @param dpi The resolution of the output figure
#' @param font.size The font size for text elements in the figure
#' @param figure.font.family The font family for text elements in the figure
#'
#' @return Plot object and files (png, tiff and pdf) in the output directory
plot_figure_5 <-
  function(all.datasets,
           df.location.labels,
           figures.dir,
           figure.file.name = "figure_5",
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
    strain.labels <- c("EVC" = "EVC",  "J23100" = expression(P["J23100"]), 
                       "petE" = expression(P["petE"]), "prha" = expression(P["rhaBAD"]))
    
    
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

    df.fold.changes <- df.pr.norm.sum |> 
      filter(time_h %in% c(0, 7)) |> 
      select(-fl_od_norm_ci) |> 
      pivot_wider(names_from = time_h, 
                  values_from = fl_od_norm, 
                  names_prefix = "time_h_") |> 
      mutate(
        fold_change = time_h_7 / time_h_0
      ) 
    
    df.leakniness <- df.pr.norm.sum |> 
      filter(time_h  == 7, induction == "-", strain %in% c("EVC", "petE", "prha")) |> 
      select(-fl_od_norm_ci) |> 
      pivot_wider(names_from = strain, values_from = fl_od_norm) |> 
      mutate(
        leakiness_prha = prha / EVC,
        leakiness_petE = petE / EVC
      ) |> 
      select(-c(EVC, petE, prha)) |> 
      pivot_longer(
        cols = starts_with("leakiness"),
        names_to = "strain", 
        values_to = "leakiness",
        names_prefix = "leakiness_"
      )
    
      p1 <-  df.fold.changes |> 
        filter(strain %in% c("petE", "prha")) |> 
        ggplot(aes(x = strain, y = fold_change)) +
        stat_summary(
          aes(fill = induction),
          fun = "mean", geom = "col", 
          position = position_dodge(width = .9),
          alpha = 0.65, show.legend = F) +
        geom_point(aes(colour = induction),
                   position = position_dodge(width = .9),
                   alpha = 0.5) +
        stat_summary(aes(group = induction), 
                     fun.data = "mean_se", geom = "errorbar",
                     position = position_dodge(width = .9),
                     fun.args = list(mult = 1.96),
                     width = .2, colour = "grey50"
        ) +
        scale_x_discrete(
          labels = strain.labels
        ) +
        scale_fill_colorblind() +
        scale_colour_colorblind() +
        labs(
          x = "Strain",
          y = "Ratio",
          title = "nRFU 7 h / nRFU 0 h",
          colour = "Induction regime",
          tag = "A"
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.05))
        ) +
        theme_bw(font.size, base_family = figure.font.family) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(.85, .75),
          legend.title = element_text(size = 10),
          legend.box.background = element_rect(fill = "black")
        )
      
      
      p2 <- df.pr.norm.sum |> 
        filter(time_h  == 7, induction == "+") |> 
        ggplot(aes(x = strain, y = fl_od_norm)) +
        stat_summary(
          fun = "mean", geom = "col", 
          position = position_dodge(width = .9),
          alpha = 0.65, fill = "#E69F00") +
        geom_point(position = position_dodge(width = .9),
                   alpha = 0.5, colour = "#E69F00") +
        stat_summary(
          fun.data = "mean_se", geom = "errorbar",
          position = position_dodge(width = .9),
          fun.args = list(mult = 1.96),
          width = .2, colour = "grey50"
        ) +
        scale_x_discrete(
          labels = strain.labels
        ) +
        scale_fill_colorblind() +
        scale_colour_colorblind() +
        labs(
          x = "Strain",
          y = "nRFU",
          title = "nRFU 7 h",
          tag = "B"
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.05))
        ) +
        theme_bw(font.size, base_family = figure.font.family) +
        theme(legend.position = "bottom") +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
      
      p3 <-  df.leakniness |> 
        ggplot(aes(x = strain, y = leakiness)) +
        stat_summary(
          fun = "mean", geom = "col", 
          position = position_dodge(width = .9),
          alpha = 0.65, fill = "#000000") +
        geom_point(position = position_dodge(width = .9),
                   alpha = 0.5) +
        stat_summary(
          fun.data = "mean_se", geom = "errorbar",
          position = position_dodge(width = .9),
          fun.args = list(mult = 1.96),
          width = .1, colour = "grey50"
        ) +
        scale_x_discrete(
          labels = strain.labels
        ) +
        labs(
          x = "Strain",
          y = "Ratio to EVC",
          title = "Off-state",
          colour = "Induction",
          tag = "C"
        ) +
        geom_hline(yintercept = 1, alpha = 0.5) +
        theme_bw(font.size, base_family = figure.font.family) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
      
      p4 <- df.pr.norm |> 
        select(time_h, location, experiment_id, strain, induction, fl_od_norm) |> 
        filter(time_h  == 7, induction == "-", strain %in% c("EVC", "petE")) |> 
        pivot_wider(names_from = strain, values_from = fl_od_norm) |> 
        mutate(
          leakiness_petE = petE / EVC
        ) |> 
        select(-c(EVC, petE)) |> 
        pivot_longer(
          cols = starts_with("leakiness"),
          names_to = "strain", 
          values_to = "leakiness",
          names_prefix = "leakiness_"
        ) |> 
        rename(location_experiment = location) |> 
        left_join(df.location.labels, by = "location_experiment")  |> 
        select(-location_experiment) |> 
        ggplot(aes(x = leakiness, y = location)) +
        stat_summary(fun = "mean", geom = "col", 
                     position = position_dodge2(width = 0.8),
                     alpha = 0.2, width = 0.5) +
        geom_point(size = 2, position = position_dodge(width = 0.5, preserve = "total"),
                   alpha = 0.4) +
        geom_vline(xintercept = 1, alpha = 0.5) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.05))
        ) +
        theme_bw(font.size, base_family = figure.font.family) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        labs(
          x = "Ratio to EVC",
          y = "",
          title = bquote(P["petE"]~"off-state"),
          tag = "D"
        )
      
      p.promoter.char <- {(p1 / p3) | 
          ((p2 + theme(title = element_text(margin = margin(r = -95, unit = "pt")), 
                       axis.title.y = element_text(margin = margin(r = -110, unit = "pt")))) / 
             (p4 + theme(title = element_text(margin = margin(r = -95, unit = "pt"))))
          )}  + plot_layout(guides = "collect")  & theme(plot.title = element_text(size = 12), 
                                                         legend.position = "bottom")
    
    save_figures_manuscript(
      p.promoter.char,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    
    p.promoter.char
  }