#' Create figure 2 of interlab manuscript 
#' 
#' Calculates growth rates spectrophotometer OD and perform ANOVA and Tukey's test
#'
#' @param all.datasets A list-column tibble with all datasets from experimental runs
#' @param df.location.labels A tibble containing the labs labels for plotting
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
plot_figure_2 <-
  function(all.datasets,
           df.location.labels,
           figures.dir,
           figure.file.name = "figure_2",
           height = 7.2,
           width = 9,
           dpi,
           scaling = 1.1,
           font.size, 
           figure.font.family) {
   
    
    df.spectrophotometer <- all.datasets |> 
      filter(data_id == "sp.od") |> 
      unnest(data) |> 
      select(-data_id) 
    
    color.scheme <- c("Amsterdam" = "#E69F00", 
                      "Edinburgh" = "#009E73",
                      "Berlin" = "#F0E442",
                      "Jena" = "#0072B2",
                      "Leipzig" = "#D55E00",
                      "Seville" = "#CC79A7",
                      "Tuebingen" = "#56B4E9",
                      "Duesseldorf (I)" = "#000000",
                      "Duesseldorf (II)" = "#4C4C4C"
    )
    
    
    # calculate growth rates
    df.growth.rates <- df.spectrophotometer |> 
      group_by(location, experiment_date, experiment_id,  bio_replicate, strain, induction) |> 
      nest() |> 
      mutate(
        model = map(data, ~lm(log(OD_730)~time_h, data = .)),
        coeffs = map(model, broom::tidy),
        r_squared = map(model, broom::glance)
      ) |> 
      unnest(coeffs) |> 
      filter(term == "time_h") |> 
      select(-c(std.error, statistic, p.value)) |> 
      unnest(r_squared) 
    
    # perform ANOVA 
    anova.test <- aov(estimate ~ location+strain+induction, data = df.growth.rates)  
    df.anova <- tidy(anova.test)
    
    # post-hoc test
    tukey.test <- TukeyHSD(x=anova.test, trt = 'location', conf.level=0.95)
    df.tukey <- tidy(tukey.test) |> 
      filter(term == "location") |> 
      select(-term) |>
      separate(contrast, into = c("location_a", "location_b"), sep = "-") 
    tukey.groups <- HSD.test(anova.test, "location") 
    df.tukey.groups <- tukey.groups$groups |> 
      as_tibble(rownames = "location")
    
    # summarize growth rates before plotting
    df.growth.all.data <- df.growth.rates |> 
      group_by(location) |> 
      summarise(
        mu = mean(estimate),
        CI =  1.96 * sqrt(var(estimate)/n()),
        n_observation = n()
      ) |> 
      ungroup() |> 
      left_join(df.tukey.groups, by = "location") 
    
    # plot
    p.growth.all.data <- df.growth.all.data |> 
      rename(location_experiment = location) |> 
      left_join(df.location.labels, by = "location_experiment")  |> 
      select(-location_experiment) |> 
      mutate(
        location = fct_reorder(location, mu)
      ) |> 
      ggplot(aes(x = mu, y = location, fill = location)) +
      geom_col(alpha = 0.6) +
      geom_errorbarh(aes(xmin = mu - CI, xmax = mu + CI), height = 0.1) +
      scale_fill_manual(values = color.scheme) +
      geom_label(aes(x = mu + CI, y = location, label = groups), 
                 alpha = 0.4, hjust = -0.5) +
      labs(
        y = "",
        x = bquote(mu~~("h"^"-1")),
        fill = "Location"
      ) +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.01)),
        limits = c(0, 0.075)
      ) +
      theme_bw(font.size, base_family = figure.font.family) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
      ) 
    
    save_figures_manuscript(
      p.growth.all.data,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    
    p.growth.all.data
  }