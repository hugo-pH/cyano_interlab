#' Create figure S3 of interlab manuscript 
#' 
#' Calculate relative OD if time 0 measurement based on reference measurement dataset,
#' estimate growth rates and compare to relative OD 
#'
#' @param all.datasets A list-column tibble with all datasets from experimental runs
#' @param reference.data A list-column tibble with all datasets from reference measurements
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
  function(experiments.data,
           reference.data,
           df.location.labels,
           figures.dir,
           figure.file.name = "figure_S3",
           height = 5,
           width = 18.5,
           dpi,
           scaling = 2,
           font.size, 
           figure.font.family) {

    df.spectrophotometer <- experiments.data |> 
      filter(data_id == "sp.od") |> 
      unnest(data) |> 
      select(-data_id) |> 
      rename(location_experiment = location) |> 
      left_join(df.location.labels, by = "location_experiment")  |> 
      select(-location_experiment) 
    
    df.sp.ref <- reference.data |> 
      filter(data_id == "sp.od") |> 
      unnest(data) |> 
      select(-data_id) |> 
      rename(location_normalization = location) |> 
      left_join(df.location.labels, by = "location_normalization")  |> 
      select(-location_normalization)
  
    
    color.scheme <- c("Amsterdam" = "#E69F00", 
                      "Edinburgh" = "#009E73",
                      "Berlin" = "#F0E442",
                      "Jena" = "#0072B2",
                      "Leipzig" = "#D55E00",
                      "Seville" = "#CC79A7",
                      "Tuebingen" = "#56B4E9",
                      "Duesseldorf (I)" = "#000000"
    )
    # extract the first measurements of normalization dataset
    df.ref.od.s <- df.sp.ref |> 
      filter(time_h == 0)
    
    df.sp.ref.st <- df.ref.od.s |> 
      group_by(location) |> 
      summarise(
        CI =  1.96 * sqrt(var(OD_730)/n()),
        OD_730 = mean(OD_730)
      ) |> 
      ungroup() |> 
      mutate(
        location = fct_reorder(location, OD_730)
      )  
    
    # normalize the assay time 0 measurements by the reference measurements
    df.norm.intial.od <- df.spectrophotometer |> 
      filter(time_h == 0) |> 
      left_join(df.sp.ref.st |> rename(ref_OD_730 = OD_730), by = "location") |> 
      mutate(
        norm_initial_OD_730 = OD_730 / ref_OD_730
      ) |> 
      group_by(location) |> 
      summarise(
        n = n(),
        CI =  1.96 * sqrt(var(norm_initial_OD_730)/n()),
        norm_initial_OD_730 = mean(norm_initial_OD_730, na.rm = T)
      ) |> 
      mutate(
        location = fct_reorder(location, norm_initial_OD_730)
      )
    
    # calculate growth rate and summarize by location
    df.growth.rates.summary <- df.spectrophotometer |> 
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
      unnest(r_squared) |> 
      group_by(location) |> 
      summarise(
        mu = mean(estimate),
        CI =  1.96 * sqrt(var(estimate)/n()),
        n_observation = n()
      )
    
    df.growth.initial.od <- left_join(
      df.growth.rates.summary,
      df.norm.intial.od |> rename(CI_OD_730 = CI),
      by = "location"
    ) |> 
      filter(!is.na(norm_initial_OD_730))
    
      p.od.ref <- df.sp.ref.st |> 
        ggplot(aes(x = OD_730, y = location, fill = location)) +
        geom_col(alpha = 0.6) +
        geom_errorbarh(aes(xmin =  OD_730 - CI, xmax = OD_730 + CI), height = 0.1) +
        scale_fill_manual(values = color.scheme) +
        labs(
          y = "",
          x = bquote("OD"[730]),
          fill = "Location"
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.01))
        ) +
        theme_bw(font.size, base_family = figure.font.family) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none"
        ) 
      
      
      
      p.norm.initial.od <- df.norm.intial.od |> 
        filter(!is.na(norm_initial_OD_730)) |> 
        ggplot(aes(x = norm_initial_OD_730, y = location, fill = location)) +
        geom_col(alpha = 0.6) +
        geom_errorbarh(aes(xmin =  norm_initial_OD_730 - CI, xmax = norm_initial_OD_730 + CI), height = 0.1) +
        scale_fill_manual(values = color.scheme) +
        labs(
          y = "",
          x = bquote("Relative OD"[730]),
          fill = "Location"
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.01)),
        ) +
        theme_bw(font.size, base_family = figure.font.family) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none"
        ) 
      
      p.cor.od.mu <- df.growth.initial.od |> 
        ggplot(aes(x = norm_initial_OD_730, y = mu)) +
        geom_point() +
        stat_smooth(method = "lm") +
        geom_errorbar(aes(ymin = mu - CI, ymax = mu + CI), width = 0.04) +
        geom_errorbarh(aes(xmin = norm_initial_OD_730 - CI_OD_730, xmax = norm_initial_OD_730 + CI_OD_730), height = 0.002) +
        labs(
          y = bquote(mu~~("h"^"-1")),
          x = bquote("Relative OD"[730])
        ) +
        theme_bw(font.size, base_family = figure.font.family) 
      
      p.norm.od <- {p.od.ref | p.norm.initial.od | p.cor.od.mu} + plot_annotation(tag_levels = "A")
      
    save_figures_manuscript(
      p.norm.od,
      figure.file.name = figure.file.name,
      figures.dir = figures.dir,
      width = width,
      height = height,
      dpi = dpi,
      scaling = scaling
    )
    
    p.norm.od
  }