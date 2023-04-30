#' Create figure S6 of interlab manuscript 
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
plot_figure_S6 <-
  function(all.datasets,
           df.location.labels,
           figures.dir,
           figure.file.name = "figure_S5",
           height = 14,
           width = 21,
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
    
    p.time.course <- df.pr.norm |> 
      left_join(df.strain.labels, by = "strain") |>
      ggplot(aes(x = time_h, y = norm_value, colour = location)) +
      geom_point(position = position_jitter(seed = 1989,  width = 0.05),
                 alpha = 0.3, size = 2) +
      facet_grid(induction~strain) +
      scale_color_manual(values = color.scheme) +
      theme(
        panel.grid.major.x = element_blank() 
      ) +
      labs(
        x = bquote("Time"~(h)),
        y = "nRFU",
        colour = "Lab"
      ) +
      theme_bw(font.size, base_family = figure.font.family) +
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