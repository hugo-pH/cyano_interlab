#' Save figures to png, tiff and pdf
#'
#' @param plot A plot object
#' @param figure.file.name The name for the figure file
#' @param figures.dir The output directory
#' @param scaling Scaling value for ggsave
#' @param width The width of the output figure
#' @param height The height of the output figure
#' @param dpi The resolution of the output figure
#'
#' @return Three files (png, tiff and pdf) in the output directory
save_figures_manuscript <- function(plot, figure.file.name, figures.dir, 
                                    scaling = 1, width = width, height = height,
                                    dpi = dpi){

  figure.file.path <- file.path(figures.dir, figure.file.name)
  
  walk(c("png", "tiff"), ~ggsave(str_c(figure.file.path, .x, sep = "."), 
                                 plot, device = .x, width = width, height = height, 
                                 units = "cm", dpi = dpi, scale = scaling))
  
  ggsave(str_c(figure.file.path, "pdf", sep = "."), 
         plot, device = cairo_pdf, width = width, height = height, 
         units = "cm", dpi = dpi, scale = scaling)
  
}
