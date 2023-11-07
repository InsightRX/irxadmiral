#' Set global ggplot2 theme and default colors for geoms
#' 
#' @param theme theme name
#' 
#' @export
set_global_plot_theme <- function(theme = "irx") {

  if(theme == "irx") {
    ## IRX color scheme
    # blue      teal      navy      grey    white 
    # "#3870FA" "#35BCB1" "#193046" "#F2F2F0" "#ffffff" 
    
    ggplot2::theme_set(
      ggplot2::theme_classic() +
        ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = "black", color = "black"),
          strip.text = ggplot2::element_text(colour = "white")
        )
    )
    ggplot2::update_geom_defaults(
      "line", 
      ggplot2::aes(color = "#3870fa", alpha = 0.5)
    )
    ggplot2::update_geom_defaults(
      "point", 
      ggplot2::aes(color = "#193046", alpha = 0.5)
    )
    ggplot2::update_geom_defaults(
      "violin", 
      ggplot2::aes(fill = "#35bcb1", color="#35bcb1", alpha = 0.2)
    )
    ggplot2::update_geom_defaults(
      "boxplot", 
      ggplot2::aes(fill = "#35bcb1", color="#35bcb1", alpha = 0.2)
    )
    ggplot2::update_geom_defaults(
      "rect",
      ggplot2::aes(fill = "#35bcb1", color = "#35bcb1")
    )
  }
  
}
