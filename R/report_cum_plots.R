#' report on cumulative transport system efficiency
#'
#' In order to run a report, first produce a model of each individual
#' element. Start with producing a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each. Next, add elements
#' in the sample system until all are complete.
#'
#' @param df is the particle data set - after transport analysis by element
#' @param micron selects the particle size (aerodynamic mass activity
#' diameter in micrometers). This must be selected from the original
#' distribution of particles that were started with the `particle_dist`
#' function.
#'
#' @return A plot of cumulative transport efficiencies is generated in a plot window
#'
#' @examples
#' @donttest{report_cum_plots(dat_for_plots, micron = 10)}
#'
#' @export
#'
report_cum_plots <- function(df, micron) {

    D_p = microns = sys_eff = probs = ambient = bin_eff = sampled = . = starts_with = everything = element = efficiency = NULL

    # make a cumulative efficiency set
    df_effs <- df %>%
        dplyr::filter(D_p == micron) %>%
        dplyr::select(., tidyselect::starts_with("eff_"))
    df_effs[1, ] <- cumprod(as.numeric(df_effs))
    names(df_effs) <- stringr::str_replace(names(df_effs), "eff_", "")

    # plot by element, by particle size
    df_effs <- df_effs %>%
        tidyr::pivot_longer(cols = everything(), names_to = "element", values_to = "efficiency")


    df_effs$element <- factor(df_effs$element, levels = df_effs$element)

    plt <- ggplot2::ggplot(df_effs, ggplot2::aes(element, efficiency)) +
        ggplot2::geom_point(size = 3, alpha = 0.5) + ggthemes::theme_calc() +
        ggthemes::scale_color_gdocs() + ggplot2::guides(x = ggplot2::guide_axis(angle = 90)) +
        ggplot2::ggtitle("cumulative transport efficiency", subtitle = paste0(micron,
            " micrometer"))
    return(plt)
}

