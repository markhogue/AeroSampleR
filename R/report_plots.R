#' plots of individual on transport system elements
#'
#' In order to run a report, first produce a model of each individual
#' element. Start with producing a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each. Next, add elements
#' in the sample system until all are complete.
#'
#' @param df is the particle data set - after transport analysis by element
#' @param dist selects the distribution for the report. Options are
#' 'discrete' for discrete particle sizes or 'log' for the log-normal
#' distribution of particles that were started with the `particle_dist`
#' function.
#'
#' @return A plot of transport efficiencies is generated in a plot window
#'
#' @examples
#' report_plots(dat_for_plots, dist = 'discrete')
#'
#' @export
#'
report_plots <- function(df, dist) {
    D_p = microns = sys_eff = dens = ambient = bin_eff = sampled = . = starts_with = everything = element = efficiency = amb_mass = rel_activity = location = NULL

    eff_cols <- tidyselect::starts_with("eff_", vars = names(df))

    if (dist == "discrete") {
        # plot by element, by particle size
        df_long <- df |>
            dplyr::filter(dist == "discrete") |>
            dplyr::select(., c(D_p, tidyselect::starts_with("eff_"))) |>
            tidyr::pivot_longer(cols = tidyselect::starts_with("eff_"),
                names_to = "element", values_to = "efficiency")

        df_long$element <- stringr::str_remove(df_long$element, "eff_")
        df_long$D_p <- as.factor(df_long$D_p)

        # This factor assignment retains the element order
        df_long$element <- factor(df_long$element, levels = unique(df_long$element))

        plt <- ggplot2::ggplot(df_long, ggplot2::aes(element, efficiency,
            color = D_p, shape = D_p)) + ggplot2::geom_point(size = 3, alpha = 0.5) +
            ggthemes::scale_color_gdocs() + ggplot2::guides(x = ggplot2::guide_axis(angle = 90)) +
            ggplot2::ggtitle("transport efficiency by element")
        return(plt)
    }

    if (dist == "log") {
        # mass weighted plot
      # make data frame of just the log data
      df_log <- df |>
        dplyr::filter(dist == "log_norm")

      # compute efficiency for each particle size (bin) and add this column

      df_log$bin_eff <- apply(df_log[, eff_cols], 1, prod)
      # compute ambient mass-based quantity for each bin

      df_log$ambient <- df_log$dens * 4/3 *
        pi * (df_log$D_p/2)^3 * c(0, diff(df_log$D_p))

      df_log$sampled <- df_log$ambient * df_log$bin_eff

      df_log$microns <-  df_log$D_p
      df_log |> dplyr::select(microns, ambient, sampled) |>
            tidyr::pivot_longer(2:3, names_to = "location",
                                values_to = "rel_activity") |>

      ggplot2::ggplot(ggplot2::aes(microns, rel_activity, color = location)) +
      ggplot2::geom_point() + ggplot2::ggtitle("ambient and sampled activity")
    }

}

