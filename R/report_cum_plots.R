#' report on cumulative transport system efficiency (discrete particle sizes only)
#'
#' Report plots on models showing cumulative efficiencies established with
#' this package. See examples in the vignette.
#'
#' @param my_df is the particle data set after all element efficiencies
#' have been built in. Default is `df`.
#'
#' @param micron selects the particle size (aerodynamic mass activity
#' diameter in micrometers). Default is 5.
#'
#' @return A plot of cumulative transport efficiencies is generated in a plot window
#'
#' @examples
#' sys_df <- example_sys_df
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 60,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- particle_dist() |> # default particle distribution
#'       set_params_2(params) |>  #particle size-dependent parameters
#'       probe_eff(el_num = 1, my_params = params, my_sys_df = sys_df) |>
#'       tube_eff(el_num = 2, my_params = params, my_sys_df = sys_df) |>
#'       bend_eff(el_num = 3, my_params = params, my_sys_df = sys_df)
#' report_cum_plots(my_df = df, micron = 10)
#'
#' @export
#'
report_cum_plots <- function(my_df = df, micron = 5) {

    my_df <- my_df |> dplyr::filter(dist == "discrete")

    # Bind variables locally to satisfy R CMD check
    D_p <- sys_eff <- prob <- ambient <- bin_eff <- sampled <- . <-
      stats <- df <-  starts_with <- everything <- element <- efficiency <- dist <- NULL

    # make a cumulative efficiency set
    df_effs <- my_df |>
        dplyr::filter(D_p == micron) |>
        dplyr::select(., tidyselect::starts_with("eff_"))
    df_effs[1, ] <- cumprod(as.numeric(df_effs))
    names(df_effs) <- stringr::str_replace(names(df_effs), "eff_", "")

    # plot by element, by particle size
    df_effs <- df_effs |>
        tidyr::pivot_longer(cols = everything(), names_to = "element", values_to = "efficiency")

    df_effs$element <- factor(df_effs$element, levels = df_effs$element)

    plt <- ggplot2::ggplot(df_effs, ggplot2::aes(element, efficiency)) +
        ggplot2::geom_point(size = 3, alpha = 0.5) + ggthemes::theme_calc() +
        ggthemes::scale_color_gdocs() + ggplot2::guides(x = ggplot2::guide_axis(angle = 90)) +
        ggplot2::ggtitle("cumulative transport efficiency", subtitle = paste0(micron,
            " micrometer"))
    return(plt)
}
