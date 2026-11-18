#' plots of individual on transport system elements
#'
#' Report plots on models established with this package. See
#' examples in the vignette.
#'
#' @param my_df is the particle data set after all element efficiencies
#' have been built in. Default is `df`.
#' @param dist selects the distribution for the report. Options are
#' 'discrete' for discrete particle sizes or 'log' for the log-normal
#' distribution of particles.
#'
#' @return A plot of transport efficiencies is generated in a plot window
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
#' report_plots(my_df = df, dist = 'discrete')
#'
#' @export
#'
report_plots <- function (my_df, dist) {

  # Bind variables locally to satisfy R CMD check
  all_of <- D_p <- microns <- sys_eff <- prob <- ambient <- bin_eff <- sampled <- . <- starts_with <- everything <- element <- efficiency <- amb_mass <- rel_activity <- location <- NULL

  eff_cols <- tidyselect::starts_with("eff_", vars = names(my_df))
  if (dist == "discrete") {

    df_long <- tidyr::pivot_longer(dplyr::select(
      dplyr::filter(my_df,
                    dist == "discrete"),
      all_of(.),
      c(D_p, tidyselect::starts_with("eff_"))),
      cols = tidyselect::starts_with("eff_"), names_to = "element",
      values_to = "efficiency")
    df_long$element <- stringr::str_remove(df_long$element, "eff_")
    df_long$D_p <- as.factor(df_long$D_p)
    df_long$element <- factor(df_long$element, levels = unique(df_long$element))
    plt <- ggplot2::ggplot(df_long,
                           ggplot2::aes(element, efficiency, color = D_p, shape = D_p)) +
      ggplot2::geom_point(size = 3, alpha = 0.5) +
      ggthemes::scale_color_gdocs() +
      ggplot2::guides(x = ggplot2::guide_axis(angle = 90)) +
      ggplot2::ggtitle("transport efficiency by element")
    return(plt)
  }
  if (dist == "log") {
    df_log <- dplyr::filter(my_df, dist == "log_norm")
    df_log$bin_eff <- apply(df_log[, eff_cols], 1, prod)
    df_log$sampled <- df_log$prob * df_log$bin_eff
    data.frame(`activity fraction sampled` = sum(df_log$sampled)/sum(df_log$prob))
    df_log$microns <- df_log$D_p
    ggplot2::ggplot(tidyr::pivot_longer(dplyr::select(df_log,
                                                      microns, prob, sampled),
                                        2:3,
                                        names_to = "location",
                                        values_to = "rel_activity"),
                    ggplot2::aes(microns, rel_activity, color = location)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle("ambient and sampled activity")
  }
}
