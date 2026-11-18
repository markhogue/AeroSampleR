#' report on transport efficiency
#'
#' A basic report on models established with this package. See
#' examples in the vignette.
#' @param my_df is the particle data set after all element efficiencies
#' have been built in. Default is `df`.
#' @param dist selects the distribution for the report. Options are
#' 'discrete' for discrete particle sizes or 'log' for the log-normal
#' distribution of particles.
#' @returns report of system efficiency
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
#' report_basic(my_df = df, dist = 'discrete')
#'
#' @export
report_basic <-function (my_df = df, dist) {

  # Bind variables locally to satisfy R CMD check
  D_p <- microns <- sys_eff <- prob <- ambient <- bin_eff <- sampled <- . <- starts_with <-  df <- params <- NULL

  # set up new columns
  eff_cols <- tidyselect::starts_with("eff_", vars = names(my_df))
  if (dist == "discrete") {
    df_disc <- dplyr::filter(my_df, dist == "discrete")
    df_disc$sys_eff <- apply(df_disc[, eff_cols], 1, prod)
    discrete_report <- dplyr::select(df_disc, D_p, sys_eff)
    return(discrete_report)
  }
  if (dist == "log") {
    df_log <- dplyr::filter(my_df, dist == "log_norm")
    df_log$bin_eff <- apply(df_log[, eff_cols], 1, prod)
    df_log$sampled <- df_log$prob * df_log$bin_eff
    data.frame(`activity fraction sampled` = sum(df_log$sampled)/sum(df_log$prob))
  }
}
