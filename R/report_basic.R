#' report on transport efficiency
#'
#' In order to run a report, first produce a model of each individual
#' element. Start with producing a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each. Next, add elements
#' in the sample system until all are complete.
#'
#' @param df is the particle data set (data frame) established with the
#' `particle_dist` function
#' @param params is the parameter data set for parameters that are not
#' particle size-dependent
#' @param dist selects the distribution for the report. Options are
#' 'discrete' for discrete particle sizes or 'log' for the log-normal
#' distribution of particles that were started with the `particle_dist`
#' function.
#'
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
#'       bend_eff(el_num = 3, my_params = params, my_sys_df = sys_df) |>
#' report_basic(params, dist = 'discrete')
#'
#' @export
report_basic <-function (df, params, dist) {
  D_p = microns = sys_eff = prob = ambient = bin_eff = sampled = . = starts_with = NULL
  eff_cols <- tidyselect::starts_with("eff_", vars = names(df))
  if (dist == "discrete") {
    df_disc <- dplyr::filter(df, dist == "discrete")
    df_disc$sys_eff <- apply(df_disc[, eff_cols], 1, prod)
    discrete_report <- dplyr::select(df_disc, D_p, sys_eff)
    return(discrete_report)
  }
  if (dist == "log") {
    df_log <- dplyr::filter(df, dist == "log_norm")
    df_log$bin_eff <- apply(df_log[, eff_cols], 1, prod)
    df_log$sampled <- df_log$prob * df_log$bin_eff
    data.frame(`activity fraction sampled` = sum(df_log$sampled)/sum(df_log$prob))
  }
}
