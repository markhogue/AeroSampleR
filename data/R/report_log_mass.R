#' report relative masses by particle of a log-normal distribution
#'
#' This function shows the entire table of results by particle diameter.
#'
#' @param df is the particle data set - after transport analysis by element
#'
#' @examples
#' df <- particle_dist() # set up particle distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
#' df <- bend_eff(df, params, method='Zhang', bend_angle=90,
#' bend_radius=0.1, elnum=3)
#' df <- tube_eff(df, params, L = 100,
#' angle_to_horiz = 90, elnum = 3)
#' report_log_mass(df)
#'
#' @returns data frame containing mass-based particle fractions in ambient
#' location and in distribution delivered through the system.
#'
#' @export
#'
report_log_mass <- function(df) {

    D_p = microns = sys_eff = dens = ambient = bin_eff = sampled = . = starts_with = everything = element = efficiency = amb_mass = sampled_mass = bin_frac_lost = total_frac_lost = dist = NULL

    # make data frame of just the log data
    df_log <- df |>
      dplyr::filter(dist == "log_norm")

    # compute efficiency for each particle size (bin) and add this column

    df_log$bin_eff <-
      purrr::pmap_dbl(dplyr::select(df_log,
              tidyselect::starts_with("eff_")), prod)

    # compute ambient mass-based quantity for each bin

    df_log$amb_mass <- df_log$dens * 4/3 *
      pi * (df_log$D_p/2)^3 * diff(c(0, df_log$D_p))

    df_log$sampled_mass <- df_log$amb_mass * df_log$bin_eff

    df_log$bin_frac_lost <-
      (df_log$amb_mass - df_log$sampled_mass) /
      df_log$amb_mass

    df_log$total_frac_lost <- (df_log$amb_mass -
                df_log$sampled_mass) /
                sum(df_log$amb_mass)

    dplyr::select(df_log, D_p, dens, bin_eff, amb_mass, sampled_mass, bin_frac_lost,
            total_frac_lost)

}

