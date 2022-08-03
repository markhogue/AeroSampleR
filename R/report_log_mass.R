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

    D_p = microns = sys_eff = probs = ambient = bin_eff = sampled = . = starts_with = everything = element = efficiency = amb_mass = sampled_mass = bin_frac_lost = total_frac_lost = dist = NULL

    # mass weighted
    df |>
        dplyr::filter(dist == "log_norm") |>
        dplyr::mutate(bin_eff = purrr::pmap_dbl(dplyr::select(., starts_with("eff_")),
            prod)) |>
        dplyr::mutate(amb_mass = probs * 4/3 * pi * (D_p/2)^3) |>
        dplyr::mutate(sampled_mass = amb_mass * bin_eff) |>
        dplyr::mutate(microns = D_p) |>
        dplyr::select(microns, probs, amb_mass, bin_eff, sampled_mass) |>
        dplyr::mutate(bin_frac_lost = (amb_mass - sampled_mass)/amb_mass) |>
        dplyr::mutate(total_frac_lost = (amb_mass - sampled_mass)/sum(amb_mass)) |>
        dplyr::select(microns, probs, bin_eff, amb_mass, sampled_mass, bin_frac_lost,
            total_frac_lost)

}

