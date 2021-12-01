#' report relative masses by particle of a log-normal distribution
#'
#' This function shows the entire table of results by particle diameter.
#'
#' @param df is the particle data set - after transport analysis by element
#'
#' @examples
#' \dontrun{log_mass_results <- report_log_mass(df)}
#'
#' @returns data frame containing mass-based particle fractions in ambient
#' location and in distribution delivered through the system.
#'
#' @export
#'
report_log_mass <- function(df) {

    D_p = microns = sys_eff = probs = ambient = bin_eff = sampled = . = starts_with = everything = element = efficiency = amb_mass = sampled_mass = bin_frac_lost = total_frac_lost = NULL

    # mass weighted
    df %>%
        dplyr::filter(dist == "log_norm") %>%
        dplyr::mutate(bin_eff = purrr::pmap_dbl(dplyr::select(., starts_with("eff_")),
            prod)) %>%
        dplyr::mutate(amb_mass = probs * 4/3 * pi * (D_p/2)^3) %>%
        dplyr::mutate(sampled_mass = amb_mass * bin_eff) %>%
        dplyr::mutate(microns = D_p) %>%
        dplyr::select(microns, probs, amb_mass, bin_eff, sampled_mass) %>%
        dplyr::mutate(bin_frac_lost = (amb_mass - sampled_mass)/amb_mass) %>%
        dplyr::mutate(total_frac_lost = (amb_mass - sampled_mass)/sum(amb_mass)) %>%
        dplyr::select(microns, probs, bin_eff, amb_mass, sampled_mass, bin_frac_lost,
            total_frac_lost)

}

