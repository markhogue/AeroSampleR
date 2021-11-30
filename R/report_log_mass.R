#' report relative masses by particle of a log-normal distribution
#'
#' In order to run a report, first produce a model of each individual
#' element. Start with producing a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each. Next, add elements
#' in the sample system until all are complete.
#'
#' @param df is the particle data set - after transport analysis by element
#' @param params is the parameter data set for parameters that are not
#' particle size-dependent
#' @param dist selects the distribution for the report. Options are
#' "discrete" for discrete particle sizes or "log" for the log-normal
#' distribution of particles that were started with the `particle_dist`
#' function.
#'
#' @examples
#' \dontrun{log_mass_results <- report_log_mass(df)}
#'
#' @returns data frame containing mass-based particle fractions in ambient
#' location and in distribution delivered throught the system.
#'
#' @export
#'
report_log_mass <- function(df, params, dist) {

    # mass weighted
    df %>%
      dplyr::filter(dist == "log_norm") %>%
      dplyr::mutate(bin_eff = purrr::pmap_dbl(
        dplyr::select(., starts_with('eff_')), prod)) %>%
      dplyr::mutate(amb_mass = probs * 4/3 * pi * (D_p / 2)^3) %>%
      dplyr::mutate(sampled_mass = amb_mass * bin_eff) %>%
      dplyr::mutate(microns = D_p) %>%
      dplyr::select(microns, probs, amb_mass, bin_eff, sampled_mass) %>%
      dplyr::mutate(bin_frac_lost = (amb_mass - sampled_mass) / amb_mass) %>%
      dplyr::mutate(total_frac_lost = (amb_mass - sampled_mass) / sum(amb_mass)) %>%
      dplyr::select(microns, probs, bin_eff, amb_mass, sampled_mass,
                    bin_frac_lost, total_frac_lost)

  }

