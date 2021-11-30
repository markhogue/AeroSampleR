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
#' "discrete" for discrete particle sizes or "log" for the log-normal
#' distribution of particles that were started with the `particle_dist`
#' function.
#'
#' @returns report of system efficiency
#'
#' @examples
#' \dontrun{report_eff(df, params, dist = "discrete")}
#'
#' @export
report_basic <- function(df, params, dist) {

  # # housekeeping to avoid no visible binding warnings
  # #. = NULL - try this if below doesn't work
  # utils::globalVariables(c("D_p", "microns", "sys_eff", "probs",
  #                   "ambient", "bin_eff", "sampled", "."))

 # provide parameter details
  cat("System Parameters")
  cat("\n")
  cat("Notes: D_tube is in m.")
  cat("\n")
  cat("Q_lpm is system flow in liters per minute.")
  cat("\n")
  cat("velocity_air is the derived air flow velocity in meters per second.")
  cat("T_K is system temperature in Kelvin.")
  cat("\n")
  cat("P_kPa is system pressure in kiloPascals.")
  cat("\n")
  cat("Re is the system Reynolds number, a measure of turbulence.")
  cat("\n")
  utils::str(params[c(1, 2, 3, 4, 5, 10)])

  if(dist == "discrete") {
    discrete_report <- df %>%
      dplyr::mutate(sys_eff = purrr::pmap_dbl(
      dplyr::select(., tidyselect::starts_with('eff_')), prod)) %>%
      dplyr::filter(dist == "discrete") %>%
      dplyr::mutate(microns = D_p) %>%
      dplyr::select(microns, sys_eff)

    return(discrete_report)
    }

   if(dist == "log") {
    df %>%
      dplyr::filter(dist == "log_norm") %>%
      dplyr::mutate(bin_eff =
                      purrr::pmap_dbl(
                        dplyr::select(., starts_with('eff_')), prod)) %>%
      dplyr::mutate(ambient = probs * 4/3 * pi * (D_p / 2)^3) %>%
      dplyr::mutate(sampled = ambient * bin_eff) %>%
      dplyr::mutate(microns = D_p) %>%
      dplyr::select(microns, probs, ambient, bin_eff, sampled)%>%
      dplyr::summarize(eff_mass_weighted = sum(sampled) /
                         sum(ambient))
  }
}

