#' System parameters
#'
#' @param params is the parameter set established as output of `set_params_1`.
#'      (`set_params_2` adds particle size-dependent parameters to the
#' particle distribution. These are not included here.
#'
#' @return a data frame with the system parameter variables, their values and their units
#'
#' examples
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325)
#' params_report()
#'
#' @export
#'
report_params <- function(params) {
  params |>
    tidyr::pivot_longer(cols = 1:11) |>
    cbind("units" = c("meters",
                      "liters per minute",
                      "meters per second",
                      "Kelvin",
                      "kPa",
                      "kg per cubic meter",
                      "Pa-s",
                      "micrometer",
                      "kg per cubic meter",
                      "unitless Reynolds number",
                      "unitless Boltzmann constant"))
}
