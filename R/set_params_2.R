#' Make a set of parameters that will be used throughout this package.
#' Carry these parameters forward in the data set with the particle
#' distribution.
#' `set_params_1` sets all single parameters.
#' `set_params_2` adds particle size-dependent parameters to the particle
#' distribution
#'
#' All parameters are to be in MKS units, except as noted.
#'
#' @param df is the particle data set (data frame) established with the
#' `particle_dist` function
#' @param params is the parameter data set for parameters that are not
#' particle size-dependent
#'
#' @examples
#' \dontrun{df <- set_params_2(df, params)}
#'
#' @return a data frame starting with the submitted particle distribution
#'  with additional columns for particle-size-dependent parameters
#'
#' @export
#'
set_params_2 <- function(df, params) {

  # Depo_Calc Eq 6: Cunningham Correction Factor
  # Depo Calc Eq 33: terminal settling velocity
  # Depo Calc Eq 32: Particle Reynolds number (tube)
  # Depo_Calc Eq 7: Stokes number


df <- df |> dplyr::mutate(
  C_c = 1 + params$mfp / df$D_p *
    (2.34 + 1.05 * exp(-0.39 * df$D_p)),

  v_ts = params$density_par * 9.807 * (1e-6 * df$D_p)^2 * C_c /
       (18 * params$viscosity_air),

  # C_d = 24 / -- skipping due to circular references (Re_p)
  #
  # v_ts_turb = sqrt(4 * params$density_par * 9.807 * (1e-6 * df$D_p)^2 /
  #   (3 * C_d * params$density_air)),


  Re_p = params$density_air * v_ts * 1e-6 * df$D_p /
    params$viscosity_air,

  Stk = C_c * params$density_par * (1e-6 * df$D_p)^2 * params$velocity_air /
    (9 * params$viscosity_air * params$D_tube))

df
  }
