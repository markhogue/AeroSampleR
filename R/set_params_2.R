#' Make a set of particle-size-dependent parameters
#'
#' This set of parameters will be used for evaluation of transport
#' efficiency for particle-size-dependent parameters.
#'
#' No user-selected arguments are needed. Parameters are used in
#' efficiency functions. For each particle diameter, an entry is
#' made in the data frame for the Cunningham slip correction factor,
#' the particle terminal velocity, the particle Reynold's number,
#' and the Stokes factor.
#'
#' `set_params_1` sets all single parameters.
#' `set_params_2` adds particle size-dependent parameters to the
#' particle distribution
#'
#' @param df is the particle data set (data frame) established with the
#' `particle_dist` function
#' @param params is the parameter data set for parameters that are not
#' particle size-dependent
#'
#' @examples
#' df <- particle_dist(n=10) # toy set for demo
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325)
#' df <- set_params_2(df, params)
#' head(df)
#'
#' @return a data frame starting with the submitted particle
#' distribution with additional columns for particle-size-dependent
#' parameters
#'
#' @export
#'
set_params_2 <- function(df, params) {
  # Author notes for documentation in Q-CLC-G-00128
  # Depo_Calc Eq 6: Cunningham Correction Factor
  # Depo Calc Eq 33: terminal settling velocity
  # Depo Calc Eq 32: Particle Reynolds number (tube)
  # Depo_Calc Eq 7: Stokes number
C_c = v_ts = sys_eff = NULL

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
