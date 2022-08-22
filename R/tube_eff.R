#' Tube efficiency
#'
#' Computation is consistent with the approach described in Hogue, Mark;
#' Thompson, Martha; Farfan, Eduardo; Hadlock, Dennis, (2014),
#' "Hand Calculations for Transport of Radioactive Aerosols through
#' Sampling Systems" Health Phys 106, 5, S78-S87,
#' <doi:10.1097/HP.0000000000000092>, with the exception that the diffusion
#' deposition mechanism is included.
#'
#' In order to run this function, first produce a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each.
#'
#' @param df is the particle data set (data frame) established with the
#' `particle_dist` function
#' @param params is the parameter data set for parameters that are not
#' particle size-dependent
#' @param L_cm tube length, cm
#' @param angle_to_horiz angle to horizontal in degrees
#' @param elnum element number to provide unique column names
#'
#' @returns data frame containing original particle distribution with added
#' data for this element
#'
#' @examples
#'
#' # laminar flow (Reynolds number < 2100)
#'
#' df <- particle_dist() #  distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 20,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
#' df <- tube_eff(df, params, L_cm = 100,
#' angle_to_horiz = 90, elnum = 2)
#' head(df)

#' # turbulent flow (Reynolds number > 4000)
#'
#' df <- particle_dist() #  distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
#' df <- tube_eff(df, params, L_cm = 100,
#' angle_to_horiz = 90, elnum = 2)
#' head(df)
#'
#' # midrange flow (Reynolds number > 2100 and < 4000)
#'
#' df <- particle_dist() #  distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 60,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
#' df <- tube_eff(df, params, L_cm = 100,
#' angle_to_horiz = 90, elnum = 2)
#' head(df)
#'
#' @export
#'
tube_eff <- function(df, params, L_cm, angle_to_horiz, elnum) {
    # convert angle from degrees to radians
    angle_to_horiz_radians <- angle_to_horiz * pi/180

    L <- L_cm / 100 # L is in meters
# assign some factors for use as needed:

    # diffusion coefficient
    Dc <- params$k * params$T_K * df$C_c/(3 * pi * params$viscosity_air *
        1e-06 * df$D_p)

    # Schmidt number
    Sc <- params$viscosity_air/(params$density_air * Dc)

    # diffusion time
    t_diffus <- pi * Dc * L/(params$Q_lpm/1000/60)

    # Sherwood number (ratio of the convective mass transfer to the rate of diffusive mass transport)
    ifelse(params$Re < 2100,
    Sh <- 3.66 + 0.2672/(t_diffus + 1.0079 * t_diffus^(1/3)),
    Sh <- 0.0118 * params$Re^(7/8) * Sc^(1/3))

    # efficiency after THERMAL DIFFUSION deposition
    eff_therm <- exp(-t_diffus * Sh)

    #
    t_prime <- L * df$v_ts/(params$velocity_air * params$D_tube) * cos(angle_to_horiz_radians)

    #
    t_plus <- 0.0395 * df$Stk * params$Re^(3/4)

    #
    V_plus <- 6e-04 * t_plus^2 + 2e-08 * params$Re

    # particle deposition velocity
    Vt <- V_plus * params$velocity_air/5.03 * params$Re^(-1/8)

    # efficiency after TURBULENT deposition
    eff_turb <- exp(-pi * t_prime * L * Vt/(params$Q_lpm/1000/60))

    #
    K <- 3/4 * t_prime

    # efficiency after GRAVITATIONAL SETTLING
    # laminar
    eff_grav_lam <- 1 - (2/pi) * (2 * K * sqrt(1 - K^(2/3)) - K^(1/3) *
        sqrt(1 - K^(2/3)) + asin(K^(1/3)))
    eff_grav_lam[is.na(eff_grav_lam)] <- 0

    #
    Z <- 4 * t_prime/pi

    # efficiency after GRAVITATIONAL SETTLING
    # turbulent
    eff_grav_turb <- exp(-Z)

    # efficiency with laminar flow
    lam <- eff_grav_lam * eff_therm

    # efficiency with turbulent flow
    turb <- eff_turb * eff_therm * eff_grav_turb

    # in between, use lower of the two
    lam_min <- lam < turb
    mixed <- dplyr::case_when(
      lam_min == TRUE ~ lam,
      TRUE ~ turb
    )


  eff_tube <- dplyr::case_when(
    # laminar flow
    params$Re < 2100 ~ lam,

    # turbulent flow
    params$Re > 4000 ~ turb,

    # not clearly laminar or turbulent
    TRUE ~ mixed)

  # add a colmn for latest efficiency
    df <- cbind(df, eff_tube)

  # rename eff_tube to provide unique column name
    names(df)[length(df)] <- paste0("eff_tube_", as.character(elnum))
    df
}
