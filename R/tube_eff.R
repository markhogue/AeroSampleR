#' Tube efficiency
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
#' @param L tube length, m
#' @param angle_to_horiz angle to horizontal in degrees
#' @param elnum element number to provide unique column names
#'
#' @returns data frame containing original particle distribution with added
#' data for this element
#'
#' @examples
#' df <- particle_dist() #  distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
#' df <- tube_eff(df, params, L = 100,
#' angle_to_horiz = 90, elnum = 3)
#' head(df)
#'
#' @export
#'
tube_eff <- function(df, params, L, angle_to_horiz, elnum) {
    # cat('This function appends a new eff_tube.. column to the data
    # frame') cat('\n')

    angle_to_horiz_radians <- angle_to_horiz * pi/180
    # Depo_Calc Eq 46:
    Dc <- params$k * params$T_K * df$C_c/(3 * pi * params$viscosity_air *
        1e-06 * df$D_p)

    # Depo_Calc Eq 49: Schmidt number
    Sc <- params$viscosity_air/(params$density_air * Dc)

    # Depo_Calc Eq 45: diffusion time
    t_diffus <- pi * Dc * L/(params$Q_lpm/1000/60)

    # Depo_Calc Eq 47: Sherwood
    ifelse(params$Re < 2100, Sh <- 3.66 + 0.2672/(t_diffus + 1.0079 * t_diffus^(1/3)),
        Sh <- 0.0118 * params$Re^(7/8) * Sc^(1/3))

    # Depo_Calc Eq 44: efficiency after thermal diffusion deposition
    eff_therm <- exp(-t_diffus * Sh)

    # Depo_Calc Eq 31:
    t_prime <- L * df$v_ts/(params$velocity_air * params$D_tube) * cos(angle_to_horiz_radians)

    # Depo_Calc Eq 39:
    t_plus <- 0.0395 * df$Stk * params$Re^(3/4)

    # Depo_Calc Eq 42:
    V_plus <- 6e-04 * t_plus^2 + 2e-08 * params$Re

    # Depo_Calc Eq 43: particle deposition velocity
    Vt <- V_plus * params$velocity_air/5.03 * params$Re^(-1/8)

    # Depo_Calc Eq 38: efficiency after tubulent deposition
    eff_turb <- exp(-pi * t_prime * L * Vt/(params$Q_lpm/1000/60))

    # Depo_Calc Eq 30:
    K <- 3/4 * t_prime

    # Depo_Calc Eq 29: efficiency after gravitational settling
    eff_grav_lam <- 1 - (2/pi) * (2 * K * sqrt(1 - K^(2/3)) - K^(1/3) *
        sqrt(1 - K^(2/3)) + asin(K^(1/3)))
    eff_grav_lam[is.na(eff_grav_lam)] <- 0

    # Depo_Calc Eq 37:
    Z <- 4 * t_prime/pi

    # Depo_Calc Eq 36:
    eff_grav_turb <- exp(-Z)

    eff_tube <- dplyr::case_when(params$Re < 2100 ~ eff_grav_lam * eff_therm,
        params$Re > 4000 ~ eff_turb * eff_therm * eff_grav_turb, TRUE ~
            eff_grav_lam * eff_therm)

    if (params$Re >= 2100 && params$Re < 4000 && eff_tube > eff_turb * eff_therm *
        eff_grav_turb) {
        eff_tube <- eff_turb * eff_therm * eff_grav_turb
    }

    # rename eff_tube to provide unique column name
    df <- cbind(df, eff_tube)
    names(df)[length(df)] <- paste0("eff_tube_", as.character(elnum))
    df
}

