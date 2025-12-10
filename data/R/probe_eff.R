#' Probe efficiency
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
#' @param method is the model for the probe efficiency. Default is
#' 'blunt pipe', based on Su WC and Vincent JH, Towards a general
#' semi-empirical model for the aspiration efficiencies of aerosol samplers
#' in perfectly calm air, Aerosol Science 35 (2004) 1119-1134
#' @param orient orientation of the probe. Options are 'u' for up,
#' 'd' for down, and 'h' for horizontal
#'
#' @returns data frame containing original particle distribution with added
#' data for this element
#'
#' @examples
#' df <- particle_dist() # set up particle distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'u') #probe orientation - draws upward
#' head(df)
#' @export
#'
probe_eff <- function(df, params, orient = "u", method = "blunt pipe") {
    # cat('This function replaces the eff_probe column if it already
    # exists') cat('\n')
    stopifnot(`Only blunt pipe method currently available` = method == "blunt pipe")

    R <- (df$D_p * 1e-04)^2 * 9.807/(18 * params$viscosity_air * params$velocity_air)
    # The stokes number in Su is different from our base Stk by 1/2
    p <- 2.2 * R^(1.3) * df$Stk/2
    q <- 75 * R^1.7 * df$Stk/2
    # ******** alpha, beta and B are from Ref 6: Thin-walled probe
    # facing downwards: alpha = 1; beta = 0; B = 1 Thin-walled probe
    # facing upwards: alpha = 0; beta = 1; B = 1 Horizontal
    # thin-walled probe: alpha = 0.8; beta = 0.2; B = 1

    alpha <- dplyr::case_when(orient == "u" ~ 0, orient == "d" ~ 1, orient ==
        "h" ~ 0.8)
    beta <- dplyr::case_when(orient == "u" ~ 1, orient == "d" ~ 0, orient ==
        "h" ~ 0.2)
    p <- 2.2 * R^(1.3) * df$Stk/2
    q <- 75 * R^1.7 * df$Stk/2
    B <- 1  #for all thin-walled probes
    df$eff_probe <- (1 - 0.8 * (4 * df$Stk/2 * R^(3/2)) + 0.08 * (4 * df$Stk/2 *
        R^(3/2))^2 - alpha * ((0.5 * R^(1/2)) - R * (B^2 - 1)) - beta *
        (0.12 * R^-0.4 * (exp(-p) - exp(-q)) - R^(3/2) * (B^(1/2) - 1)))
    # all the bonkers results set to zero
    df$eff_probe[which(df$D_p > 75)] <- 0
    df$eff_probe[which(df$eff_probe == "NaN")] <- 0
    df$eff_probe[which(df$eff_probe < 0)] <- 0  #correct for negative
    # ggplot(df, aes(D_p, eff_probe)) + geom_line() to compare to
    # reference results in Su and Vincent: ggplot(df, aes(Stk,
    # eff_probe)) + geom_point() + scale_x_log10()
    df
}

