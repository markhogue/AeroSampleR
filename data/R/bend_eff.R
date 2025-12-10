#' bend efficiency
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
#' @param bend_angle bend angle in degrees
#' @param bend_radius bend radius in m
#' @param method choice of models: Pui or Zhang. Zhang is the default.
#' @param elnum element number to provide unique column names
#'
#' @references
#' Pusheng Zhang, Randy M. Roberts, André Bénard,
#' Computational guidelines and an empirical model for particle deposition
#' in curved pipes using an Eulerian-Lagrangian approach,
#' Journal of Aerosol Science, Volume 53, 2012, Pages 1-20,ISSN 0021-8502,
#' <doi:10.1016/j.jaerosci.2012.05.007>
#'
#' David Y. H. Pui, Francisco Romay-Novas & Benjamin Y. H. Liu (1987)
#' Experimental Study of Particle Deposition in Bends of Circular Cross
#' Section, Aerosol Science and Technology, 7:3, 301-315,
#' <doi:10.1080/02786828708959166>
#'
#' @returns data frame containing original particle distribution with added
#' data for this element
#'
#' @examples
#' df <- particle_dist() # set up particle distribution
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- set_params_2(df, params) #particle size-dependent parameters
#' df <- probe_eff(df, params, orient = 'h') #probe orientation - horizontal
#' df <- bend_eff(df, params, method='Zhang', bend_angle=90,
#' bend_radius=0.1, elnum=3)
#' head(df)
#' @export
#'
bend_eff <- function(df,
                     params,
                     method = "Zhang",
                     bend_angle,
                     bend_radius,
                     elnum) {
    # cat('This function appends a new eff_bend.. column to the data
    # frame') cat('\n')

    angle_rad <- bend_angle * pi/180
    rat_curv <- bend_radius/(params$D_tube/2)

    if (method == "Pui") {
        ifelse(params$Re < 6000,
          eff_bend <- (1 + (df$Stk/0.171)^(0.452 *
          df$Stk/0.171 + 2.242))^-(2 * angle_rad/pi),
          eff_bend <- exp(-2.823 *
            df$Stk * angle_rad))
    }
    if (method == "Zhang") {
        eff_bend <- exp(-0.528 * angle_rad * df$Stk^((2)^(1/rat_curv)) *
            rat_curv^0.5)
    }

    eff_bend[eff_bend > 1] <- 1
    df <- cbind(df, eff_bend)
    names(df)[length(df)] <- paste0("eff_bend_", as.character(elnum))
    df
}

