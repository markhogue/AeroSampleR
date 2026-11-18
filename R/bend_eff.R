#' bend efficiency
#'
#' In order to run this function, first produce a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each.
#'
#' @param el_num element number to provide unique column names
#' @param my_df is the particle data set (data frame) established with the
#' `particle_dist` function. Default is `df`.
#' @param my_sys_df is a data frame describing the system. See [example_sys_df]. Default is `sys_df`
#' @param my_params is the parameter data set for parameters that are not
#' particle size-dependent
#' @param method choice of models: Pui or Zhang. Zhang is the default.
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
#' sys_df <- example_sys_df
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 60,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- particle_dist() |> # default particle distribution
#'       set_params_2(params) |>  #particle size-dependent parameters
#'       probe_eff(el_num = 1, my_params = params, my_sys_df = sys_df) |>
#'       tube_eff(el_num = 2, my_params = params, my_sys_df = sys_df) |>
#'       bend_eff(el_num = 3, my_params = params, my_sys_df = sys_df)
#'  message("displaying samples of 1003 lines of results")
#' (df[c(seq(100, 700, length.out = 5), 1001:1003), ])
#' @export
#'
bend_eff <- function(my_df = df,
                     el_num,
                     my_sys_df = sys_df,
                     my_params = params,
                     method = "Zhang") {

  # Bind variables locally to satisfy R CMD check
df <- sys_df <- params <- NULL

  # check that the element number describes a bend
  if (my_sys_df$el_type[el_num] != "bend"){
    message("The element specified does not appear to be a bend.\n")
    return()
  }

  # check that the bend angle is included
  if (is.na(my_sys_df$bend_angle[el_num])){
    message("The bend angle is missing.\n")
    return()
  }

  angle_rad <- my_sys_df$bend_angle[el_num] * pi / 180

  # check that the bend radius is included
  if (is.na(my_sys_df$bend_rad_cm[el_num])){
    message("The bend radius is missing.\n")
    return()
  }
  rat_curv <- my_sys_df$bend_rad_cm[el_num] / (my_params$D_tube / 2)


    if (method == "Pui") {
        ifelse(my_params$Re < 6000,
          eff_bend <- (1 + (my_df$Stk/0.171)^(0.452 *
          my_df$Stk/0.171 + 2.242))^-(2 * angle_rad/pi),
          eff_bend <- exp(-2.823 *
            my_df$Stk * angle_rad))
    }
    if (method == "Zhang") {
        eff_bend <- exp(-0.528 * angle_rad * my_df$Stk^((2)^(1/rat_curv)) *
            rat_curv^0.5)
    }

    eff_bend[eff_bend > 1] <- 1
    my_df <- cbind(my_df, eff_bend)
    names(my_df)[length(my_df)] <- paste0("eff_bend_", as.character(el_num))
    my_df
}
