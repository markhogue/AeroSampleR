#' Probe efficiency for open pipe style occupational air sample inlet
#'
#' The efficiency is calculated based on a 'blunt pipe' from Su WC and
#' Vincent JH, Towards a general semi-empirical model for the aspiration
#' efficiencies of aerosol samplers in perfectly calm air, Aerosol Science 35
#' (2004) 1119-1134.
#'
#' In order to run this function, first produce a particle distribution
#' with the `particle_dist` function, then produce a parameter set with
#' the `set_params` function. Both of these results must be stored as
#' per examples described in the help set with each.
#'
#' @param my_df is the particle data set (data frame) established with the
#' `particle_dist` function. Default is `df`.
#' @param my_params is the parameter data set for parameters that are not
#' particle size-dependent. Default is `params`.
#' @param my_sys_df is a data frame describing the system. See [example_sys_df]. Default is `sys_df`.
#' @param el_num element number of the tube in `my_sys_df`. Default = 1
#'
#' @returns data frame containing original particle distribution with added
#' data for this element, an open pipe collection point.
#'
#' @references Su, W., & H. Vincent, J. (2004). Towards a general semi-empirical
#' model for the aspiration efficiencies of aerosol samplers in perfectly calm
#' air. Journal of Aerosol Science, 35(9), 1119-1134.
#' https://doi.org/10.1016/j.jaerosci.2004.04.003

#'
#' @examples
#' sys_df <- example_sys_df
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- particle_dist() |> # set up particle distribution
#'        set_params_2(params) |>  #particle size-dependent parameters
#'        probe_eff(el_num = 1, my_params = params, my_sys_df = sys_df)
#' head(df)
#' @export
#'
probe_eff <- function (my_df = df,
                       el_num = 1,
                       my_sys_df = sys_df,
                       my_params = params
) {

  # Bind variables locally to satisfy R CMD check
  df <- sys_df <- params <- NULL

  # Bind variables locally to satisfy R CMD check
  orient <- my_sys_df$orient[el_num]

  # check that the element number describes a tube
  if (my_sys_df$el_type[el_num] != "probe"){
    message("The element specified does not appear to be a probe.\n")
    return()
  }

  # check that the tube length is included
  if (is.na(my_sys_df$orient[el_num])){
    message("The orientation of the probe is missing.\n")
    return()
  }

    R <- (my_df$D_p * 1e-04)^2 * 9.807/(18 * my_params$viscosity_air * my_params$velocity_air)
    # The stokes number in Su is different from our base Stk by 1/2
    p <- 2.2 * R^(1.3) * my_df$Stk/2
    q <- 75 * R^1.7 * my_df$Stk/2
    # ******** alpha, beta and B are from Ref 6: Thin-walled probe
    # facing downwards: alpha = 1; beta = 0; B = 1 Thin-walled probe
    # facing upwards: alpha = 0; beta = 1; B = 1 Horizontal
    # thin-walled probe: alpha = 0.8; beta = 0.2; B = 1

    alpha <- dplyr::case_when(orient == "u" ~ 0, orient == "d" ~ 1, orient ==
        "h" ~ 0.8)
    beta <- dplyr::case_when(orient == "u" ~ 1, orient == "d" ~ 0, orient ==
        "h" ~ 0.2)
    p <- 2.2 * R^(1.3) * my_df$Stk/2
    q <- 75 * R^1.7 * my_df$Stk/2
    B <- 1  #for all thin-walled probes
    eff_probe <- (1 - 0.8 * (4 * my_df$Stk/2 * R^(3/2)) + 0.08 * (4 * my_df$Stk/2 *
        R^(3/2))^2 - alpha * ((0.5 * R^(1/2)) - R * (B^2 - 1)) - beta *
        (0.12 * R^-0.4 * (exp(-p) - exp(-q)) - R^(3/2) * (B^(1/2) - 1)))
    # all the bonkers results set to zero
    eff_probe[which(my_df$D_p > 75)] <- 0
    eff_probe[which(eff_probe == "NaN")] <- 0
    eff_probe[which(eff_probe < 0)] <- 0  #correct for negative
    my_df <- cbind(my_df, eff_probe)
    names(my_df)[length(my_df)] <- paste0("eff_probe_", as.character(el_num))
    my_df
}

