#' Tube efficiency
#'
#' Computation is consistent with the approach described in Hogue, Mark;
#' Thompson, Martha; Farfan, Eduardo; Hadlock, Dennis, (2014),
#' "Hand Calculations for Transport of Radioactive Aerosols through
#' Sampling Systems" Health Phys 106, 5, S78-S87,
#' <doi:10.1097/HP.0000000000000092>, with the exception that the diffusion
#' deposition mechanism is included. (SRNS-STI-2012-00731 R1, Shorthand STI)
#'
#' Kulkarni is shorthand reference for Chapter 2 in Aerosol Measurement:
#' Principles, Techniques and Applications, Third Edition, Edited by Kulkarni P,
#' Baron PA, and Willeke K. Wiley, Hoboken NJ, 2011.
#'
#' Blunt is shorthand reference for Deposition Calculator Technical Description
#' Manual, BC-15-RP-003 version 1.1.1. 2015.
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
#' @param my_sys_df is a data frame describing the system. See [example_sys_df]
#' @param el_num element number of the tube in `sys_df`
#'
#' @returns data frame containing original particle distribution with added
#' data for this element
#'
#' @examples
#' # Example output is a sample of the full particle data set.
#'
#' # laminar flow (Reynolds number < 2100)
#'
#' sys_df <- example_sys_df
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 20,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- particle_dist() |> # default particle distribution
#'       set_params_2(params) |>  #particle size-dependent parameters
#'       probe_eff(el_num = 1, my_params = params, my_sys_df = sys_df) |>
#'       tube_eff(el_num = 2, my_params = params, my_sys_df = sys_df)
#'
#'  message("displaying samples of 1003 lines of results")
#' (df[c(seq(100, 700, length.out = 5), 1001:1003), ])

#' # turbulent flow (Reynolds number > 4000)
#'
#' sys_df <- example_sys_df
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- particle_dist() |> # default particle distribution
#'       set_params_2(params) |>  #particle size-dependent parameters
#'       probe_eff(el_num = 1, my_params = params, my_sys_df = sys_df) |>
#'       tube_eff(el_num = 2, my_params = params, my_sys_df = sys_df)
#'
#'  message("displaying samples of 1003 lines of results")
#' (df[c(seq(100, 700, length.out = 5), 1001:1003), -c(4:7)])
#'
#' # midrange flow (Reynolds number > 2100 and < 4000)
#'
#' sys_df <- example_sys_df
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 60,
#' "T_C" = 25, "P_kPa" = 101.325) #example system parameters
#' df <- particle_dist() |> # default particle distribution
#'       set_params_2(params) |>  #particle size-dependent parameters
#'       probe_eff(el_num = 1, my_params = params, my_sys_df = sys_df) |>
#'       tube_eff(el_num = 2, my_params = params, my_sys_df = sys_df)
#'
#'  message("displaying samples of 1003 lines of results")
#' (df[c(seq(100, 700, length.out = 5), 1001:1003), ])
#'
#' @export
#'
tube_eff <- function (my_df = df,
                      el_num,
                      my_sys_df = sys_df,
                      my_params = params
                      ) {

  # Bind variables locally to satisfy R CMD check
  df <- sys_df <- params <- NULL

  L_cm <- my_sys_df$length_cm[el_num]
  angle_to_horiz <- my_sys_df$angle_to_horiz[el_num]

  # check that the element number describes a tube
  if (my_sys_df$el_type[el_num] != "tube"){
    message("The element number specified does not appear to be a tube.\n")
    return()
  }

  # check that the tube length is included
  if (is.na(L_cm)){
    message("The length of the tube is not included in my_sys_df.\n")
    return()
  }
  # check that the element number describes a tube
  if (is.na(angle_to_horiz != "tube")){
    message("The tube's angle to horizontal seems to be missing.\n")
    return()
  }

   angle_to_horiz_radians <- angle_to_horiz * pi/180

  #tube length
  L <- L_cm/100

  # diffusion coefficient per Kulkarni Eq. 2-25
  Dc <- my_params$k * my_params$T_K * my_df$C_c/(3 * pi * my_params$viscosity_air *
                                          1e-06 * my_df$D_p)

 # Schmidt number per Kulkarni Eq. 2-28
  Sc <- my_params$viscosity_air/(my_params$density_air * Dc)

# Xi in SRNS-STI-2012-0073 R3
  t_diffus <- pi * Dc * L/(my_params$Q_lpm/1000/60)

# set the shape factor for laminar, turbulent cases
  ifelse(my_params$Re < 2100,
         Sh <- 3.66 + 0.2672/(t_diffus + 1.0079 * t_diffus^(1/3)),
         Sh <- 0.0118 * my_params$Re^(7/8) * Sc^(1/3))

  # thermal diffusion model. Ref Blunt Eq. 44
  eff_therm <- exp(-t_diffus * Sh)

  # residence time
  # v_ts is particle velocity, set with set_params2
  # Ref Blunt Eq. 31 - next three assignments
  t_prime <- L * my_df$v_ts/(my_params$velocity_air * my_params$D_tube) *
    cos(angle_to_horiz_radians)

  # for tubulent eddy transport efficiency t_plus is relaxation time
  t_plus <- 0.0395 * my_df$Stk * my_params$Re^(3/4)

  V_plus <- 6e-04 * t_plus^2 + 2e-08 * my_params$Re

  # particle deposition velocity Blunt Eq. 43
  Vt <- V_plus * my_params$velocity_air/5.03 * my_params$Re^(-1/8)

  # STI p 101
  eff_turb <- exp(-pi * t_prime * L * Vt/(my_params$Q_lpm/1000/60))

  # formerly e.grav.dep - STI page 94
  K <- 3/4 * t_prime

  # Blunt Eq. 29
  eff_grav_lam <- suppressWarnings(1 - (2/pi) *
                (2 * K * sqrt(1 - K^(2/3)) -                                  K^(1/3) * sqrt(1 - K^(2/3)) + asin(K^(1/3))))
  # fix the NAs.
  eff_grav_lam[is.na(eff_grav_lam)] <- 0

  # Blunt Eq. 37
  Z <- 4 * t_prime/pi

  # formerly STI page 94
    eff_grav_turb <- exp(-Z)

  # combine models where they apply - laminar or turbulent
  lam <- eff_grav_lam * eff_therm

  # eff_therm goes in both
  turb <- eff_turb * eff_therm * eff_grav_turb

  # Set up to pick which is lower because in the mixed Reynolds range, we pick
  # the lower of the two.
  lam_min <- lam < turb

  mixed <- rep(NA, length(my_df$D_p))

  mixed[which(lam_min == TRUE)] <- lam[which(lam_min == TRUE)]

  mixed[which(is.na(mixed))] <- turb[which(is.na(mixed))]

  eff_tube <- data.frame(eff_tube = rep(NA, length(my_df$D_p)))
  if(my_params$Re < 2100) eff_tube$eff_tube <- lam
  if(my_params$Re > 4000) eff_tube$eff_tube <- turb
  if(my_params$Re >= 2100 & my_params$Re <= 4000) eff_tube$eff_tube <- mixed
  my_df <- cbind(my_df, eff_tube)
  names(my_df)[length(my_df)] <- paste0("eff_tube_", as.character(el_num))
  my_df
}
