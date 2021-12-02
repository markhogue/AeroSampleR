#' Set parameters (not particle size specific)
#'
#' Make a set of parameters that will be used throughout this package.
#' `set_params_1` sets all single parameters.
#' `set_params_2` adds particle-size-dependent parameters to the
#' particle distribution
#'
#' All parameters are to be in MKS units, except as noted.
#'
#' @param D_tube_cm Inside diameter of tubing in cm, no default
#' @param Q_lpm System flow in lpm, no default
#' @param T_C System temperature in Celsius
#' @param P_kPa System pressure in kPa (Pa is the MKS unit)
#'
#'
#' @return a data frame with singular parameters
#'
#' examples
#' params <- set_params_1("D_tube" = 2.54, "Q_lpm" = 100,
#' "T_C" = 25, "P_kPa" = 101.325)
#' t(params)
#'
#' @export
set_params_1 <- function(D_tube_cm,
                       Q_lpm,
                       T_C = 20,
                       P_kPa = 101.325) {
  S <-  110.56 #Sutherland constant, K
  CK <- 273.15

 D_tube <- D_tube_cm / 100
 Q_lpm <- Q_lpm #used for reporting and calculating v
 velocity_air <- Q_lpm / 1000 / 60 / # conversion from lpm to cfs
        (pi * (D_tube / 2)^2) #m/s
 T_C <- T_C
 T_K <- T_C + CK
 P_kPa <- P_kPa
 R_u <- 8314.471 #U gas constant J/kmol·K
 MW_air <- 28.962 #kg/kmol
 k <- 1.3807E-23 # N*m/K Boltzmann's Constant
 g <- 9.807 # m/s^2 gravitational acceleration
 density_par <- 1000 # kg/m^3 AMAD density

 #air density at ntp corrected to system T and P
 density_air <- 1.2041	*
                ((CK + 20) / (T_K) *
                 (P_kPa / 101.325))

 # Depo_Calc Eq 3:
 viscosity_air <- 1.716E-05 * #ref viscosity N-s/m2
               ((T_K) / 273.11)^1.5 *
               (273.11 + S) / (T_K + S)

 # Depo_Calc Eq 4: (in microns)
 mfp <- 1e6 * sqrt(pi/8) *
        (viscosity_air / 0.4987445) *
         sqrt(1 / (density_air * P_kPa * 1000))

 # Depo_Calc Eq 5: Reynolds number for flow (applying only to tube)
Re <- density_air * velocity_air * D_tube / viscosity_air


 params <- data.frame("D_tube" = D_tube,
                      "Q_lpm" = Q_lpm,
                      "velocity_air" = velocity_air,
                      "T_K" = T_K,
                      "P_kPa" = P_kPa,
                      "density_air" = density_air,
                      "viscosity_air" = viscosity_air,
                      "mfp" = mfp,
                      "density_par" = density_par,
                      "Re" = Re,
                      "k" = k)
 params
  }
