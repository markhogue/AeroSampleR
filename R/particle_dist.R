#' Create a particle distribution
#'
#' Needed as a first step in estimating system efficiency.
#' Make the data frame that will be used to estimate efficiency of
#' variously sized aerosol particles' transport through the sampling
#' system. To create your data, save this data to the global
#' environment as shown in the examples.
#'
#' All inputs are in micron AMAD, meaning:
#'      the aerodynamic diameter of a particle is the diameter of a
#'      standard density (1000 kg/m3) sphere that has the same
#'      gravitational settling velocity as the particle in question.
#' @param AMAD default is 5 based on ICRP 66
#' @param log_norm_sd default is 2.5 based on ICRP 66
#' @param log_norm_min default is 0.0005 based on ICRP 66
#' @param log_norm_max default is 100 based on ICRP 66
#' @param discrete_vals default is c(1, 5, 10)
#'
#' @examples
#' df <- particle_dist() # default
#' df <- particle_dist(AMAD = 4.4,
#'                     log_norm_sd = 1.8)
#' head(df)
#'
#' @return a data frame containing a lognormally distributed set of
#' particles and discrete particle sizes
#'
#' @export
#'
particle_dist <- function(AMAD = 5,
                          log_norm_sd = 2.5,
                          log_norm_min = 5e-4,
                          log_norm_max = 100,
                          discrete_vals = c(1, 5, 10)) {
  n <- 1000 # number of bins - have to be high to meet del target
  log_int <- (log(log_norm_max) - log(log_norm_min)) / (n - 1)
  particle_bins <- log_norm_min * exp(0:(n - 1) * log_int)
  particle_dens <- stats::dlnorm(particle_bins,
                    log(AMAD),
                    log(log_norm_sd))

  df <- data.frame("D_p" = particle_bins, "dens" = particle_dens)
  df$dist <- "log_norm"
  df <- rbind(df,
      data.frame("D_p" =  discrete_vals,
                 "dens" = rep(1, length(discrete_vals)),
                 "dist" =  rep("discrete", length(discrete_vals))))
  df
  }
