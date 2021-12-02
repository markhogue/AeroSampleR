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
    log_int <- (log(log_norm_max) - log(log_norm_min)) / (n - 1)

  # log_int sets up micron sizes.
  particle_bins <- log_norm_min * exp(0:(n - 1) * log_int)

  particle_probs <- stats::dlnorm(particle_bins,
                    log(AMAD), # to start - replace with m
                    log(log_norm_sd))

  dist1 <- data.frame("D_p" = particle_bins,
                      "probs" = particle_probs)

  # activity is proportional to the cube of the radius
  dist1$rel_act <- (dist1$D_p / 2)^3 * dist1$probs

  close_minus <- function(m) {
    dist1$probs <- stats::dlnorm(particle_bins,
                   log(m), # to start - replace with m
                   log(log_norm_sd))
                   dist1$rel_act <- (dist1$D_p / 2)^3 * dist1$probs
                   dist1$D_p[max(which(cumsum(dist1$rel_act) / sum(dist1$rel_act) < 0.5))]
    }

  close_plus <- function(m) {
   dist1$probs <- stats::dlnorm(particle_bins,
   log(m), # to start - replace with m
   log(log_norm_sd))
   dist1$rel_act <- (dist1$D_p / 2)^3 * dist1$probs
   dist1$D_p[min(which(cumsum(dist1$rel_act) / sum(dist1$rel_act) >= 0.5))]
   }

   # with 1000 data points ranging 5e-4 to 100, all integers < 99 are included
   del <- 100
   m <- AMAD # mean set to AMAD at first - this is way too high
   while(del > 0.1) {
   (low_D <- close_minus(m))
   (high_D <- close_plus(m))
    del <- min(abs(c(AMAD - low_D, high_D - AMAD)))
   del
   cat("\n")
   m <- m * (AMAD / low_D)
   }
   cat("_mean_ diameter for AMAD (median activity) = ", m)

  # log_int sets up micron sizes.
  particle_bins <- log_norm_min * exp(0:(n - 1) * log_int)
  particle_probs <- stats::dlnorm(particle_bins,
                           log(m),
                           log(log_norm_sd))
  df <- data.frame("D_p" = particle_bins, "probs" = particle_probs)
  df$dist <- "log_norm"
  df <- rbind(df,
      data.frame("D_p" =  discrete_vals,
                 "probs" = rep(1, length(discrete_vals)),
                 "dist" =  rep("discrete", length(discrete_vals))))
  df
  }
