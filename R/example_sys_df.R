#' @title example_sys_df
#'
#' @description example air sample system data
#'
#' @format a `data.frame` with the following columns:
#' \describe{
#' \item{system}{textual description of the system}
#' \item{el_num}{sequential numbers of the elements starting with the probe}
#' \item{el_type}{element type, either: probe, tube, or bend}
#' \item{length_cm}{for tubes, length of this element in cm}
#' \item{angle_to_horiz}{for tubes, angles in degrees of angle to horizontal}
#' \item{orient}{orientation of the probe, either: 'u' for up,
#' 'd' for down, and 'h' for horizontal}
#' \item{bend_angle}{for bends, angle of rotation in degrees}
#' \item{bend_rad_cm}{for bends, the bend radius in cm}
#' }
"example_sys_df"
