#'
#' Define a New Axis System
#'
#' @param angle Numeric. The angle between the original x, y axis system and
#'              the newly created/defined axis.  Defined as a counter-clockwise rotation
#'              from the positive x-axis, centered on the center of the original ellipse.
#' @param data List.  Output of `generate_2D_ellipse`.
#' @param plot Logical. If `TRUE` the new axes are added to an existing plot.
#' @return A named numeric vector giving the slopes and intercepts of the new axis system.
#'
#' @authors David T. Harvey, Bryan A. Hanson
#'
define_new_axes <- function(data = NULL, angle = 15, plot = FALSE) {

  # Describe new axes by defining slope & intercept (plot routines will accept these values)
  # _m = slope, _b = y-intercept; axis1 can be thought of as the original x-axis rotated
  # Key: Both axes will pass through xc, yc!
  if (angle == 0.0 | angle == 180.0) {
    axis1_m <- 0.0
    axis1_b <- data$yc
    axis2_m <- Inf
    axis2_b <- NaN
  } else if (angle == 90.0) {
    axis1_m <- Inf
    axis1_b <- NaN
    axis2_m <- 0.0
    axis2_b <- data$yc
  } else { # not any of the special cases above
    axis1_m <- tan(angle * pi / 180)
    axis1_b <- data$yc - axis1_m*data$xc 
    axis2_m <- -1.0/axis1_m
    axis2_b <- data$yc - axis2_m*data$xc

  # plot; the special cases above could be plotted if we use abline(v = 0.0) for instance
  # but the axes will overlap the original axes, so no real point in doing so.  Could add though.
    if (plot) {
      abline(coef = c(axis1_b, axis1_m), col = "black")
      abline(coef = c(axis2_b, axis2_m), col = "black")
      abline(v = 0.0, h = 0.0, col = "gray90")
    }
  }

  vec <- c(axis1_m, axis1_b, axis2_m, axis2_b)
  names(vec) <- c("axis 1 slope", "axis 1 intercept", "axis 2 slope", "axis 2 intercept")
  return(vec)
} # end of define_new_axis

