
#'
#' Rotate a Matrix Through An Angle
#'
#' @param angle Numeric.  Angle in degrees.  Positive values give CCW rotations.
#' @return A numeric matrix that should be used to pre-multiply a matrix of the
#'         data points to be rotated.  The data matrix should have rows of `x`, `y`,
#'         and `rep(1), length(x)` (i.e. augmented to give homogeneous coordinates).
#'
rot_mat_2D <- function(angle) {
  theta <- angle * pi / 180
  mat <- matrix(c(cos(theta), sin(theta), 0.0,
                  -sin(theta), cos(theta), 0.0,
                  0.0, 0.0, 1.0),
                  ncol = 3)
}

#'
#' Translate a Matrix By a Given x, y Change
#'
#' @param x Numeric.  Change in x-coordinate.
#' @param y Numeric.  Change in y-coordinate.
#' @return A numeric matrix that should be used to pre-multiply a matrix of the
#'         data points to be translated.  The data matrix should have rows of `x`, `y`,
#'         and `rep(1), length(x)` (i.e. augmented to give homogeneous coordinates).
#'
trans_mat_2D <- function(x, y) {
  mat <- matrix(c(1.0, 0.0, 0.0,
                  0.0, 1.0, 0.0,
                  x, y, 1.0),
                  ncol = 3)
}

#'
#' Project a Data Set Onto a Line
#'
#' https://math.stackexchange.com/a/62718/177376 or any number of other places
#' @param point Numeric column matrix containing one x,y value on the line.  It is
#'        assumed that the line goes through 0,0. Do not augment the matrix.
#' @param data Numeric matrix.  x,y in columns.  These points will be projected
#'        onto the line described by `point`. Do not augment the matrix.
#' @return A numeric matrix with x,y in columns.
#'
project_data_onto_line <- function(point, data) {
  # this requires that the reference line passes through 0,0 & point
  if (!inherits(point, "matrix")) stop("point must be a matrix with one column")
  if (!inherits(data, "matrix")) stop("data must be a matrix with one column")
  vec <- (tcrossprod(point) %*% data) / c(crossprod(point))
}


