#'
#' Generate Data in a 2D Ellipse
#'
#' A rectangle (with its long dimension) aligned with the x-axis is filled
#' with random data, then trimmed to an ellipse.  This ellipse is then rotated
#' counter-clockwise by the specified angle relative to the x-axis.  This approach means
#' that one may end up with less than `n` points.
#'
#' @param x_center Numeric. Center of ellipse along x-axis.
#' @param y_center Numeric. Center of ellipse along y-axis.
#' @param x_range Numeric. The length of the major axis of the ellipse.
#' @param y_range Numeric. The length of the minor axis of the ellipse.
#' @param angle Numeric.  Angle in degrees to rotate the initial ellipse. Rotation is 
#'        counter-clockwise relative to the positive x-axis.
#' @param n Integer.  Number of data points to generate.
#' @param plot Logical. Should a plot be produced? Used for exploration and troubleshooting.
#' @param showOrig Logical. If plotting, should we show the original ellipse in addition to the
#'        rotated ellipse?  Sometimes it is handy to omit the original ellipse points.
#' @param dots Additional arguments to be passed to plot function.
#' @return A list containing the `x` and `y` coordinates of the data points, along with
#'         the values of `x_center`, `y_center`, and the row number of the  special point.
#'         UPDATE xe, ye
#' @authors David T. Harvey, Bryan A. Hanson
#'
generate_2D_ellipse <- function(x_center = 0,
                                y_center = 0,
                                x_range = 10,
                                y_range = 2,
                                angle = 30,
                                n = 10,
                                plot = FALSE,
                                showOrig = FALSE,
                                ...) {


  # Helper Function
  # Choose a nice point for showing projections
  # Need to do here because we are in the original coordinates system
  # @param DF Data frame with x and y coordinates
  choose_special_point <- function(DF) {
    DF$xy <- abs(DF$x * DF$y) # area of projection rectangle
    DF$ratio <- abs(DF$x / DF$y)
    DF <- DF[order(-DF$xy),]
    # use the largest area rectangle unless it is very tall/wide, then try the next largest
    spc <- NULL
    keep <- FALSE
    for (i in 1:nrow(DF)) {
      keep <- (DF$ratio[i] < 2) & (DF$ratio[i] > 0.5)
      if (keep) {
        spc <- as.numeric(rownames(DF)[i])
        # cat("spc is ", spc, " and was found at i = ", i, "\n")
        break # found it
      }
    }
    spc
  }

  # rename a few arguments for convenience
  x0 <- x_center
  y0 <- y_center
  xa <- x_range/2
  yb <- y_range/2

  # generate random points within the ellipsoid's boundaries by first
  # generating random points within rectangle that encompasses the ellipse
  x <- runif(n, min = -xa + x0, max = xa + x0)
  y <- runif(n, min = -yb + y0, max = yb + y0)

  # determine which points have x, y values inside the ellipse
  # using equation for ellipse; negative value for check means point
  # is inside of ellipse
  check <- (x - x0)^2 / xa^2 + (y - y0)^2 / yb^2 - 1
  keep <- which(check < 0)

  # extract set of x, y points inside of ellipse
  xe <- x[keep]
  ye <- y[keep]

  spc <- choose_special_point(data.frame(x = xe - x0, y = ye - y0))

  if (plot) cat(length(xe), "of", n, "original data points were in the final ellipse\n")

  # translate data to ellipse center, rotate data, then translate back to original spot
  raw <- matrix(c(xe, ye, rep(1, length(xe))), byrow = TRUE, nrow = 3)
  new_data <- trans_mat_2D(x0, y0) %*% rot_mat_2D(angle) %*% trans_mat_2D(-x0, -y0) %*% raw
  xrot <- new_data[1,]
  yrot <- new_data[2,]

  if (plot) {
    xl <- range(xrot, xe) + diff(range(xrot, xe)) * c(-1, 1) * 0.5
    yl <- range(yrot, ye) + diff(range(yrot, ye)) * c(-1, 1) * 0.5
    # xl <- range(xrot, xe)
    # yl <- range(yrot, ye)
    plot(xrot, yrot, asp = 1, xlim = xl, ylim = yl,
         xlab = "x", ylab = "y", type = "n", ...)
    abline(v = x_center, h = y_center, col = "gray90") # or maybe use grid() ?
    points(xrot, yrot, pch = 19, col = "black")
    if (showOrig) points(xe, ye, col = "red", pch = 19)
  }

  out <- list(
    "x" = xrot,
    "y" = yrot,
    "xc" = x_center,
    "yc" = y_center,
    "xe" = xe,
    "ye" = ye,
    "sp" = spc
  )
} # end of generate_2D_ellipse



