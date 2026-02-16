te#'
#' Show the Process of PCA
#'
Show_PCA_Process <- function(x_center = 0, y_center = 0, x_range = 10, y_range = 4,
         rotEllipse = 30, n = 10, rotAxes = 30, seed = 13, ...) {

  set.seed(seed) # set here so same data set is used in each plot

  # all troubleshooting plot options turned off in these next functions
  res <- generate_2D_ellipse(x_center = x_center, y_center = y_center,
                             x_range = x_range, y_range = y_range,
                             angle = rotEllipse, n = n,
                             plot = FALSE, showOrig = FALSE, ...)
  res2 <- define_new_axes(res, rotAxes, FALSE)
  res3 <- project_data_onto_axes(res, res2, FALSE)

  # construct the plot manually
  xl <- range(res$x) + diff(range(res$x)) * c(-1, 1) * 0.1
  yl <- range(res$y) + diff(range(res$y)) * c(-1, 1) * 0.1
  xvals <- round(xl)
  yvals <- round(yl)
  lim <- min(xvals[1], yvals[1]):max(xvals[2], yvals[2])
  plot(lim, lim, type = "n", asp = 1,
       xlab = "x", ylab = "y", main = paste("Axes Angle =", rotAxes, sep = " "))

  # draw rotated ellipse
  points(res$x, res$y) 

  # draw reference lines
  abline(v = res$xc, h = res$yc, col = "gray90")
  abline(v = 0.0, h = 0.0, col = "gray90")

  # draw axes of ellipse
  abline(coef = c(res2[2], res2[1]))
  abline(coef = c(res2[4], res2[3]), col = "red")

  # add projected points
  points(res3$axis_1_projection[1,], res3$axis_1_projection[2,], pch = 19)
  points(res3$axis_2_projection[1,], res3$axis_2_projection[2,], pch = 19)

  # add all orthogonal projections (SAVE these 2 lines for now)
  # all projections can be shown, but is messy (though good for troubleshooting)
  # segments(res$x, res$y, res3$axis_1_projection[1,], res3$axis_1_projection[2,], col = "orange")
  # segments(res$x, res$y, res3$axis_2_projection[1,], res3$axis_2_projection[2,], col = "orange")

  # use the "special point" to display one set of projections
  spc <- res$sp
  segments(res$x[spc], res$y[spc],
    res3$axis_1_projection[1,spc], res3$axis_1_projection[2,spc], col = "orange")
  segments(res$x[spc], res$y[spc],
    res3$axis_2_projection[1,spc], res3$axis_2_projection[2,spc], col = "orange")

  # compute and display the variance
  var_total = var(res$xe) + var(res$ye)
  var_axis1 = var(res3$axis_1_projection[1,]) + var(res3$axis_1_projection[2,])
  var_axis2 = var(res3$axis_2_projection[1,]) + var(res3$axis_2_projection[2,])
  percent_axis1 = round(100 * var_axis1/var_total, 1)
  percent_axis2 = round(100 * var_axis2/var_total, 1)
  leg1 <- paste("var ax 1:", as.character(percent_axis1), sep = " ")
  leg2 <- paste("var ax 2:", as.character(percent_axis2), sep = " ")
  legend("topleft", legend = c(leg1, leg2), bty = "n")



}
