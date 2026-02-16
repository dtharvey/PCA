
# function to plot scores and loadings data
# file is object created using rot_axes

plot_rot_axes <- function(file, show_rotated = TRUE,
                          show_projections = TRUE,
                          show_simple_legend = TRUE,
                          show_full_legend = TRUE,
                          show_full_axes_legend = FALSE,
                          show_title = TRUE,
                          show_scores = TRUE,
                          show_loadings = TRUE) {

  # set colors
  axis2_col <- "#3db7ed"
  axis2_colname <- "light blue"
  axis1_col <- "#f748a5"
  axis1_colname <- "pink"
  loadings_col <- "#359b73"
  loadings_colname <- "green"
  origdata_col <- "#000000"
  origdata_colname <- "black"

  # determine maximum value for axis limits
  axis_max <- max(c(file$x_range, file$y_range))

  # plot original data
  plot(
    x = file$x_original, y = file$y_original, type = "p", asp = 1,
    xlim = c(-axis_max, axis_max), ylim = c(-axis_max, axis_max),
    xlab = "variable 2", ylab = "variable 1", pch = 19, col = origdata_col,
    cex = 1
  )

  # add original axes as dashed lines
  abline(v = 0, lwd = 1, lty = 2, col = axis1_col)
  abline(h = 0, lwd = 1, lty = 2, col = axis2_col)

  # add rotated axes
  if (show_rotated == TRUE) {
    if (file$angle == 0 | file$angle == 180) {
      abline(h = 0, lwd = 2, lty = 1, col = axis2_col)
      abline(v = 0, lwd = 2, lty = 1, col = axis1_col)
    } else if (file$angle == 90) {
      abline(h = 0, lwd = 2, lty = 1, col = axis1_col)
      abline(v = 0, lwd = 2, lty = 1, col = axis2_col)
    } else {
      abline(
        a = file$axis1_intercept, b = file$axis1_slope,
        lwd = 1, col = axis1_col
      )
      abline(
        a = file$axis2_intercept, b = file$axis2_slope,
        lwd = 1, col = axis2_col
      )
    }
  }

  # add projections of points onto rotated axes
  if (show_projections == TRUE) {
    points(x = file$axis1_x, y = file$axis1_y, pch = 19, col = axis1_col, cex = 0.75) ###
    points(x = file$axis2_x, y = file$axis2_y, pch = 19, col = axis2_col, cex = 0.75)
  }

  # calculate variances
  var_total <- var(file$x_original) + var(file$y_original)
  var_axis1 <- var(file$axis1_x) + var(file$axis1_y)
  var_axis2 <- var(file$axis2_x) + var(file$axis2_y)
  percent_axis1 <- round(100 * var_axis1 / var_total, 1)
  percent_axis2 <- round(100 * var_axis2 / var_total, 1)

  # add legend with just axis names
  if (show_simple_legend == TRUE & show_full_legend == FALSE) {
    legend(
      x = "topleft", legend = c("axis 1", "axis 2"),
      col = c(axis1_col, axis2_col), lwd = 2, lty = 2, bty = "n"
    )
  }

  # add legend with information on variances for axes
  if (show_full_axes_legend == TRUE) {
    axis1_text <- paste0("axis A: ", round(var_axis1, 1), " (", percent_axis1, "%)")
    axis2_text <- paste0("axis B: ", round(var_axis2, 1), " (", percent_axis2, "%)")
    legend(
      x = "topleft", legend = c("variances", axis1_text, axis2_text),
      bty = "n", text.col = c("black", axis1_col, axis2_col)
    )
  }

  # add legend with information on variances for principal components
  if (show_full_legend == TRUE) {
    axis1_text <- paste0("PC 1: ", round(var_axis1, 1), " (", percent_axis1, "%)")
    axis2_text <- paste0("PC 2: ", round(var_axis2, 1), " (", percent_axis2, "%)")
    legend(
      x = "topleft", legend = c("variances", axis1_text, axis2_text),
      bty = "n", text.col = c("black", axis1_col, axis2_col)
    )
  }

  # add main title with angle
  if (show_title == TRUE) {
    title(main = paste0("axes rotated by ", file$angle, "°"))
  }

  # show scores for one extreme point BAH grey lines version
  if (show_scores == TRUE) {
    min_id <- which.min(file$y_original)
    lines(
      x = c(file$x_original[min_id], file$axis1_x[min_id]),
      y = c(file$y_original[min_id], file$axis1_y[min_id]),
      lty = 1, lwd = 2, col = "gray80"
    )
    lines(
      x = c(file$x_original[min_id], file$axis2_x[min_id]),
      y = c(file$y_original[min_id], file$axis2_y[min_id]),
      lty = 1, lwd = 2, col = "gray80"
    )
  }

  # show loadings for first axis
  if (show_loadings == TRUE) {
    draw.arc(
      x = 0, y = 0, radius = axis_max / 1, deg1 = 90, deg2 = 90 - file$angle,
      lwd = 2, col = axis1_col, lty = 1
    )
    # points(x = axis_max, y = 0.1, pch = -9660, col = axis1_col)
    text(x = 7.1, y = 8.5, labels = expression(Theta), cex = 1.25, col = axis1_col)
    # arctext(x = "\U0398", center = c(0,0), cex = 1.25, col = axis1_col,
    # radius=axis_max/0.9, middle = (180 - file$angle)/2 * (pi/180),
    # stretch = 1, clockwise = TRUE)
    draw.arc(
      x = 0, y = 0, radius = axis_max / 1, deg1 = 0, deg2 = 90 - file$angle,
      lwd = 2, col = axis1_col, lty = 1
    )
    # points(x = 0.1, y = axis_max, pch = -9668, col = axis1_col)
    text(x = 10.7, y = 2.5, labels = expression(Phi), cex = 1.25, col = axis1_col)
    # arctext(x = "\U03A6", center = c(0,0), cex = 1.25, col = axis1_col,
    # radius=axis_max/0.9, middle = (90 - file$angle)/2 * (pi/180),
    # stretch = 1, clockwise = TRUE)
    theta2 <- round(cos(-file$angle * pi / 180), digits = 3)
    leg.txt <- c(
      "loadings",
      # paste0(expression(Theta), ": cos(",-file$angle,") = ",
      # round(cos(-file$angle * pi/180),digits = 3)),
      # paste0(expression(Phi), ": cos(",90-file$angle,") = ",
      # round(cos((90-file$angle) * pi/180),digits = 3)),
      TeX("$\\Theta : \\, cos(-60)$ = 0.50"),
      TeX("$\\Phi : \\, cos(30)$ = 0.866")
    )
    legend(x = "bottomright", legend = leg.txt, bty = "n", text.col = c("black", axis1_col, axis1_col))
  }
}
