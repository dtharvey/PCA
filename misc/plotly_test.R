library(knitr)
library(plot3D)
library(plotly)
library(magrittr)

# colors and color names
pcdata_col <- "#3db7ed"
pcdata_colname <- "light blue"
pcproj_col <- "#f748a5"
pcproj_colname <- "pink"
pcaxis_col <- "#d55e00"
pcaxis_colname <- "brown"
xyzaxis_col <- "#000000"
xyzaxis_colname <- "black"

# set coordinates for center of ellipsoid
x0 <- 0
y0 <- 0
z0 <- 0

# set dimensions of ellipsoid relative to center; values chosen to
# make x-axis more important than y-axis, which is more important
# than the z-axis; thus pc1 is x-axis, pc2 is y-axis, pc3 = z-axis
xa <- 15
yb <- 9
zc <- 2

# generate set of random points within the ellipsoid's boundaries
# done by first generating random points within rectangular solid that
# encompasses the ellipsoid
set.seed(13)
x <- runif(400, min = -xa, max = xa)
y <- runif(400, min = -yb, max = yb)
z <- runif(400, min = -zc, max = zc)

# determine which points have (x,y,z) values that are inside the
# ellipsoid using equation for ellipsoid; a negative value for
# check means the point is inside of ellipsoid; flag as id
check <- (x - x0)^2 / xa^2 + (y - y0)^2 / yb^2 + (z - z0)^2 / zc^2 - 1
id <- which(check < 0)

# extract sets of (x,y,z) points inside of ellipsoid
xe <- x[id]
ye <- y[id]
ze <- z[id]

# function to rotate data and axes; a, b, and g are angles for rotation
# around the z, y, and x axes; see en.wikipedia.org/wiki/Rotation_matrix
rot <- function(a = 10, b = 10, g = 10, x = xe, y = ye, z = ze) {
  xrot <- cos(a) * cos(b) * x + (cos(a) * sin(b) * sin(g) - sin(a) * cos(g)) * y + (cos(a) * sin(b) * cos(g) + sin(a) * sin(g)) * z
  yrot <- sin(a) * cos(b) * x + (sin(a) * sin(b) * sin(g) + cos(a) * cos(g)) * y + (sin(a) * sin(b) * cos(g) - cos(a) * sin(g)) * z
  zrot <- -sin(b) * x + cos(b) * sin(g) * y + cos(b) * cos(g) * z
  out <- list(
    "xrot" = xrot,
    "yrot" = yrot,
    "zrot" = zrot
  )
}

# original pc axes (same as x,y,z axes)
xpc1 <- c(-xa, xa)
ypc1 <- c(0, 0)
zpc1 <- c(0, 0)
xpc2 <- c(0, 0)
ypc2 <- c(-xa, xa)
zpc2 <- c(0, 0)
xpc3 <- c(0, 0)
ypc3 <- c(0, 0)
zpc3 <- c(-xa, xa)

# rotate the pc axes
pc1 <- rot(x = xpc1, y = ypc1, z = zpc1)
pc2 <- rot(x = xpc2, y = ypc2, z = zpc2)
pc3 <- rot(x = xpc3, y = ypc3, z = zpc3)

# rotate the original data
rotdata <- rot()

# Visualizing Original Data

axis_width <- 8L
rdata <- as.data.frame(rotdata)
x_axis <- data.frame(xpc1, ypc1, zpc1)
y_axis <- data.frame(xpc2, ypc2, zpc2)
z_axis <- data.frame(xpc3, ypc3, zpc3)
fig <- plot_ly(
  name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
  marker = list(size = 2.0, color = pcdata_col)
) %>%
  add_markers() %>%
  add_trace(name = "x axis", data = x_axis, x = ~xpc1, y = ~ypc1, z = ~zpc1, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(width = axis_width, color = xyzaxis_col)) %>%
  add_trace(name = "y axis", data = y_axis, x = ~xpc2, y = ~ypc2, z = ~zpc2, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(width = axis_width, color = xyzaxis_col)) %>%
  add_trace(name = "z axis", data = z_axis, x = ~xpc3, y = ~ypc3, z = ~zpc3, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(width = axis_width, color = xyzaxis_col)) %>%
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "y"),
    zaxis = list(title = "z")
  ))
fig

# The First Principal Component
axis_width <- 8L
rdata <- as.data.frame(rotdata)
x_axis <- data.frame(xpc1, ypc1, zpc1)
y_axis <- data.frame(xpc2, ypc2, zpc2)
z_axis <- data.frame(xpc3, ypc3, zpc3)
fig <- plot_ly(
  name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
  marker = list(size = 2.0, color = okabe_ito[3])
) %>%
  add_markers() %>%
  add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = okabe_ito[1], width = axis_width)) %>%
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "y"),
    zaxis = list(title = "z")
  ))
fig

# The Second Principal Component

proj <- as.data.frame(rot(x = rep(0, length(id)), y = ye, z = ze))

# DFs to define 4 lines to draw "surface"
P1P2 <- data.frame(
  x = c(pc3$xrot[2], pc2$xrot[2]),
  y = c(pc3$yrot[2], pc2$yrot[2]),
  z = c(pc3$zrot[2], pc2$zrot[2])
)

P2P3 <- data.frame(
  x = c(pc2$xrot[2], pc3$xrot[1]),
  y = c(pc2$yrot[2], pc3$yrot[1]),
  z = c(pc2$zrot[2], pc3$zrot[1])
)

P3P4 <- data.frame(
  x = c(pc3$xrot[1], pc2$xrot[1]),
  y = c(pc3$yrot[1], pc2$yrot[1]),
  z = c(pc3$zrot[1], pc2$zrot[1])
)

P4P1 <- data.frame(
  x = c(pc3$xrot[2], pc2$xrot[1]),
  y = c(pc3$yrot[2], pc2$yrot[1]),
  z = c(pc3$zrot[2], pc2$zrot[1])
)

fig <- plot_ly(
  name = "data", proj, x = ~xrot, y = ~yrot, z = ~zrot,
  marker = list(size = 2.0, color = okabe_ito[3])
) %>%
  add_markers() %>%
  add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = okabe_ito[1], width = axis_width),
            visible = "legendonly") %>%
  # Add PC2 projection plane rectangle as 4 lines; no 3d polygon appears to exist in plotly.  Need 4 DFs to do this!
  add_trace(name = "line1", data = P1P2, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = okabe_ito[3], width = axis_width / 2)) %>%
  add_trace(name = "line2", data = P2P3, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = okabe_ito[3], width = axis_width / 2)) %>%
  add_trace(name = "line3", data = P3P4, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = okabe_ito[3], width = axis_width / 2)) %>%
  add_trace(name = "line4", data = P4P1, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = okabe_ito[3], width = axis_width / 2)) %>%
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "y"),
    zaxis = list(title = "z")
  ))
fig

# The Third Principal Component

fig <- plot_ly(
  name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
  marker = list(size = 2.0, color = okabe_ito[3])
) %>%
  add_markers() %>%
  add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, 
            mode = "lines", type = "scatter3d", inherit = FALSE, 
            line = list(color = okabe_ito[1], width = axis_width),
            visible = "legendonly") %>%
  add_trace(name = "PC2", data = pc2, x = ~xrot, y = ~yrot, z = ~zrot, 
            mode = "lines", type = "scatter3d", inherit = FALSE, 
            line = list(color = okabe_ito[4], width = axis_width),
            visible = "legendonly") %>%
  add_trace(name = "PC3", data = pc3, x = ~xrot, y = ~yrot, z = ~zrot, 
            mode = "lines", type = "scatter3d", inherit = FALSE, 
            line = list(color = okabe_ito[7], width = axis_width),
            visible = "legendonly") %>%
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "y"),
    zaxis = list(title = "z")
  ))
fig


