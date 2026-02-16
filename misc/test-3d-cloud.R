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

# pca results in case they are of interest
pc_results <- prcomp(data.frame(rotdata$xrot, rotdata$yrot, rotdata$zrot))

# colors and color names
pcdata_col <- "#3db7ed"
pcdata_colname <- "light blue"
pcproj_col <- "#f748a5"
pcproj_colname <- "pink"
pcaxis_col <- "#d55e00"
pcaxis_colname <- "brown"
xyzaxis_col <- "#000000"
xyzaxis_colname <- "black"

# plotting functions below; data manipulation functions above

# all points and xyz axes
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  phi = 10, theta = 50, ticktype = "detailed"
)
points3D(
  x = xpc1, y = ypc1, z = zpc1, type = "l", col = xyzaxis_col,
  lwd = 2, lty = 1, add = TRUE
)
points3D(
  x = xpc2, y = ypc2, z = zpc2, type = "l", col = xyzaxis_col,
  lwd = 2, lty = 1, add = TRUE
)
points3D(
  x = xpc3, y = ypc3, z = zpc3, type = "l", col = xyzaxis_col,
  lwd = 2, lty = 1, add = TRUE
)

# data cloud and projection onto xy, xz, and yz planes
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  phi = 10, theta = 50, ticktype = "detailed"
)
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rep(-xa, length(id)),
  pch = 19, col = pcproj_col, cex = 0.2, add = TRUE
)
scatter3D(
  x = rep(-xa, length(id)), y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcproj_col, cex = 0.2, add = TRUE
)
scatter3D(
  x = rotdata$yrot, y = rep(xa, length(id)), z = rotdata$zrot,
  pch = 19, col = pcproj_col, cex = 0.2, add = TRUE
)

# show first PC
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  phi = 10, theta = 50, ticktype = "detailed"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 1, add = TRUE
)

# show points onto plane
proj <- rot(x = rep(0, length(id)), y = ye, z = ze)
scatter3D(
  x = proj$xrot, y = proj$yrot, z = proj$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa)
)
polygon3D(
  x = c(pc3$xrot[2], pc2$xrot[2], pc3$xrot[1], pc2$xrot[1]),
  y = c(pc3$yrot[2], pc2$yrot[2], pc3$yrot[1], pc2$yrot[1]),
  z = c(pc3$zrot[2], pc2$zrot[2], pc3$zrot[1], pc2$zrot[1]),
  col = adjustcolor("white", alpha.f = 0.1), border = pcdata_col,
  add = TRUE
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 1, add = TRUE
)

# show PC2
scatter3D(
  x = proj$xrot, y = proj$yrot, z = proj$zrot,
  pch = 19, col = adjustcolor(pcdata_col, alpha.f = 0.5), cex = 0.2,
  ticktype = "detailed", phi = 10, theta = 50, bty = "b2",
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa)
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 1, add = TRUE
)
points3D(
  x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 2, add = TRUE
)

# show pC3
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  phi = 10, theta = 50, ticktype = "detailed"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 1, add = TRUE
)
points3D(
  x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 2, add = TRUE
)
points3D(
  x = pc3$xrot, y = pc3$yrot, z = pc3$zrot, type = "l", col = pcaxis_col,
  lwd = 2, lty = 3, add = TRUE
)

# final grid
old.par <- par(mfrow = c(2, 2))
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcdata_col, bty = "b2", cex = 0.2,
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  phi = 10, theta = 50, ticktype = "detailed",
  main = "(a) Original Data Cloud"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
scatter3D(
  x = proj$xrot, y = proj$yrot, z = proj$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  main = "(b) After Removing First PC"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
points3D(
  x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
proj2 <- rot(x = rep(0, length(id)), y = rep(0, length(id)), z = ze)
scatter3D(
  x = proj2$xrot, y = proj2$yrot, z = proj2$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  main = "(c) After Removing Second PC"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
points3D(
  x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
points3D(
  x = pc3$xrot, y = pc3$yrot, z = pc3$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
scatter3D(
  x = proj2$xrot, y = proj2$yrot, z = proj2$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2),
  main = "(d) Close Up of (c)"
)
par(old.par)
