#'
#' Some Affine Transformations Helper Functions
#'
#' Matrix multiplications always assume 0,0 is the reference point.  In other words
#' the origin never moves.  One can compose or chain together combinations of rotations
#' and translations.
#'

#' common test data
set.seed(1)
# easy to visualize set of points centered on origin "The Big Dipper"
tm1 <- matrix(c(-1.0, 0.5, 0.25, -0.75,
                 0.5, 1.0, -0.75, -0.5,
                 rep(1.0, 4)),
                 byrow = TRUE, nrow = 3)
# same set of points, but not centered on the origin
tm2 <- tm1 + matrix(c(rep(2, ncol(tm1)),
                      rep(2, ncol(tm1)),
                      rep(0.0, ncol(tm1))), # note 0.0 don't want to modify 3rd row
                      byrow = TRUE, nrow = 3)

#'
#' Rotate a Matrix Through An Angle
#'
# tests
# net zero rotation by chaining calls
res <- rot_mat_2D(15) %*% rot_mat_2D(-15) %*% tm1
all.equal(tm1, res)

res <- rot_mat_2D(15) %*% rot_mat_2D(-15) %*% tm2
all.equal(tm2, res)

# graphical demo using tm1, centered on origin
plot(-5:5, -5:5, type = "n")
grid()
points(tm1[1,], tm1[2,], col = "black")
points(0, 0, col = "black", pch = 4)
segments(tm1[1,], tm1[2,], tm1[1, c(2:4, 1)], tm1[2, c(2:4, 1)])

res <- rot_mat_2D(30) %*% tm1
points(res[1,], res[2,], col = "red")
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")

# graphical demo using tm2, centered on 2,2
plot(-2:4, -2:4, type = "n")
grid()
points(tm2[1,], tm2[2,], col = "black")
points(2, 2, col = "black", pch = 4)
segments(tm2[1,], tm2[2,], tm2[1, c(2:4, 1)], tm2[2, c(2:4, 1)])

res <- rot_mat_2D(15) %*% tm2
points(res[1,], res[2,], col = "pink") # demonstrates rotation is about 0,0
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "pink")
segments(0, 0, tm2[1,3], tm2[2,3], col = "black", lty = 2)
segments(0, 0, res[1,3], res[2,3], col = "pink", lty = 2)

# translate to 0,0, rotate, translate back to origin
res <- trans_mat_2D(2, 2) %*% rot_mat_2D(45) %*% trans_mat_2D(-2, -2) %*% tm2
points(res[1,], res[2,], col = "red")
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")

#'
#' Translate a Matrix By a Given x, y Change
#'

# tests
# net zero translation by chaining calls
res <- trans_mat_2D(-2, -2) %*% trans_mat_2D(2, 2) %*% tm1
all.equal(tm1, res)

res <- trans_mat_2D(1.5, -2.5) %*% trans_mat_2D(-1.5, 2.5) %*% tm2
all.equal(tm2, res)

# move tm1 to tm2 (tm2 is offset by 2,2)
res <- trans_mat_2D(2, 2) %*% tm1
all.equal(tm2, res)

# graphical demo using tm1, centered on origin
plot(-5:5, -5:5, type = "n")
grid()
points(tm1[1,], tm1[2,], col = "black") # original data
points(0, 0, col = "black", pch = 4)
segments(tm1[1,], tm1[2,], tm1[1, c(2:4, 1)], tm1[2, c(2:4, 1)])

res <- trans_mat_2D(2, 2) %*% tm1
points(res[1,], res[2,], col = "red")
points(2, 2, col = "red", pch = 4) # mark manually computed "center"
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")

res <- trans_mat_2D(-4, 2) %*% tm1
points(res[1,], res[2,], col = "blue")
points(-4, 2, col = "blue", pch = 4)
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "blue")

res <- trans_mat_2D(0, -3) %*% tm1
points(res[1,], res[2,], col = "orange")
points(0, -3, col = "orange", pch = 4)
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "orange")

# graphical demo using tm2, not centered on origin
plot(-5:5, -5:5, type = "n")
grid()
points(tm2[1,], tm2[2,], col = "black")
points(2, 2, col = "black", pch = 4) # original data
segments(tm2[1,], tm2[2,], tm2[1, c(2:4, 1)], tm2[2, c(2:4, 1)])

res <- trans_mat_2D(-2, -2) %*% tm2 # translate back to 0, 0
points(res[1,], res[2,], col = "gray")
points(0, 0, col = "gray", pch = 4) # mark manually computed "center"
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "gray")

res <- trans_mat_2D(2, 2) %*% tm2
points(res[1,], res[2,], col = "red")
points(4, 4, col = "red", pch = 4)
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")

res <- trans_mat_2D(-4, 2) %*% tm2
points(res[1,], res[2,], col = "blue")
points(-2, 4, col = "blue", pch = 4)
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "blue")

res <- trans_mat_2D(0, -3) %*% tm2
points(res[1,], res[2,], col = "orange")
points(2, -1, col = "orange", pch = 4)
segments(res[1,], res[2,], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "orange")

#'
#' Project a Data Set Onto a Line
#'

# tests
p <- matrix(c(3, 5), ncol = 1)
v <- matrix(rnorm(20), nrow = 2)
tst <- project_data_onto_line(p, v)

plot(v[1,], v[2,], type = "p", pch = 20, asp = 1)
abline(v = 0, h = 0, col = "gray")
abline(coef = c(0.0, 5/3))
points(tst[1,], tst[2,], col = "red")
segments(v[1,], v[2,], tst[1,], tst[2,])

