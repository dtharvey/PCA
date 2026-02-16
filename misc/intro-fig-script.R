# script for introduction figure

# load data
allSpec <- read_csv("data/allSpec.csv", show_col_types = FALSE)
load("data/co_stds.RData")
load("data/cr_stds.RData")
load("data/cu_stds.RData")
load("data/ni_stds.RData")

# find index for each analyte's wavelength for maximum absorbance
cu.id = which.max(cu_stds2$cu_std1)
ni.id = which.max(ni_stds2$ni_std1)
cr.id = which.max(cr_stds$cr_std1)
co.id = which.max(co_stds$co_std1)

#find wavelength of maximum absorbance for each analyte
lambda_1 = cu_stds2$wavelength[cu.id]
lambda_2 = ni_stds2$wavelength[ni.id]
lambda_3 = cr_stds$wavelength[cr.id]
lambda_4 = co_stds$wavelength[co.id]

# find absorbance for each analyte at copper's maximum wavelength
abs_cu_1 = cu_stds2$cu_std1[cu.id]
abs_ni_1 = ni_stds2$ni_std1[cu.id]
abs_cr_1 = cr_stds$cr_std1[cu.id]
abs_co_1 = co_stds$co_std1[cu.id]

# find absorbance for each analyte at nickel's maximum wavelength
abs_cu_2 = cu_stds2$cu_std1[ni.id]
abs_ni_2 = ni_stds2$ni_std1[ni.id]
abs_cr_2 = cr_stds$cr_std1[ni.id]
abs_co_2 = co_stds$co_std1[ni.id]

# find absorbance for each analyte at chromium's maximum wavelength
abs_cu_3 = cu_stds2$cu_std1[cr.id]
abs_ni_3 = ni_stds2$ni_std1[cr.id]
abs_cr_3 = cr_stds$cr_std1[cr.id]
abs_co_3 = co_stds$co_std1[cr.id]

# find absorbance for each analyte at cobalt's maximum wavelength
abs_cu_4 = cu_stds2$cu_std1[co.id]
abs_ni_4 = ni_stds2$ni_std1[co.id]
abs_cr_4 = cr_stds$cr_std1[co.id]
abs_co_4 = co_stds$co_std1[co.id]

# prepare one, two, and three dimensional plots
old.par = par(mfrow = c(2,2))

# plot in one dimension
x = c(abs_cu_1, abs_ni_1, abs_cr_1, abs_co_1)
y = c(0,0,0,0)
plot(
  x = x, y = y, type = "p", 
  pch = c(21,22,23,24), col = c(3,4,2,8), 
  bg = c(3,4,2,8), cex = 1.75,
  xlab = "absorbance at 809.1 nm",
  ylab = "", ylim = c(0,-0.1),
  yaxt = "n"
)
grid()
abline(h = 0)

# plot in two dimensions
x = c(abs_cu_1, abs_ni_1, abs_cr_1, abs_co_1)
y = c(abs_cu_2, abs_ni_2, abs_cr_2, abs_co_2)
plot(
  x = x, y = y, type = "p", 
  pch = c(21,22,23,24), col = c(3,4,2,8), 
  bg = c(3,4,2,8), cex = 1.75,
  xlab = "absorbance at 809.1 nm",
  ylab = "absorbance at 394.2 nm"
)
grid()

# plot in three dimensions
x = c(abs_cu_1, abs_ni_1, abs_cr_1, abs_co_1)
y = c(abs_cu_2, abs_ni_2, abs_cr_2, abs_co_2)
z = c(abs_cu_3, abs_ni_3, abs_cr_3, abs_co_3)
scatterplot3d(
  x = x, y = y, z = z, type = "h", 
  pch = c(21,22,23,24), color = c(3,4,2,8), 
  bg = c(3,4,2,8), cex.symbols = 1.75,
  xlab = "absorbance at 809.1 nm",
  ylab = "absorbance at 394.2 nm",
  zlab = "absorbance at 407.2 nm",
)

par(old.par)

