
# Demonstration of Concept

source("Affine_Transformations.R")
source("generate_2D_ellipse.R")
source("define_new_axes.R")
source("project_data_onto_axes.R")
source("Show_PCA_Process.R")

seed <- 51

angs <- seq(0, 180, by = 15)
angs <- angs[-c(1, 7, 13)] # remove 0, 90, 180

angs <- seq(0, 85, by = 5) # finer angle resolution
angs <- angs[-c(1, 10)] # remove 0, 90

# pdf("Demo.pdf")
for (i in 1:length(angs)) Show_PCA_Process(rotEllipse = 15, rotAxes = angs[i], seed = seed)
# dev.off()
