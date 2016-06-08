# taskSolutions.R
#
# Code for (some of) the workshop tasks


# Task: plot the last plot (without vectors) with plotting
# symbols that correspond to the gender and type of crab:
# orange and blue circles for females and triangles for males.

# 1. run a PCA on the data and assign the result
pc <- prcomp(crabs[ ,4:8])

# 2. confirm that $x[ ,2] $x[ ,3] contain the values we are looking for
plot(pc$x[ ,2], pc$x[ ,3])

# 3. figure out how to define an orange circle, blue triangle etc. as a plotting character.
plot(pc$x[ ,2], pc$x[ ,3], pch=21, bg="orange", col="red")
plot(pc$x[ ,2], pc$x[ ,3], pch=24, bg="skyblue", col="blue")

# 4. plot an empty frame of the right size
plot(pc$x[ ,2], pc$x[ ,3], type="n")

 # 5. use points()
points(pc$x[1:50,2], pc$x[1:50,3], pch=21, bg="skyblue", col="blue")
points(pc$x[51:100,2], pc$x[51:100,3], pch=24, bg="skyblue", col="blue")
points(pc$x[101:150,2], pc$x[101:150,3], pch=21, bg="orange", col="red")
points(pc$x[151:200,2], pc$x[151:200,3], pch=24, bg="orange", col="red")

# ==== a more principled solution: =====================
# define a function:
#
plotCrabs <- function(x, y, species, sex, age,
                      main = "",
                      xlab = "",
                      ylab = "") {
    # Purpose:
    #     Create a plot of crabs data in which individual points
    #     are colored by species, shaped by gender and scaled by
    #     age.
    # Version:  1.0
    # Date:     2016-06-07
    # Author:   Boris Steipe
    #
    # Parameters:
    #     x: numeric   x-coordinate of point
    #     y: numeric   y-coordinate of point
    #     species: factor with level B and O
    #     gender: factor with levels M and F
    #     age: numeric
    # Value:
    #     None. Creates plot as side-effect

    N <- length(x)  #  number of points

    # 1. Create a color vector from species and sex factors. Taken as
    #    integers, both factors can be either 1 or 2. We use this to
    #    pick a color value from a vector. The first factor
    #    is transformed to (0, 2), the second is (1,2), the index
    #    in the colSet vector is obtained by adding the two.

    colSet <- c("#00baff", "#0066ff", "#ff9900","#ff5500")
    crabCols <- colSet[((as.integer(species) - 1) * 2) + as.integer(sex)]

    # 2. create a vector of plotting characters. M: 21; F: 24
    pchSet <- c(24, 21)
    crabPch <- pchSet[as.integer(sex)]

    # 3. create a scale vector from sMin to sMax
    sMin <- 0.5
    sMax <- 4
    crabCex <- (((age - min(age)) / (max(age) - min(age))) * (sMax - sMin)) + sMin

    plot(x, y,
         main = main, xlab = xlab, ylab = ylab,
         pch = crabPch,
         col = "black",
         bg  = crabCols,
         cex = crabCex)
}

# Try it out ...
plotCrabs(pcaCrabs$x[,2], pcaCrabs$x[,3],
          crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
          main = "Principal components 2 and 3 distinguish crabs",
          xlab = "PC2",
          ylab = "PC3")


