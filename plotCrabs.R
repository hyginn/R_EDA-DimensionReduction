# plotCrabs.R
#
# Purpose: Plot crabs data with symbols that reflect gender, species and age
#
# ToDo:
# Notes:
#
# ==============================================================================

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



# [END]
