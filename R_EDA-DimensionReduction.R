# R_EDA-DimensionReduction.R
# Purpose:
#
# Version: 1.0
#
# Date:    2016  06  01
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#          Some prior contributions by:
#            Raphael Gottardo, FHCRC
#            Sohrab Shah, UBC
# V 1.0    First code
#
# TODO:
#
#
# == HOW TO WORK WITH THIS FILE ======================================
#
#  Go through this script line by line to read and understand the
#  code. Execute code by typing <cmd><enter>. When nothing is
#  selected, that will execute the current line and move the cursor to
#  the next line. You can also select more than one line, e.g. to
#  execute a block of code, or less than one line, e.g. to execute
#  only the core of a nested expression.
#
#  Edit code, as required, experiment with options, or just play.
#  Especially play.
#
#  DO NOT simply source() this whole file!
#
#  If there are portions you don't understand, use R's help system,
#  Google for an answer, or ask me. Don't continue if you don't
#  understand what's going on. That's not how it works ...
#
#  This is YOUR file. Write your notes in here and add as many
#  comments as you need to understand what's going on here when you
#  come back to it in a year. That's the right way: keep code, data
#  and notes all in one place.
#
# ====================================================================
#
# Module 3: Dimension Reduction
#
# ====================================================================



# =============================================
# PCA introduction
# =============================================

# Synthetic data example
# 500 normally distributed samples each: uncorrelated
set.seed(2707)
x1 <- rnorm(500,0,1)
y1 <- rnorm(500,0,1)

# generate y2 corrleated with (dependent on) x1
y2 <- 2*x1 + y1
mean(y2)
y2 <- y2-mean(y2)
mean(y2)
sd(y2)
y2 <- y2 / sd(y2)
sd(y2)
print(sd(y2), digits=22)


# Create a lattice plot with two rows and two columns
oPar <- par(mfrow = c(2,2)) # set new and save old graphics state
par(mfrow = c(2,2))
# four plots ...
hist(x1)
hist(y2)
plot(x1, y1)
plot(x1, y2)
par(oPar) # restore graphics state parameters


# calculate a PCA of x1 and y2
pcaSample <- prcomp(cbind(x1,y2))

# here are the information items from the returned list of results
pcaSample
pcaSample$sdev
pcaSample$rotation
summary(pcaSample)
head(pcaSample$x)
plot(pcaSample$x, xlim=c(-5,5), ylim=c(-5,5))

# Compare the histograms before and after the rotation:
oPar <- par(mfrow = c(2,2))
hist(x1, xlim=c(-4,4), ylim=c(0,150), main="")
hist(y2, xlim=c(-4,4), ylim=c(0,150), main="")
hist(pcaSample$x[,1], xlim=c(-4,4), ylim=c(0,150),
     main="", col=rgb(0.86,0,0,0.5))
hist(pcaSample$x[,2], xlim=c(-4,4), ylim=c(0,150),
     main="", col=rgb(0.31, 0.5, 0.74, 0.5))
par(oPar) # restore graphics state parameters

# Plot the sample along the Principal Components as axes
plot(pcaSample$x[,1],pcaSample$x[,2], xlim=c(-4,4), ylim=c(-4,4))

typeInfo(pcaSample)

?prcomp

# ==================================================
# EDA with PCA
# The relative importance of PCs
# ==================================================

# load one of the sample data sets in the R distribution

library(MASS)
data(crabs)

head(crabs)
# Two types: blue and orange
# Two genders: female and male
# FL frontal lobe size (mm)
# RW rear width (mm)
# CL carapace length (mm)
# CW carapace width (mm)
# BD body depth (mm)

# annotate...
fac <- as.factor(paste(crabs[, 1], crabs[, 2],sep="."))
head(fac)
c(fac[1], fac[51], fac[101], fac[151])
as.numeric(c(fac[1], fac[51], fac[101], fac[151]))

plot(crabs[, 4:8], pch=as.numeric(fac))
plot(crabs[, 4:5], pch=as.numeric(fac))
plot(crabs[, 5:6], pch=as.numeric(fac))

# Apply principal components analysis to the five measured dimensions
head(crabs)
pcaCrabs <- prcomp(crabs[, 4:8])

plot(pcaCrabs)
summary(pcaCrabs)
str(pcaCrabs)

# Plot projections along the components into a scatterplot.
# Axes for points are scaled as values, for vectors as variance
# Default for biplot() is the first and second component.

biplot(pcaCrabs, xlabs=as.numeric(fac))
legend(81, -63,c("1: B.F", "2: B.M", "3: O.F", "4: O.M"), box.col=1, bg="lightgrey")

# Plot the first against the third principal component
biplot(pcaCrabs, xlabs=as.numeric(fac), choices = c(1, 3))
legend(84, -63,
       c("1: B.F", "2: B.M", "3: O.F", "4: O.M"),
       box.col=1,
       bg="lightgrey")

# Plot the second against the third principal component
biplot(pcaCrabs, xlabs=as.numeric(fac), choices = c(2, 3))
legend(-14.8, 16.2,
       c("1: B.F", "2: B.M", "3: O.F", "4: O.M"),
       box.col=1,
       bg="lightgrey")

# ===================================================
# Task: plot the last plot (without vectors) with plotting
# symbols that correspond to the gender and type of crab:
# orange and blue circles for females and triangles for males.

# Advanced: also make symbol-size depend on carapace length or
# the mean of all five measurements.
# ===================================================

plot(pcaCrabs$x[,2], pcaCrabs$x[,2], type ="n")
points(pcaCrabs$x[  1: 50,2], pcaCrabs$x[  1: 50,3], pch=17, col="blue")
points(pcaCrabs$x[ 51:100,2], pcaCrabs$x[ 51:100,3], pch=19, col="blue")
points(pcaCrabs$x[101:150,2], pcaCrabs$x[101:150,3], pch=17, col="orange")
points(pcaCrabs$x[151:200,2], pcaCrabs$x[151:200,3], pch=19, col="orange")


# My solution.
# 1. Create only the plotting frame

crabPoints <- function(n) {
    if (crabs[n,"sp"] == "B") {
        sp = "#0066FF"
    } else {
        sp = "#FF9900"
    }
    if (crabs[n,"sex"] == "M") {
        sex = 24  # triangle
    } else {
        sex = 21  # circle
    }
    points(pcaCrabs$x[n,2],
           pcaCrabs$x[n,3],
           col = sp, bg = sp,
           pch = sex,
           cex = scalePC(pcaCrabs$x[n,1]))
}

# scale by first PC
scalePC <- function(x) {
    x <- x +30
    x <- (x / 55) * 2.5
    x <- x + 0.5
    return(x)
}

plot(pcaCrabs$x[,2], pcaCrabs$x[,3], type = "n")


for (i in 1:nrow(crabs)) {
    crabPoints(i)
}
summary(pcaCrabs$x[,1])


plot(pcaCrabs$x[,1:5])


# ==================================================
# EDA with PCA
# Exploring the structure of datasets
# ==================================================

# Load the Raymond Cho dataset of expression profiles
# containing 237 genes known or suspected to be involved
# in cell-cycle regulation.

cho.data <- read.table("Logcho_237_4class.txt", skip=1)[, 3:19]
nrow(cho.data)
head(cho.data)

# compare the general trends of the measurements
boxplot(cho.data)

# Since we are looking for expression changes
# in a cycle, we are less interested in the
# absolute values of expression, and more in
# whether values change in a cyclical fashion.
# As a first step to normalize the data, we can
# therefore subtract the row-mean from each row.

for (i in 1:nrow(cho.data)) {
    cho.data[i,] <- cho.data[i,] - mean(as.numeric(cho.data[i,]))
}

boxplot(cho.data)

# This has caused the averages and ranges to become quite
# divergent between experiments. We should normalize them
# so as to make sure every experiment (dimension)
# contributes equally to our principal component analysis.

# Let's create a function to normalize the data.

normDat <- function(x) {
    # input: a column from a dataframe
    x <- as.numeric(x)
    norm <- x - mean(x)       # subtract means
    norm <- norm / sd(norm)   # scale SD to 1
    return(norm)
}

for (i in 1:ncol(cho.data)) {
    cho.data[ ,i] <- normDat(cho.data[ ,i])
}

boxplot(cho.data)
# Change the column names
names(cho.data) <- paste("t", 1:17, sep="")
head(cho.data)

pcaCho <- prcomp(cho.data)
plot(pcaCho, n=17)

# Compare the PCs to PCs from a matrix of random numbers
# ... eg as follows:
rU = matrix(runif(17 * 237,0,3), nrow = 237, ncol=17)
plot(prcomp(rU), n=17, ylim = c(0, 3))

# Exercise: compare the PCs to a matrix of permuted
# observations.

# Explore the correlations along the first few principal components

oPar <- par(mfrow = c(2,2))
biplot(pcaCho)
biplot(pcaCho, choices = c(1, 3))
biplot(pcaCho, choices = c(2, 3))
biplot(pcaCho, choices = c(3, 4))
par(oPar)


# Examine the actual principal components in a paralell-coordinates
# plot for the seventeen dimensions. In microarray analysis we call
# these the "eigengenes", with reference to the "eigenvalues" of
# linear algebra.
matplot(pcaCho$rotation[, 1:3],
        type="b", lwd=3,
        xlab = "timepoint", ylab="PCs")

legend(x = "topright", inset = 0.02,
       legend = c("PC1", "PC2", "PC3"), cex = 0.8,
       text.width = 1,
       bg = "#EEEEEE",
       seg.len = 5, col = 1:3, lty = 1:3, lwd = 2,
       pch = as.character(1:3))


# ====================
# ... select some genes to see how similar they are
#
#
# C O N T I N U E here

# [END]
