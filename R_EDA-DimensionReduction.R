# R_EDA-DimensionReduction.R
# Purpose:
#
# Version: 1.1
#
# Date:    2017  06  01
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
#          Some prior contributions by:
#          Raphael Gottardo, FHCRC
#          Sohrab Shah, UBC
#
# V 1.1    2017 updates
# V 1.0    First code 2016
#
# TODO:
#
#
# == HOW TO WORK WITH THIS FILE ================================================
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
#  Once you have typed and executed the function init(), you will find a file
#  called myScript.R in the project directory.
#
#  Open it, you can place all of your code-experiments and notes into that
#  file. This will be your "Lab Journal" for this session.
#
# ==============================================================================
#
# Module 3: Dimension Reduction
#
# ==============================================================================

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                              Line
#TOC> ------------------------------------------------------------------
#TOC>   1        PCA introduction                                     70
#TOC>   2        EDA with PCA                                        131
#TOC>   2.1      The relative importance of PCs                      134
#TOC>   2.1.1    Task: identify categories of elements in a plot     192
#TOC>   3        EDA with PCA                                        229
#TOC>   3.1      Load cell-cycle expression profiles                 235
#TOC>   3.2      Explore the principal components                    277
#TOC>   3.3      Explore some similar genes                          329
#TOC>   4        Exploring data relative to models                   386
#TOC>   5        t-SNE                                               478
#TOC>
#TOC> ==========================================================================




# ==============================================================================
# =    1  PCA introduction  ====================================================
# ==============================================================================

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

# ==============================================================================
# =    2  EDA with PCA  ========================================================
# ==============================================================================

# ==   2.1  The relative importance of PCs  ====================================

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
legend(81, -63,
       c("1: B.F", "2: B.M", "3: O.F", "4: O.M"),
       box.col=1, bg="lightgrey")

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


# ===  2.1.1  Task: identify categories of elements in a plot

# Task: plot the last plot (without vectors) with plotting
# symbols that correspond to the gender and type of crab:
# orange and blue circles for females and triangles for males.

# Advanced: also make symbol-size depend on carapace length or
# the mean of all five measurements.

# A pedestrian (but simple) solution
plot(pcaCrabs$x[,2], pcaCrabs$x[,2], type ="n")
points(pcaCrabs$x[  1: 50, 2], pcaCrabs$x[  1: 50, 3], pch=17, col="blue")
points(pcaCrabs$x[ 51:100, 2], pcaCrabs$x[ 51:100, 3], pch=19, col="blue")
points(pcaCrabs$x[101:150, 2], pcaCrabs$x[101:150, 3], pch=17, col="orange")
points(pcaCrabs$x[151:200, 2], pcaCrabs$x[151:200, 3], pch=19, col="orange")


# Dimension Reduction - Part 2 (second day)
#

# ======== Recreate data =======================================================

library(MASS)
data(crabs)
pcaCrabs <- prcomp(crabs[, 4:8])

# Study the code in crabsPlot.R for a more advanced solution

source("crabsPlot.R")
crabsPlot(pcaCrabs$x[,2], pcaCrabs$x[,3],
          crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
          main = "Principal components 2 and 3 distinguish crabs",
          xlab = "PC2",
          ylab = "PC3")


# ==============================================================================
# =    3  EDA with PCA  ========================================================
# ==============================================================================

# Exploring the structure of datasets
# ==================================================

# ==   3.1  Load cell-cycle expression profiles  ===============================

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
# Therefore we scale all rows to mean 0 and
# sd 1. Scale works on columns - to scale
# rows we use t() to transpose ...
x <- t(cho.data)
x <- scale(x)
cho.data <- as.data.frame(t(x))
boxplot(cho.data)

# Change the column names
colnames(cho.data) <- paste("t", 1:17, sep="")
head(cho.data)


# Plot some expression profiles
set.seed(112358)
Sel <- sample(1:nrow(cho.data), 10)

matplot(t(cho.data[Sel, ]),
        type="b", lwd=2, col=cm.colors(10),
        main = "Random selection",
        ylab = "Scaled expression levels",
        xlab = "Timepoints")



# ==   3.2  Explore the principal components  ==================================

# calculate the PCS
pcaCho <- prcomp(cho.data)
plot(pcaCho, n=17)

# Compare the PCs to PCs from a matrix of random numbers
# ... eg as follows:
rU = matrix(runif(17 * 237, 0, 3), nrow = 237, ncol=17)
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
N <- 4
matplot(pcaCho$rotation[, 1:N],
        type="b", lwd=3,
        xlab = "timepoint", ylab="PCs")


# Define a way to fetch the actual gene names:
# A: read the gene names for the original data
choGenes <- as.character(read.table("logcho_237_4class.txt", skip=1)[, 1])
head(choGenes)

# B: define a function to list gene names for row numbers
listGenes <- function(x) {
    for (i in 1:length(x)) {
        cat(sprintf("%d:\t%s\n", x[i], choGenes[x[i]] ))
    }
}

# C: try it
listGenes(c(1,2,3,5,8))


# ==   3.3  Explore some similar genes  ========================================

plot(pcaCho$x[ , 2], pcaCho$x[ , 3], type = "n")
text(pcaCho$x[ , 2], pcaCho$x[ , 3], cex = 0.7)



Sel1 <- c(104, 72, 86, 148, 186, 71)
text(pcaCho$x[Sel1, 2], pcaCho$x[Sel1, 3], labels = Sel1, col="red", cex = 0.7)

matplot(t(cho.data[Sel1, ]),
        type = "b", lwd = 3, col = "seagreen",
        main = "Clustered Genes - Set 1",
        ylab = "Scaled expression levels",
        xlab = "Timepoints")

listGenes(Sel1)

# TASK: select and plot 10 random genes for comparison
?sample
SelRand <- sample(1:237, 10)

matplot(t(cho.data[SelRand, ]),
        type = "b", lwd = 3, col = "maroon",
        main = "Random genes",
        ylab = "Scaled expression levels",
        xlab = "Timepoints")



# ====================

# Choose a few gene indices from a different biplot and
# compare...

plot(pcaCho$x[ , 1], pcaCho$x[ , 3], type = "n")
text(pcaCho$x[ , 1], pcaCho$x[ , 3], cex = 0.7)
text(pcaCho$x[Sel1, 1], pcaCho$x[Sel1, 3], labels = Sel1, col="seagreen", cex = 0.7)


Sel2 <- c(98, 205, 126, 127, 102, 236)
text(pcaCho$x[Sel2, 1], pcaCho$x[Sel2, 3], labels = Sel2, col="firebrick", cex = 0.7)

matplot(t(cho.data[Sel2, ]),
        type = "b", lwd = 3, col = "firebrick",
        main = "Clustered Genes - Set 2",
        ylab = "Scaled expression levels",
        xlab = "Timepoints")

listGenes(Sel2)

# TASK: (collaborative)
#    A list of systematic genes is not that informative.
#    In EDA, you aim to get a "feeling" for your data.
#    Improve the gene listing to get the common names.

# ==============================================================================
# =    4  Exploring data relative to models  ===================================
# ==============================================================================


# PCA will show us what is actually found in our data. However we might
# already have some a priori ideas of what might be interesting. If we
# can cast these ideas in a model, we can simply calculate correlation
# coefficients between observation and model and project the data along
# these dimensions.

# PCA is a projection of the data under a high-dimensional
# rotation. This model-based analysis can be seen as a
# projection along a particular model.

# Here we define three simple models:
# a half-sine, one sine wave and two sine waves over the course of
# the experiment.


Nval <- 17

ProfHalf <- 1:Nval
ProfFull <- 1:Nval
ProfTwo  <- 1:Nval
ProfHalf <- cos((ProfHalf-1) * (pi / (Nval-1)))
ProfFull <- sin((ProfFull-1) * ((2 * pi) / (Nval-1)))
ProfTwo  <- sin((ProfTwo-1) * ((4 * pi) / (Nval-1)))

plot(ProfHalf, type="l",
     xlim = c(1, Nval),
     ylim = c(-2, 2), col="2")
lines(ProfFull, type="l",
      xlim = c(1, Nval),
      ylim = c(-2, 2))
lines(ProfTwo, type="l",
      xlim = c(1, Nval),
      ylim = c(-2, 2), col="3")

# plotting the correlation of each datapoint against our two models
n <- dim(cho.data)[1]
CorHalf <- rep(0, n)
CorFull <- rep(0, n)
CorTwo <- rep(0, n)
for (i in 1:n) {
    x <- c(t(cho.data[i, ]))
    CorHalf[i] <- cor(x, ProfHalf)
    CorFull[i]  <- cor(x, ProfFull)
    CorTwo[i]  <- cor(x, ProfTwo)
}

plot(CorTwo, CorFull, type="n")
text(CorTwo, CorFull, cex = 0.7)

plot(CorTwo, CorHalf, type="n")
text(CorTwo, CorHalf, cex = 0.7)

# Select indices from the plot and plot paralell coordinates
# of the actual values.

Sel1 <- c(237, 233, 170, 143, 229,   7,  87, 200, 128, 192)
Sel2 <- c(171, 205, 236, 187, 181, 102, 126,  84, 127,  97)
Sel3 <- c(   2, 101,  3, 175, 142,  94,  23,  19,  78, 116)

text(CorTwo[Sel1], CorHalf[Sel1], labels = Sel1, col="skyblue",   cex = 0.7)
text(CorTwo[Sel2], CorHalf[Sel2], labels = Sel2, col="firebrick", cex = 0.7)
text(CorTwo[Sel3], CorHalf[Sel3], labels = Sel3, col="seagreen",  cex = 0.7)

listGenes(Sel1)
listGenes(Sel2)
listGenes(Sel3)


oPar <- par(mfrow = c(3,1), mar=c(1,1,1,1)) # reduce margin sizes
matplot(t(cho.data[Sel1, ]), type="b", lwd=3,
        ylab="Similar genes",
        xlab = "Timepoints",
        ylim = c(-4, 4),
        col="skyblue")
matplot(t(cho.data[Sel2, ]), type="b", lwd=3,
        ylab="",
        xlab="",
        ylim = c(-4, 4),
        col="firebrick")
matplot(t(cho.data[Sel3, ]), type="b", lwd=3,
        ylab="",
        xlab="",
        ylim = c(-4, 4),
        col="seagreen")
par(oPar)




# ============================================================
# =    5  t-SNE  ===============================================================
# ============================================================
# t-Stochastic Neighbour Embedding is a powerful dimension re-
# duction algorithm developed in the lab of Geoff Hinton at UofT.
#
# see: https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding
#
# Its implementation for flow cytometry - viSNE -is available
# from the Dana Pe'er lab (as Matlab code).
# http://www.c2b2.columbia.edu/danapeerlab/html/cyt.html
# It is the basis for a very powerful flow-cytometry exploration tool:
# Amir et al. (2013) Nature Biotechnology, doi:10.1038/nbt.2594
#
# Below we will try the t-SNE algorithm for exploration of
# some of the data we have looked at before.

if (!require(tsne, quietly=TRUE)) {
    install.packages("tsne")
    library(tsne)
}
?tsne


# Apply tsne to the crabs data
# First: define a plotting function
tsnePlot <- function(x) {
    crabsPlot(x[,1], x[,2],
              crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
              main = "Crabs TSNE")
}

# make the run reproducible
set.seed(208)

# run tsne
tsneCrabs <- tsne(crabs[,4:8],
                  epoch_callback = tsnePlot,
                  perplexity=70,
                  max_iter = 500)




# Apply tsne to the cell-cycle data

sel1 <- which(CorTwo > 0.77) # Pick a subset, as above. We will track whether
sel1                         # the subset is cohesive during the iterations.

sel2 <- which(CorTwo < -0.6) # Negative correlations
sel2


matplot(t(cho.data[sel1, ]), type="b", lwd=3,
        ylab="Normalized expression (AU)",
        xlab="timepoint",
        ylim = c(-4, 4),
        col="firebrick")
for (i in 1:length(sel2)) {
    lines(t(cho.data[sel2[i], ]), type="l", lwd=2, lty=2, col="turquoise")
}


ecb <- function(x){
    plot(x,t='n');
    points(x[sel1,], bg="firebrick", pch=21, cex=2)
    points(x[sel2,], bg="turquoise", pch=21, cex=2)
    text(x, labels=paste(1:dim(cho.data)[1]), cex=0.7)
}

set.seed(12345678)
tsne_cho <- tsne(cho.data[,1:17],
                 epoch_callback = ecb,
                 perplexity = 50,
                 max_iter = 1000)

# Pick a cluster of genes in the vicinity of the originally
# selected values

sel3 <- c(4, 192, 200, 20, 143, 7, 107, 128, 75, 124)
matplot(t(cho.data[sel1, ]), type="b", lwd=3,
        ylab="Normalized expression (AU)",
        xlab="timepoint",
        ylim = c(-4, 4),
        col="firebrick")
for (i in 1:length(sel3)) {
    lines(t(cho.data[sel3[i], ]), type="l", lwd=2, col="salmon")
}

# TASK: where are these genes on the model-correlation plot?

plot(CorTwo, CorHalf, type="n")
text(CorTwo, CorHalf)
points(CorTwo[sel1], CorHalf[sel1], pch=19, col="firebrick", cex=2)
points(CorTwo[sel3], CorHalf[sel3], pch=19, col="salmon", cex=2)




# Exercise for self study: plot the position of a two-cycle sine function, its
# inverse, a two-cycle cosine function and its inverse on the SNE map itself.
# Are there genes in the vicinity of these reference points? Are these related
# to the ones you found earlier when projecting along models? Hint: you'll have
# to re-run the embedding after adding the profiles to the data, and highlight
# the points that correspond to the models themselves in the plots.

# [END]
