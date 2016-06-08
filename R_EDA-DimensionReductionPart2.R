# R_EDA-DimensionReductionPart2.R
#
# Dimension Reduction - Part 2 (continued from yesterday)
#

# ======== Recreate data =======================================================

library(MASS)
data(crabs)

cho.data <- read.table("Logcho_237_4class.txt", skip=1)[, 3:19]
for (i in 1:nrow(cho.data)) {
    cho.data[i,] <- cho.data[i,] - mean(as.numeric(cho.data[i,]))
}
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
names(cho.data) <- paste("t", 1:17, sep="")
pcaCho <- prcomp(cho.data)
# ==============================================================================

# define a way to identify gene names
choGenes <- as.character(read.table("logcho_237_4class.txt", skip=1)[, 1])
head(choGenes)

listGenes <- function(x) {
    for (i in 1:length(x)) {
        cat(sprintf("%d: %s\n", i, choGenes[i]))
    }
}


plot(pcaCho$x[ , 1], pcaCho$x[ , 2], type = "n")
text(pcaCho$x[ , 1], pcaCho$x[ , 2], cex = 0.7)

Sel1 <- c(193, 142, 235, 98, 73, 54, 114)
text(pcaCho$x[Sel1, 1], pcaCho$x[Sel1, 2], labels = Sel1, col="red", cex = 0.7)

matplot(t(cho.data[Sel1, ]),
        type="b", lwd=3, col="3",
        ylab="Clustered genes",
        xlab = "Timepoints")

listGenes(Sel1)

# TASK: select and plot 10 random genes for comparison
?sample
SelRand <- sample(1:237, 10)

matplot(t(cho.data[SelRand, ]),
        type="b", lwd=3, col="maroon",
        ylab="Random genes",
        xlab = "Timepoints")



# ====================

# Choose a few gene indices from a different biplot and
# compare...

plot(pcaCho$x[ , 1], pcaCho$x[ , 3], type = "n")
text(pcaCho$x[ , 1], pcaCho$x[ , 3], cex = 0.7)
text(pcaCho$x[Sel1, 1], pcaCho$x[Sel1, 3], labels = Sel1, col="skyblue", cex = 0.7)


Sel2 <- c(20, 227, 143, 128, 192, 75)
text(pcaCho$x[Sel2, 1], pcaCho$x[Sel2, 3], labels = Sel2, col="firebrick", cex = 0.7)

matplot(t(cho.data[Sel1, ]),
        type="b", lwd=3, col="skyblue",
        ylab="normalized expression",
        xlab = "Timepoint",
        ylim = c(-4, 4))

listGenes(Sel1)

# TASK: (collaborative)
#    A list of systematic genes is not that informative.
#    In EDA, you aim to get a "feeling" for your data.
#    Improve the gene listing to get the common names.

oPar <- par(new=TRUE)
matplot(t(cho.data[Sel2, ]),
        type="b", lwd=3, col="firebrick",
        ylab="",
        xlab="",
        ylim = c(-4, 4))
par(oPar)

listGenes(Sel2)


# ==================================================
# Exploring the structure of datasets
# relative to preconceived models
# ==================================================

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
# t-SNE
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
    plotCrabs(x[,1], x[,2],
              crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
              main = "Crabs TSNE")
}

# make the run reproducible
set.seed(20)

# run tsne
tsneCrabs <- tsne(crabs[,4:8],
                  epoch_callback = tsnePlot,
                  perplexity=70,
                  max_iter = 5000)




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
                 max_iter = 2000)

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
points(CorTwo[sel3], CorHalf[sel3], pch=19, col="firebrick", cex=2)
points(CorTwo[sel1], CorHalf[sel1], pch=19, col="salmon", cex=2)




# Exercise for self study: plot the position of a two-cycle sine function, its
# inverse, a two-cycle cosine function and its inverse on the SNE map itself.
# Are there genes in the vicinity of these reference points? Are these related
# to the ones you found earlier when projecting along models? Hint: you'll have
# to re-run the embedding after adding the profiles to the data, and highlight
# the points that correspond to the models themselves in the plots.

# [END]
