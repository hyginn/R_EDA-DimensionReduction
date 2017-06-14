# taskSolutions.R
#
# Example code for (some of) the workshop tasks
# ==========================================================================

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




# ==== systematic to standard gene name conversion ======
#
# found the information at SGD, using
# http://yeastmine.yeastgenome.org/
# made a tab-separated set of:
# sytematc name / standard name / name
# downloaded and edited to remove quotation marks for
# missing fields
geneInfo <- read.csv("yeastStandardGeneNames.tsv",
                     sep = "\t",
                     header = FALSE,
                     stringsAsFactors = FALSE)

str(geneInfo)
str(choGenes)

# PROBLEMS:
# gene names are mixed-case in choGenes, all upper-case in geneInfo
choGenes[1]
geneInfo[1,1]

# There might be names that are duplicated ... (why ?)
which(duplicated(choGenes))
which(duplicated(geneInfo[ ,1]))

# There might be names in choGenes that are not in geneInfo
which(!(toupper(choGenes) %in% geneInfo[,1]))

# ... R has a way to merge tables - see ?merge -
# but(!) all of these issues might prevent automated procedures
# to give false results (or not).

# Doing this very safely ...
# The output table we want:
stdGenes <- data.frame(sys = toupper(choGenes),
                       std = character(length(choGenes)),
                       name = character(length(choGenes)),
                       stringsAsFactors = FALSE)
head(stdGenes)

# Iterate over the gene names (in column 1)
# and for each gene name fetch the additional information
# from geneInfo
for (i in 1:nrow(stdGenes)) {
    sysID <- stdGenes$sys[i]

    index <- grep(sysID, geneInfo[ , 1])
    stdID <- geneInfo[index, 2]
    if (length(stdID) == 0) {   # handle no-match
        stdID <- sysID
    }
    myName <- geneInfo[index, 3]
    if (length(myName) == 0) {   # handle no-match
        myName <- "unknown"
    }

    # update fields in the dataframe
    stdGenes$std[i] <- stdID
    stdGenes$name[i] <- myName
}

# update listGenes()
listGenes <- function(x) {
    for (idx in x) {
        cat(sprintf("%d:\t%s\t%s\t%s\n",
                    idx,
                    stdGenes$sys[idx],
                    stdGenes$std[idx],
                    stdGenes$name[idx] ))
    }
}

listGenes(c(1,2,3,5,8))



# [END]
