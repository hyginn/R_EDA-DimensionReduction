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


# ==== systematic to standard gene name conversion ======
#
head(choGenes)
# Goal: vector of 237 standard names
#    use http://www.uniprot.org/docs/yeast.txt
#    edit the file into csv format
csvRaw <- read.csv("yeast.csv", header = FALSE, stringsAsFactors = FALSE)

"YAL040c"
std <- csvRaw[csvRaw[ , "V2"] == toupper("YAL040c"), "V1"]
strsplit(std, ";")[[1]][1]

test <- choGenes[sample(1:237, 5)]

choStandard <- character(length(choGenes))
for (i in 1:length(choGenes)) {
    syst <- choGenes[i]
    std <- csvRaw[csvRaw[ , "V2"] == toupper(syst), "V1"]
    if (length(std) == 1) {
        choStandard[i] <- strsplit(std, ";")[[1]][1]
    } else {
        choStandard[i] <- toupper(syst)
    }
}


# shuffling data

a <- letters[1:6]
a
sample(a)




