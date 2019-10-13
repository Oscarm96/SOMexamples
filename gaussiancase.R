require(kohonen)
require(RColorBrewer)
require(spatstat)
require(mvtnorm)
require(graphics)
require(utils)
require(MASS)
require(scatterplot3d)

## Generate data from a 3-D Gaussian distribution
n <- 500
mu <- c(0,0,0)
sigma <- diag(3)
normal <- mvrnorm(n = n, mu, sigma)


# Color gradient function retrieved from http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Kohonen_SOM_R.pdf
degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))
}

# Convert to data frame
df <- data.frame(x1 = normal[,1], x2 = normal[,2], x3 = normal[,3])
# Set seed
set.seed(232133)
# SOM object
data.supersom <- supersom(scale(df), somgrid(5, 5, "rectangular"), rlen = 100, mode = "online")

# Scatter plot of data
scatterplot3d::scatterplot3d(normal, color = "blue", main = "Original data (A)" )
# Counts plot
plot(data.supersom, type = "counts", palette.name=degrade.bleu, main = "Count map (B)", keepMargins = FALSE, shape = "straight")
# Code plot
plot(data.supersom, type = "codes", palette.name=degrade.bleu, main = "Codes plot (C)", keepMargins = TRUE, shape = "straight")
# Property plot
plot(data.supersom, type = "property", main = colnames(getCodes(data.supersom, 1))[1], property = getCodes(data.supersom, 1)[,1], keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data.supersom, type = "changes",palette.name=degrade.bleu, main = "Changes plot (D)", keepMargins = TRUE, shape = "straight")
# U-Matrix
plot(data.supersom, type = "dist.neighbours", palette.name=degrade.bleu, main = "U-Matrix", keepMargins = TRUE, shape = "straight")
# Mapping plot
plot(data.supersom, type = "mapping", pch = 1, main = "Mapping Plot ", keepMargins = TRUE)
# Quality plot
plot(data.supersom, type = "quality", palette.name=degrade.bleu, main = "Quality plot (G)", keepMargins = TRUE, shape = "straight")

# Print number of samples of each unit
counts <- table(data.supersom$unit.classif)
print(counts)

# Get codebook cordinates and create matrix with extra row containing the distribution mean
codebooks <- getCodes(data.supersom)
codebooks <- rbind(codebooks, mu)

# Print distances between codebooks and Gaussian mean
aux <- as.matrix(dist(codebooks))
print(aux[26,])





