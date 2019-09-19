require(kohonen)
require(RColorBrewer)
require(spatstat)
require(mvtnorm)
require(graphics)
require(scatterplot3d)

n<- 200 #number of samples per class

# Generate samples from Normal 1
sigma1 <- diag(3)*0.5 
mu1 <- c(-3,-3,-3)
normal1 <- rmvnorm(n, mean = mu1, sigma = sigma1)
class1 <- rep(1, n)
# Attach class labels
G1 <- cbind(normal1, class1)

# Generate samples from Normal 2
sigma2 <- diag(3)*0.5
mu2 <-  c(3,3,3)
normal2 <- rmvnorm(n, mean = mu2, sigma = sigma2)
class2 <- rep(2, n)
# Attach labels
G2 <- cbind(normal2, class2)

# Complete data set
X <- rbind(G1,G2)

# Gradient function taken from http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Kohonen_SOM_R.pdf
degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))
}

# Data frame 
df <- data.frame(x1 = X[,1], x2 = X[,2], x3= X[,3], class = X[,4])
# Components
df.measures <- c("x1", "x2", "x3")


scatterplot3d::scatterplot3d(X[,1:3], color = rainbow(2)[X[,4]], main = "Original data (A)" )

## UNSUPERVISED SOM

set.seed(232133)
data2.supersom <- supersom(scale(df[df.measures]),somgrid(13, 13, "hexagonal"))
# Compute clusters
som.hc <- cutree(hclust(object.distances(data2.supersom, "codes")), 2)
# Mapping plot
plot(data2.supersom, type = "mapping", col = rainbow(2)[X[,4]], pch = 1, main = "Mapping plot for unsupervised SOM", keepMargins = TRUE )
# Add boundary to mapping plot
add.cluster.boundaries(data2.supersom, som.hc)
# U-Matrix
plot(data2.supersom, type = "dist.neighbours", palette.name=degrade.bleu, main = "U-Matrix for unsupervised SOM", keepMargins = TRUE, shape = "straight")
# Code plot
plot(data2.supersom, type = "codes", palette.name=degrade.bleu, main = "Codes Map", keepMargins = TRUE, shape = "straight")
# Counts plot
plot(data2.supersom, type = "counts", palette.name=degrade.bleu, main = "Count map", keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data2.supersom, type = "changes",palette.name=degrade.bleu, main = "Changes plot (D)", keepMargins = TRUE, shape = "straight")
# Quality plot
plot(data2.supersom, type = "quality", palette.name=degrade.bleu, main = "Quality plot (G)", keepMargins = TRUE, shape = "straight")
# SUPERVISED SOM

data.supersom <- xyf(scale(df[df.measures]), as.factor(df$class), somgrid(13, 13, "hexagonal"))

bgcols<- c("darkred", "darkblue")
xyfpred<- classmat2classvec(getCodes(data.supersom, 2))
#Mapping plot
plot(data.supersom, type = "mapping", col = rainbow(2)[as.integer(as.factor(df$class))], pch = 1, main = "Mapping Plot for supervised SOM", keepMargins = TRUE, bgcol = bgcols[as.integer(xyfpred)])
# U-Matrix
plot(data.supersom, type = "dist.neighbours", palette.name=terrain.colors, main = "U-Matrix for unsupervised SOM", keepMargins = TRUE, shape = "straight")
# Code plot
plot(data.supersom, type = "codes", palette.name=terrain.colors, main = "Codes Map", keepMargins = TRUE, shape = "straight")
# Counts plot
plot(data.supersom, type = "counts", palette.name=terrain.colors, main = "Count map", keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data.supersom, type = "changes",palette.name=terrain.colors, main = "Changes plot (D)", keepMargins = TRUE, shape = "straight")
# Quality plot
plot(data.supersom, type = "quality", palette.name=terrain.colors, main = "Quality plot (G)", keepMargins = TRUE, shape = "straight")
