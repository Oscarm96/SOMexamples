require(kohonen)
require(RColorBrewer)
require(spatstat)
require(mvtnorm)
require(graphics)
require(rgl)


## Generate data

# Train and test sizes
n_train <- 400
n_test <- 100

set.seed(232133) #set seed

th_train <- runif(n = n_train, min = 0, max = 2 * pi)
th_test <- runif(n = n_test, min = 0, max = 2 * pi)

normal1_train <- rnorm(n = n_train, mean = 2, sd = 0.1) * cbind(cos(th_train), sin(th_train))
normal1_test <- rnorm(n = n_test, mean = 2, sd = 0.1) * cbind(cos(th_test), sin(th_test))

# Attach class labels (1)
R1_train <- cbind(normal1_train, rep(1, n_train))
R1_test <- cbind(normal1_test, rep(1, n_test))

normal2_train <- rnorm(n = n_train, mean = 0, sd = 0.2) * cbind(cos(th_train), sin(th_train))
normal2_test <- rnorm(n = n_test, mean = 0, sd = 0.2) * cbind(cos(th_test), sin(th_test))

# Attach class labels (2)
R2_train<- cbind(normal2_train,  rep(2, n_train))
R2_test<- cbind(normal2_test,  rep(2, n_test))

rings_train <- rbind(R1_train,R2_train)
rings_test <- rbind(R1_test,R2_test)

# Scatter plots of train and test sets
par(mfrow = c(1, 2))
plot(rings_train, main = "Scatter plot for train samples", col = rainbow(2)[rings_train[,3]] )
plot(rings_test , main = "Scatter plot for test samples" , col = rainbow(2)[rings_test[,3]] )

# Color gradient function retrieved from http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Kohonen_SOM_R.pdf
degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))
}


# Create dataframes
df_train <- data.frame(x1 = rings_train[,1], x2 = rings_train[,2], class = rings_train[,3])
df_test <- data.frame(x1 = rings_test[,1], x2 = rings_test[,2], class = rings_test[,3])

# Separate train and test class labels (data type conflicts when using supersom class)
labels_train <- as.factor(df_train$class)
labels_test <- as.factor(df_test$class)

# Components
measures <- c("x1", "x2")

# Normalize data
Xtraindata <- scale(df_train[measures])
trainingdata <- list(measurements = Xtraindata, labels = labels_train)

Xtestdata <- scale(df_test[measures], center = attr(Xtraindata,  "scaled:center"), scale = attr(Xtraindata, "scaled:scale"))
testdata <- list(measurements = Xtestdata, labels = labels_test)

# Supervised SOM
data.supersom <- xyf(trainingdata[[1]],trainingdata[[2]], somgrid(10, 10, "rectangular"), rlen = 100, mode = "online")

# Train data mapping plot 
par(mfrow = c(1,1))
plot(data.supersom, type = "mapping", pch = 1, col = rainbow(2)[df_train[,3]],main = "Mapping plot for trainning data ", keepMargins = TRUE)

# Class prediction for test data and confussion matrix
som.prediction <- predict(data.supersom, newdata = testdata,  whatmap = 1)
table(testdata[[2]], som.prediction$predictions[[2]], useNA = 'always')


# Mapping of test data with predictions
bgcols<- c("darkred", "darkblue") # color list for predicted class

mapping <- map(data.supersom, newdata= testdata, whatmap = 1) # features layer
mapping_lab <- map(data.supersom, newdata= testdata, whatmap = 2) # target variables layer

plot(data.supersom, type = "mapping",main = "Mapping plot for test data ",classif = mapping$unit.classif, col = rainbow(2)[as.integer(as.factor(df_test$class))], bgcol = bgcols[classmat2classvec(som.prediction$unit.predictions[[2]])])


# U-Matrix
plot(data.supersom, type = "dist.neighbours", palette.name=degrade.bleu, main = "U-Matrix for supervised SOM", keepMargins = TRUE, shape = "straight")
# Code plot
plot(data.supersom, type = "codes", palette.name=degrade.bleu, main = "Codes Map for supervised SOM", keepMargins = TRUE, shape = "straight")
# Counts plot
plot(data.supersom, type = "counts", palette.name=degrade.bleu, main = "Count map for supervised SOM", keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data.supersom, type = "changes",palette.name=degrade.bleu, main = "Changes plot for supervised SOM", keepMargins = TRUE, shape = "straight")
# Quality plot
plot(data.supersom, type = "quality", palette.name=degrade.bleu, main = "Quality plot for supervised SOM", keepMargins = TRUE, shape = "straight")
















