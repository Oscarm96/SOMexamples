require(kohonen)
require(RColorBrewer)
require(spatstat)
require(mvtnorm)
require(graphics)
require(rgl)


# número de muestras test y train
n_train <- 400
n_test <- 100
#Genero datos
th_train <- runif(n = n_train, min = 0, max = 2 * pi)
th_test <- runif(n = n_test, min = 0, max = 2 * pi)

set.seed(232133)

normal1_train <- rnorm(n = n_train, mean = 2, sd = 0.1) * cbind(cos(th_train), sin(th_train))
normal1_test <- rnorm(n = n_test, mean = 2, sd = 0.1) * cbind(cos(th_test), sin(th_test))

# Attach class labels
R1_train <- cbind(normal1_train, rep(1, n_train))
R1_test <- cbind(normal1_test, rep(1, n_test))

normal2_train <- rnorm(n = n_train, mean = 0, sd = 0.2) * cbind(cos(th_train), sin(th_train))
normal2_test <- rnorm(n = n_test, mean = 0, sd = 0.2) * cbind(cos(th_test), sin(th_test))

# Attach labels
R2_train<- cbind(normal2_train,  rep(2, n_train))
R2_test<- cbind(normal2_test,  rep(2, n_test))

rings_train <- rbind(R1_train,R2_train)
rings_test <- rbind(R1_test,R2_test)

#Ploteo los datos
par(mfrow = c(1, 2))
plot(rings_train, main = "Scatter plot for train samples", col = rainbow(2)[rings_train[,3]] )
plot(rings_test , main = "Scatter plot for test samples" , col = rainbow(2)[rings_test[,3]] )

# Gradient function taken from http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Kohonen_SOM_R.pdf
degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))
}


# Creo dataframes
df_train <- data.frame(x1 = rings_train[,1], x2 = rings_train[,2], class = rings_train[,3])
df_test <- data.frame(x1 = rings_test[,1], x2 = rings_test[,2], class = rings_test[,3])

#etiquetas de train y test por separado (por conflitos de tipos de datos en la función superosm)
labels_train <- as.factor(df_train$class)
labels_test <- as.factor(df_test$class)
# Componenetes
measures <- c("x1", "x2")

Xtraindata <- scale(df_train[measures])
trainingdata <- list(measurements = Xtraindata, labels = labels_train)

Xtestdata <- scale(df_test[measures], center = attr(Xtraindata,  "scaled:center"), scale = attr(Xtraindata, "scaled:scale"))
testdata <- list(measurements = Xtestdata, labels = labels_test)

#SOM supervisado
data.supersom <- xyf(trainingdata[[1]],trainingdata[[2]], somgrid(10, 10, "rectangular"), rlen = 100, mode = "online")


# Mapping plot de datos de train
par(mfrow = c(1,1))
plot(data.supersom, type = "mapping", pch = 1, col = rainbow(2)[df_train[,3]],main = "Mapping plot for trainning data ", keepMargins = TRUE)

# Predicción de datos de test y matriz de confusión
som.prediction <- predict(data.supersom, newdata = testdata,  whatmap = 1)
table(testdata[[2]], som.prediction$predictions[[2]], useNA = 'always')

#Mapeo de datos de train y mapping plot con predicciones
bgcols<- c("darkred", "darkblue")

mapping <- map(data.supersom, newdata= testdata, whatmap = 1)
mapping_lab <- map(data.supersom, newdata= testdata, whatmap = 2)

plot(data.supersom, type = "mapping",main = "Mapping plot for test data ",classif = mapping$unit.classif, col = rainbow(2)[as.integer(as.factor(df_test$class))], bgcol = bgcols[classmat2classvec(som.prediction$unit.predictions[[2]])])
par(mfrow = c(1,1))

# U-Matrix
plot(data.supersom, type = "dist.neighbours", palette.name=degrade.bleu, main = "U-Matrix for unsupervised SOM", keepMargins = TRUE, shape = "straight")
# Code plot
plot(data.supersom, type = "codes", palette.name=degrade.bleu, main = "Codes Map", keepMargins = TRUE, shape = "straight")
# Counts plot
plot(data.supersom, type = "counts", palette.name=degrade.bleu, main = "Count map", keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data.supersom, type = "changes",palette.name=degrade.bleu, main = "Changes plot (D)", keepMargins = TRUE, shape = "straight")
# Quality plot
plot(data.supersom, type = "quality", palette.name=degrade.bleu, main = "Quality plot (G)", keepMargins = TRUE, shape = "straight")
















