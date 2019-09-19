require(kohonen)
require(RColorBrewer)
require(spatstat)
require(mvtnorm)
require(graphics)
require(rgl)
source('coolBlueHotRed.R')

data <- read.csv(file = "datalimpia.csv", head = TRUE, sep = ",")

# Atributos jugadores
features <- c( 'Crossing', 'Finishing', 'ShortPassing',
                   'Dribbling', 'LongPassing', 'BallControl',
                   'Acceleration', 'SprintSpeed', 'Agility', 'Reactions', 'Balance',
                   'Jumping', 'Stamina', 'Strength', 'LongShots', 'Aggression', 'Interceptions',
                   'Positioning', 'Vision', 'Composure', 'Marking', 'StandingTackle',
                   'SlidingTackle')



position <- c('Position_Cat')
features_short <- c('Crossing', 'Finishing', 'ShortPassing',
              'Dribbling', 'LongPassing', 'BallControl',
              'Acceleration', 'SprintSpeed', 'Agility', 'Reactions',
              'Jumping', 'Stamina', 'Strength',  'Interceptions',
              'Positioning', 'Vision', 'Composure', 'StandingTackle',
              'SlidingTackle')

data$Position_Cat <- as.character(data$Position_Cat)
data$Position_Cat[data$Position_Cat == "Delantero"] <- "Forward"
data$Position_Cat[data$Position_Cat == "Centrocampista"] <- "Midfielder"
data$Position_Cat[data$Position_Cat == "Defensa"] <- "Defender"
data$Position_Cat[data$Position_Cat == "Portero"] <- "Goalkeeper"
data$Position_Cat <- factor(data$Position_Cat,  c("Forward", "Midfielder", "Defender", "Goalkeeper"))

lv <- levels(data$Position_Cat)

# Division de dataset original: solo jugadores de campo y con puntuacion global mayor de 75
data1 <- subset(data, subset = Position_Cat != 'Goalkeeper' & Overall > 75)
# Datos con atributos seleccionados
X <- data1[features_short]

# Crear los conjuntos de train y set

n <- nrow(X)
index_train <- sample(x = n, size = 1000)

X_train <- X[index_train,]

X_test <- X[-index_train,]

# Etiquetas de train y test por separado (por conflitos de tipos de datos en la función superosm)
labels_train <- as.factor(data1[index_train,position])
labels_test <- as.factor(data1[-index_train,position])

Xtraindata <- scale(X_train)
trainingdata <- list(feat = Xtraindata, labels = labels_train)

Xtestdata <- scale(X_test, center = attr(Xtraindata,  "scaled:center"), scale = attr(Xtraindata, "scaled:scale"))
testdata <- list(features = Xtestdata, labels = labels_test)


# Modelo
par(mfrow = c(1,1) ,cex.main= 0.7)

mygrid<- somgrid(20,20,"rectangular")

data.som1 <- xyf(trainingdata[[1]],trainingdata[[2]], grid = mygrid, rlen= 250)

# Mapping Plot
png(paste0("fig_", "mapping", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data.som1, type = "mapping", col = rainbow(3)[labels_train], keepMargins = TRUE )
legend("topleft", legend = lv[c(TRUE, TRUE, TRUE, FALSE)], text.col = rainbow(3), 
       inset = c(-0.02, 0))
dev.off()
# U-Matrix
png(paste0("fig_", "umatrix", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data.som1, type = "dist.neighbours", palette.name=terrain.colors, main = "U-Matrix", keepMargins = TRUE, shape = "straight")
dev.off()
# Code plot
png(paste0("fig_", "codes", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data.som1, type = "codes", palette.name=terrain.colors, main = "Codes Map", keepMargins = TRUE, shape = "straight")
dev.off()
# Counts plot
png(paste0("fig_", "counts", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data.som1, type = "counts", palette.name=terrain.colors, main = "Count map (A)", keepMargins = TRUE, shape = "straight")
dev.off()
# Changes plot
png(paste0("fig_", "changes", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data.som1, type = "changes",palette.name=terrain.colors, main = "Changes plot (B)", keepMargins = TRUE, shape = "straight")
dev.off()
# Quality plot
png(paste0("fig_", "quality", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data.som1, type = "quality", palette.name=terrain.colors, main = "Quality plot (C)", keepMargins = TRUE, shape = "straight")
dev.off()

# Predicción de datos de test y matriz de confusión
som.prediction <- predict(data.som1, newdata = testdata,  whatmap = 1)
table(testdata[[2]], som.prediction$predictions[[2]], useNA = 'always')

#Mapeo de datos de train y mapping plot con predicciones
bgcols<- c("darkred", "darkgreen", "darkblue")

mapping <- map(data.som1, newdata= testdata, whatmap = 1)
mapping_lab <- map(data.som1, newdata= testdata, whatmap = 2)

plot(data.som1, type = "mapping",main = "Mapping plot for test data ",classif = mapping_lab$unit.classif, col = rainbow(3)[as.integer(testdata[[2]])], bgcol = bgcols[classmat2classvec(som.prediction$unit.predictions[[2]])])


par(mfrow = c(1,1) ,cex.main= 0.7)
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,1], main = colnames(X[1]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,2], main = colnames(X[2]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,3], main = colnames(X[3]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,4], main = colnames(X[4]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,5], main = colnames(X[5]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,6], main = colnames(X[6]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,7], main = colnames(X[7]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,8], main = colnames(X[8]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,9], main = colnames(X[9]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,10], main = colnames(X[10]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,11], main = colnames(X[11]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,12], main = colnames(X[12]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,13], main = colnames(X[13]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,14], main = colnames(X[14]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,15], main = colnames(X[15]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,16], main = colnames(X[16]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,17], main = colnames(X[17]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,18], main = colnames(X[18]), palette.name = coolBlueHotRed )
plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,19], main = colnames(X[19]), palette.name = coolBlueHotRed )

for (i in 1:19) {
  
  png(paste0("fig_", i, ".png"), width = 7, height = 7, units = "in", res = 200)
  plot(data.som1, type = "property", property = getCodes(data.som1,1 )[,i],
       main = colnames(X[i]), palette.name = coolBlueHotRed )
  dev.off()
  
}

## CLASSIFICATION
data1 <- subset(data, subset = Position_Cat != 'Goalkeeper' & Overall > 75)
data1$Position_Cat <- factor(data1$Position_Cat,  c("Forward", "Midfielder", "Defender", "Goalkeeper"))

# Datos con atributos seleccionados
X1 <- data1[features]

# Crear los conjuntos de train y set

n1 <- nrow(X1)
index_train1 <- sample(x = n1, size = 1000 )

X1_train <- X[index_train1,]

X1_test <- X[-index_train1,]

# Etiquetas de train y test por separado (por conflitos de tipos de datos en la función superosm)
labels_train1 <- as.factor(data1[index_train1,position])
labels_test1 <- as.factor(data1[-index_train1,position])

X1traindata <- scale(X1_train)
trainingdata1 <- list(feat = X1traindata, labels = labels_train1)

X1testdata <- scale(X1_test, center = attr(X1traindata,  "scaled:center"), scale = attr(X1traindata, "scaled:scale"))
testdata1 <- list(features = X1testdata, labels = labels_test1)


# Modelo
par(mfrow = c(1,1) ,cex.main= 0.7)

mygrid<- somgrid(20,20,"rectangular")

data.som2 <- xyf(trainingdata1[[1]],trainingdata1[[2]], grid = mygrid)

# Mapping Plot

plot(data.som2, type = "mapping", col = rainbow(3)[labels_train1], keepMargins = TRUE )
legend("topleft", legend = lv[c(TRUE, TRUE, TRUE, FALSE)], text.col = rainbow(3), 
       inset = c(-0.02, 0))
# U-Matrix
plot(data.som2, type = "dist.neighbours", palette.name=terrain.colors, main = "U-Matrix", keepMargins = TRUE, shape = "straight")
# Code plot

plot(data.som2, type = "codes", palette.name=terrain.colors, main = "Codes Map", keepMargins = TRUE, shape = "straight")
# Counts plot

plot(data.som2, type = "counts", palette.name=terrain.colors, main = "Count map", keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data.som2, type = "changes",palette.name=terrain.colors, main = "Changes plot (D)", keepMargins = TRUE, shape = "straight")
# Quality plot
plot(data.som2, type = "quality", palette.name=terrain.colors, main = "Quality plot (G)", keepMargins = TRUE, shape = "straight")


# Predicción de datos de test y matriz de confusión
som.prediction <- predict(data.som2, newdata = testdata1,  whatmap = 1)
table(testdata1[[2]], som.prediction$predictions[[2]], useNA = 'always')

#Mapeo de datos de train y mapping plot con predicciones
bgcols<- c("darkred", "darkgreen", "darkblue")

mapping <- map(data.som2, newdata= testdata1, whatmap = 1)
mapping_lab <- map(data.som2, newdata= testdata1, whatmap = 2)

plot(data.som2, type = "mapping",main = "Mapping plot for test data ",classif = mapping$unit.classif, col = rainbow(3)[as.integer(testdata1[[2]])], bgcol = bgcols[classmat2classvec(som.prediction$unit.predictions[[2]])])



