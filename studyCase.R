require(kohonen)
require(RColorBrewer)
require(spatstat)
require(mvtnorm)
require(graphics)
require(rgl)
source('coolBlueHotRed.R') # Retrieved from https://github.com/shanealynn/Kohonen-Self-organising-maps-in-R/blob/master/coolBlueHotRed.R

# Read dats
data <- read.csv(file = "datalimpia.csv", head = TRUE, sep = ",")

# Players skills
features <- c( 'Crossing', 'Finishing', 'ShortPassing',
                   'Dribbling', 'LongPassing', 'BallControl',
                   'Acceleration', 'SprintSpeed', 'Agility', 'Reactions', 'Balance',
                   'Jumping', 'Stamina', 'Strength', 'LongShots', 'Aggression', 'Interceptions',
                   'Positioning', 'Vision', 'Composure', 'Marking', 'StandingTackle',
                   'SlidingTackle')


# Subset of player skills
features_short <- c('Crossing', 'Finishing', 'ShortPassing',
              'Dribbling', 'LongPassing', 'BallControl',
              'Acceleration', 'SprintSpeed', 'Agility', 'Reactions',
              'Jumping', 'Stamina', 'Strength',  'Interceptions',
              'Positioning', 'Vision', 'Composure', 'StandingTackle',
              'SlidingTackle')

position <- c('Position_Cat') # name of target variable (simplified field position)

# Spanish to english translation
data$Position_Cat <- as.character(data$Position_Cat)
data$Position_Cat[data$Position_Cat == "Delantero"] <- "Forward"
data$Position_Cat[data$Position_Cat == "Centrocampista"] <- "Midfielder"
data$Position_Cat[data$Position_Cat == "Defensa"] <- "Defender"
data$Position_Cat[data$Position_Cat == "Portero"] <- "Goalkeeper"
data$Position_Cat <- factor(data$Position_Cat,  c("Forward", "Midfielder", "Defender", "Goalkeeper"))

lv <- levels(data$Position_Cat)


# Dataset division: only field players (no goalkeepers) with overall rating > 75
data1 <- subset(data, subset = Position_Cat != 'Goalkeeper' & Overall > 75)
# Data division with selected short features
X <- data1[features_short]

# Train and test sets creation
n <- nrow(X)
index_train <- sample(x = n, size = 1000)

X_train <- X[index_train,]

X_test <- X[-index_train,]

# Separation of train and test class labels (data types conflicts when using supersom class)
labels_train <- as.factor(data1[index_train,position])
labels_test <- as.factor(data1[-index_train,position])

# Data normalization
Xtraindata <- scale(X_train)
trainingdata <- list(feat = Xtraindata, labels = labels_train)

Xtestdata <- scale(X_test, center = attr(Xtraindata,  "scaled:center"), scale = attr(Xtraindata, "scaled:scale"))
testdata <- list(features = Xtestdata, labels = labels_test)


# CLASSIFICATION PART 1

mygrid<- somgrid(20,20,"rectangular")

# Supervised SOM object
data.som1 <- xyf(trainingdata[[1]],trainingdata[[2]], grid = mygrid, rlen= 250)

# Mapping Plot of train data
png(paste0("fig_", "mapping", ".png"), width = 7, height = 7, units = "in", res = 200) #save image as png
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
# Class predictions for test and confusion matrix
som.prediction <- predict(data.som1, newdata = testdata,  whatmap = 1)
table(testdata[[2]], som.prediction$predictions[[2]], useNA = 'always')

# Mapping plot for test data with predictions
bgcols<- c("darkred", "darkgreen", "darkblue") # color list for predicted classes

mapping <- map(data.som1, newdata= testdata, whatmap = 1)
mapping_lab <- map(data.som1, newdata= testdata, whatmap = 2)

plot(data.som1, type = "mapping",main = "Mapping plot for test data ",classif = mapping_lab$unit.classif, col = rainbow(3)[as.integer(testdata[[2]])], bgcol = bgcols[classmat2classvec(som.prediction$unit.predictions[[2]])])

# UNDERSTANDING DATA (property maps)
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

## CLASSIFICATION PART 2


data2<- read.csv(file = "datalimpia.csv", head = TRUE, sep = ",")

# players skills 
features <- c( 'Finishing', 
               'Dribbling',  'BallControl',
               'Interceptions',
               'Positioning', 'StandingTackle')



position <- c('Position_Cat')
features_short <- c('Crossing', 'Finishing', 'ShortPassing',
                    'Dribbling', 'LongPassing', 'BallControl',
                    'Acceleration', 'SprintSpeed', 'Agility', 'Reactions',
                    'Jumping', 'Stamina', 'Strength',  'Interceptions',
                    'Positioning', 'Vision', 'Composure', 'StandingTackle',
                    'SlidingTackle')


# Create new classes depending of soccer position
data2$Position_Cat <- as.character(data2$Position_Cat)
data2$Position_Cat[data2$Position_Cat == "Delantero"] <- "Forward"
data2$Position_Cat[data2$Position == "RCM"] <- "Neutral Midfielder"
data2$Position_Cat[data2$Position == "LCM"] <- "Neutral Midfielder"
data2$Position_Cat[data2$Position == "LDM"] <- "Defensive Midfielder"
data2$Position_Cat[data2$Position == "CAM"] <- "Attacking Midfielder"
data2$Position_Cat[data2$Position == "CDM"] <- "Defensive Midfielder"
data2$Position_Cat[data2$Position == "RM"] <- "Neutral Midfielder"
data2$Position_Cat[data2$Position == "LM"] <- "Neutral Midfielder"
data2$Position_Cat[data2$Position == "LAM"] <- "Attacking Midfielder"
data2$Position_Cat[data2$Position == "RAM"] <- "Attacking Midfielder"
data2$Position_Cat[data2$Position == "RDM"] <- "Neutral Midfielder"
data2$Position_Cat[data2$Position == "CM"] <- "Neutral Midfielder"
data2$Position_Cat[data2$Position_Cat == "Defensa"] <- "Defender"
data2$Position_Cat[data2$Position_Cat == "Portero"] <- "Goalkeeper"
data2$Position_Cat <- factor(data2$Position_Cat,  c("Forward", "Attacking Midfielder", "Neutral Midfielder", "Defensive Midfielder","Defender", "Goalkeeper"))

lv <- levels(data2$Position_Cat)

# Dataset division: only field players (no goalkeepers) with overall rating > 75
data12 <- subset(data2, subset = Position_Cat != 'Goalkeeper' & Overall > 70)
# Data division with selected short features
X2 <- data12[features_short]

# Train and test sets creation
n2 <- nrow(X2)
index_train2 <- sample(x = n2, size = 3000 )

X2_train <- X2[index_train2,]

X2_test <- X2[-index_train2,]

# Separation of train and test class labels (data types conflicts when using supersom class)
labels_train2 <- as.factor(data12[index_train2,position])
labels_test2 <- as.factor(data12[-index_train2,position])

# Data normalization
X2traindata <- scale(X2_train)
trainingdata2 <- list(feat = X2traindata, labels = labels_train2)

X2testdata <- scale(X2_test, center = attr(X2traindata,  "scaled:center"), scale = attr(X2traindata, "scaled:scale"))
testdata2 <- list(features = X2testdata, labels = labels_test2)


# MODEL

mygrid<- somgrid(25,25,"rectangular")

# Supervised SOM object
data2.som2 <- xyf(trainingdata2[[1]],trainingdata2[[2]], grid = mygrid)

# Mapping Plot for train data
png(paste0("fig_", "map2", ".png"), width = 7, height = 7, units = "in", res = 200) #save image as png
plot(data2.som2, type = "mapping", col = rainbow(5)[labels_train2], keepMargins = TRUE )
legend("topright", legend = lv[c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)], text.col = rainbow(5), 
       inset = c(-0.02, 0))
dev.off()
# U-Matrix
plot(data2.som2, type = "dist.neighbours", palette.name=terrain.colors, main = "U-Matrix", keepMargins = TRUE, shape = "straight")
# Code plot
plot(data2.som2, type = "codes", palette.name=terrain.colors, main = "Codes Map", keepMargins = TRUE, shape = "straight")
# Counts plot
plot(data2.som2, type = "counts", palette.name=terrain.colors, main = "Count map", keepMargins = TRUE, shape = "straight")
# Changes plot
plot(data2.som2, type = "changes",palette.name=terrain.colors, main = "Changes plot (D)", keepMargins = TRUE, shape = "straight")
# Quality plot
plot(data2.som2, type = "quality", palette.name=terrain.colors, main = "Quality plot (G)", keepMargins = TRUE, shape = "straight")


# Class prediction for test data and confusion matrix
som.prediction <- predict(data2.som2, newdata = testdata2[[1]],  whatmap = 1)
table(testdata2[[2]], som.prediction$predictions[[2]], useNA = 'always')

# Mapping plot for test data with predictions
bgcols<- c("darkred", "darkgreen", "darkolivegreen","darkblue", "darkorchid3") # color list for predicted classes

mapping <- map(data2.som2, newdata= testdata2, whatmap = 1) # features layer
mapping_lab <- map(data2.som2, newdata= testdata2, whatmap = 2) # target variable layer


png(paste0("fig_", "maptest2", ".png"), width = 7, height = 7, units = "in", res = 200)
plot(data2.som2, type = "mapping",main = "Mapping plot for test data ",classif = mapping_lab$unit.classif, col = rainbow(5)[as.integer(testdata2[[2]])], bgcol = bgcols[classmat2classvec(som.prediction$unit.predictions[[2]])])
legend("bottomleft", title = "True labels", title.col = "black",legend = lv[c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)], text.col = rainbow(5), bty = "o")
legend("topleft", title = "Predictions",title.col = "black",legend = lv[c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)], text.col = bgcols, )
dev.off()
