# Course: Stat 730 Multivariate Statistical Methods 
# 
# Mid-Term Data Project 
#
# Bhaskar Aryal 

#setwd("C:/Users/bhaskar/OneDrive - Kansas State University/Documents/KSU 2022/MS BAE/Semester II - Aug 22 - Dec 22/STAT 730 Multivariate Statistical Method/Mid Term Data Project")
setwd("C:/Users/aryal/OneDrive - Kansas State University/Documents/KSU 2022/MS BAE/Semester II - Aug 22 - Dec 22/STAT 730 Multivariate Statistical Method/Mid Term Data Project")

wine <- read.csv('ALLwinequality.csv', header = TRUE)
head(wine)

#Splitting Datset into training and testing sets 

WID <- 06455
set.seed(WID)
samp <- sample(nrow(wine))
Training <- wine[samp[1:300], ]
Testing <- wine[samp[301:600], ]
Validation <- wine[samp[601:900], ]
Exploratory <- data.frame(rbind(Training, Testing))

#Writing reslults into a csv file 
write.csv(Training,"Training.csv",row.names = FALSE)
write.csv(Testing,"Testing.csv",row.names = FALSE)
write.csv(Validation,"Validation.csv",row.names = FALSE)

#Exploratory Analysis 

# Summary Statistics 
names(Exploratory) # Variables in the data set 
summary(Exploratory[, 1:12])

#Exploring patterns with histograms 

#Setting graphics parameter histograms 
par(mfrow=c(4,3), cex.lab =1, cex.axis=1, cex.main=1)

hist(Exploratory$fixed_acidity, xlab = 'Fixed Acidity', col = 'maroon', 
     main = 'Fixed Acidity')

hist(Exploratory$volatile_acidity, xlab = 'Volatile Acidity', col = 'aquamarine', 
     main = 'Volatile Acidity')

hist(Exploratory$citric_acid, xlab = 'Citric Acid', col = 'cadetblue', 
     main = 'Citric Acid')

hist(Exploratory$residual_sugar, xlab = 'residual sugar', col = 'turquoise', 
     main = 'residual sugar')

hist(Exploratory$chlorides, xlab = 'Chlorides', col = 'coral', 
     main = 'Chlorides')

hist(Exploratory$free_sulfur_dioxide, xlab = 'Free Sulphur Dioxide', col = 'brown', 
     main = 'Free Sulphur Dioxide')

hist(Exploratory$total_sulfur_dioxide, xlab = 'Total Sulphur Dioxide', col = 'bisque', 
     main = 'Total Sulphur Dioxide')

hist(Exploratory$density, xlab = 'Density', col = 'azure', 
     main = 'Density')

hist(Exploratory$pH, xlab = 'pH', col = 'chocolate4', 
     main = 'pH')

hist(Exploratory$sulphates, xlab = 'Sulphates', col = 'skyblue', 
     main = 'Sulphates')

hist(Exploratory$alcohol, xlab = 'Alcohol', col = 'coral2', 
     main = 'Alcohol')

hist(Exploratory$quality, xlab = 'Quality', col = 'purple', 
     main = 'Quality')

dev.off()


# (b) Scatter Plot and Box Plots 

#Using the library "Car" and scatterplot matrix for scatter plot 
library(car)
spm(Exploratory[,1:12], var.labels = colnames(Exploratory[, 1:12]),
    diagonal=list(method="boxplot"), smooth=F, regLine=F, cex.axis=1.5, 
    cex.labels=1.25, pch=20, id= TRUE, col="cadetblue")


# Divide Quality into different grades 

Exploratory$quality <- factor(Exploratory$quality, ordered = T)

Exploratory$rating <- ifelse(Exploratory$quality < 5, 'bad', ifelse(
  Exploratory$quality < 7, 'average', 'good'))

Exploratory$rating <- ordered(Exploratory$rating,
                       levels = c('bad', 'average', 'good'))



# Selecting six variables at once to create plot and study pattern 
library(car)
spm(Exploratory[, c(1:4,11:12)], var.labels = colnames(Exploratory[, c(1:4,11:12)]),
    diagonal=list(method="boxplot"), smooth=F, regLine=F, cex.axis=1.5, 
    cex.labels=1.25, pch=20, id= TRUE, col= 16)

spm(Exploratory[, c(5:8,11:12)], var.labels = colnames(Exploratory[, c(5:8,11:12)]),
    diagonal=list(method="boxplot"), smooth=F, regLine=F, cex.axis=1.5, 
    cex.labels=1.25, pch=20, id= TRUE, col= 20)

spm(Exploratory[, c(9:12)], var.labels = colnames(Exploratory[, c(9:12)]),
    diagonal=list(method="boxplot"), smooth=F, regLine=F, cex.axis=1.5, 
    cex.labels=1.25, pch=20, id= TRUE, col= 26)

# Quality Ratings and other variables distribution 


library(ggplot2)

# alcohol vs Quality rating 
al <- ggplot(data = Exploratory, aes(x = rating, y = alcohol, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter()

# fixed acidity vs quality rating 
fa <- ggplot(data = Exploratory, aes(x = rating, y = fixed_acidity, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# volatile acidity vs quality rating  
va <- ggplot(data = Exploratory, aes(x = rating, y = volatile_acidity, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# citric acid vs quality rating  
ca <- ggplot(data = Exploratory, aes(x = rating, y = citric_acid, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# residual sugar vs quality rating  
rs <- ggplot(data = Exploratory, aes(x = rating, y = residual_sugar, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# chlorides vs quality rating  
cl <- ggplot(data = Exploratory, aes(x = rating, y = chlorides, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# free_sulfur_dioxide vs quality rating 
fsd <- ggplot(data = Exploratory, aes(x = rating, y = free_sulfur_dioxide, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# total_sulfur_dioxide vs quality rating
tsd <- ggplot(data = Exploratory, aes(x = rating, y = total_sulfur_dioxide, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# density vs quality rating 
den <- ggplot(data = Exploratory, aes(x = rating, y = density, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# pH vs quality rating 
ph <- ggplot(data = Exploratory, aes(x = rating, y = pH, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# sulphates vs quality rating 
sul <- ggplot(data = Exploratory, aes(x = rating, y = sulphates, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

# type vs quality rating 
type <- category <- as.factor(Exploratory$type)
typ <- ggplot(data = Exploratory, aes(x = rating, y = category, color = quality))+
  geom_boxplot() + theme_classic() + geom_jitter() 

library(ggpubr)
Bxplts <- ggarrange(al, fa, va, ca, rs, cl, fsd, tsd, den, ph, sul, typ, common.legend = TRUE) 
Bxplts

# Variance covariance matrix and correlation 

library(ggplot2)
library(ggcorrplot)

#Computing variance covariance matrix 

# Changing quality score back to numeric 
Exploratory$quality <- as.numeric(Exploratory$quality)

var.covar <- var(Exploratory[, 1:12])
var.covar

# Computing correlation matrix
correlation_matrix <- round(cor(Exploratory[, 1:12]),1)

# Computing correlation matrix with p-values
corrp.mat <- cor_pmat(Exploratory[, 1:12])

# Adding the correlation coefficient
ggcorrplot(correlation_matrix, hc.order =TRUE,
           type ="lower", lab =TRUE, outline.color = 'white', 
          insig = 'blank') + theme_classic() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 


# Alcohol and Density at different quality 
Exploratory$quality <- as.factor(Exploratory$quality)
Exploratory.Red <- Exploratory[Exploratory$type == 1, ]
Exploratory.White <- Exploratory[Exploratory$type == 0, ]
  
ad.red <- ggplot(aes(x = alcohol, y = density, color = quality, size = quality), data = Exploratory.Red) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=1) +
  labs(x = 'Alcohol', y = 'Density', title = 'Density and Alcohol - Red') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black')

ad.white <- ggplot(aes(x = alcohol, y = density, color = quality, size = quality), data = Exploratory.White) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=2) +
  labs(x = 'Alcohol', y = 'Density', title = 'Density and Alcohol - White') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black')
  
alcohol.vs.density <- ggarrange(ad.white, ad.red, common.legend = TRUE)
alcohol.vs.density

# Free and Total Sulphur Dioxide and Quality 

sulp.red <- ggplot(aes(x = free_sulfur_dioxide, y = total_sulfur_dioxide, color = quality, size = quality), data = Exploratory.Red) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=3) +
  labs(x = 'Free Sulphur', y = 'Total Sulphur', title = 'Free Sulphur and Total Sulphur at Quality Levels - Red') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black')

sulp.white <- ggplot(aes(x = free_sulfur_dioxide, y = total_sulfur_dioxide, color = quality, size = quality), data = Exploratory.White) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=4) +
  labs(x = 'Free Sulphur', y = 'Total Sulphur', title = 'Free Sulphur and Total Sulphur at Quality Levels - White') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black')

Free.vs.Total.Sulphur <- ggarrange(sulp.red, sulp.white, common.legend = TRUE)
Free.vs.Total.Sulphur


# Alcohol and Chlorides at different Quality Levels 

ac.red <- ggplot(aes(x = chlorides, y = alcohol, color = quality, size = quality), data = Exploratory.Red) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=5) +
  labs(x = 'Chlorides', y = 'Alcohol', title = 'Alcohol vs Chlorides at Quality Levels - Red') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black') +
  coord_cartesian(xlim = c(0, 0.45))

ac.white <- ggplot(aes(x = chlorides, y = alcohol, color = quality, size = quality), data = Exploratory.White) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=6) +
  labs(x = 'Chlorides', y = 'Alcohol', title = 'Alcohol vs Chlorides at Quality Levels - White') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black') + 
  coord_cartesian(xlim = c(0, 0.30))

Alcohol.vs.Chlorides <- ggarrange(ac.red, ac.white, common.legend = TRUE)
Alcohol.vs.Chlorides

# Alcohol and Residual Sugar at different Quality Levels 

res.sugar.red <- ggplot(aes(x = residual_sugar, y = alcohol, color = quality, size = quality), data = Exploratory.Red) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=7) +
  labs(x = 'Residual Sugar', y = 'Alcohol', title = 'Alcohol vs Residual Sugar at Quality Levels - Red') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black') 

res.sugar.white <- ggplot(aes(x = residual_sugar, y = alcohol, color = quality, size = quality), data = Exploratory.White) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=8) +
  labs(x = 'Residual Sugar', y = 'Alcohol', title = 'Alcohol vs Residual Sugar at Quality Levels - White') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black')

Alcohol.vs.Sugar <- ggarrange(res.sugar.red, res.sugar.white, common.legend = TRUE)
Alcohol.vs.Sugar


# Density and Sugar Concentration 

Den.sug.red <- ggplot(aes(x = residual_sugar, y = density, color = quality, size = quality), data = Exploratory.Red) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=9) +
  labs(x = 'Residual Sugar', y = 'Density', title = 'Density vs Residual Sugar at Quality Levels - Red') +
  theme_classic2() + stat_regline_equation(label.x = 5, color = 'black') 

Den.sug.white <- ggplot(aes(x = residual_sugar, y = density, color = quality, size = quality), data = Exploratory.White) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE, size = 1) +
  scale_color_brewer(palette=10) +
  labs(x = 'Residual Sugar', y = 'Density', title = 'Density vs Residual Sugar at Quality Levels - White') +
  theme_classic2() + stat_regline_equation(label.x = 11, color = 'black')

Density.vs.sugar <- ggarrange(Den.sug.red, Den.sug.white, common.legend = TRUE)
Density.vs.sugar

Exploratory$type <- as.factor(Exploratory$type)
ggplot(Exploratory, aes(x = residual_sugar, y = density, color = type, size = quality)) +
  geom_point(alpha = 0.8) + 
  geom_smooth(method = 'lm', se = FALSE, size = 1, aes(color = type )) + 
  theme_classic2() + scale_color_brewer(palette='Set1') +
  labs(x = 'Residual Sugar', y = 'Density', title = 'Density vs Residual Sugar at Quality Levels - White and Red')



################ Question Number 2 - Cluster Analysis##########################################
##################                                   ##########################################


# Creating boxplot for early signs on cluster - Type 

#Setting graphics parameter
par(mfrow=c(4,3), cex.lab =1, cex.axis=1, cex.main=1)

boxplot(fixed_acidity ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(1))
boxplot(volatile_acidity ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(2))
boxplot(citric_acid ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(3))
boxplot(residual_sugar ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(4))
boxplot(chlorides ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(5))
boxplot(free_sulfur_dioxide ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(6))
boxplot(total_sulfur_dioxide ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(7))
boxplot(density ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(8))
boxplot(pH ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(9))
boxplot(sulphates ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(10))
boxplot(alcohol ~ type, data = Exploratory, horizontal = TRUE, col = terrain.colors(11))

dev.off()

# Creating boxplot for early signs on cluster - Quality

par(mfrow=c(4,3), cex.lab =1, cex.axis=1, cex.main=1)

boxplot(fixed_acidity ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(1))
boxplot(volatile_acidity ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(2))
boxplot(citric_acid ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(3))
boxplot(residual_sugar ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(4))
boxplot(chlorides ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(5))
boxplot(free_sulfur_dioxide ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(6))
boxplot(total_sulfur_dioxide ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(7))
boxplot(density ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(8))
boxplot(pH ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(9))
boxplot(sulphates ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(10))
boxplot(alcohol ~ quality, data = Exploratory, horizontal = TRUE, col = terrain.colors(11))

dev.off()

# Scatters for cluster to verify boxplot cluster assumptions 
# Total Sulfur Dioxide and Acidity 
plt1 <- ggplot(Exploratory, aes(y = total_sulfur_dioxide, x = volatile_acidity, color = type)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c("White", "Red"))

# Free Sulfur Dioxide and Acidity 
plt2 <- ggplot(Exploratory, aes(y = free_sulfur_dioxide, x = volatile_acidity, color = type)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c("White", "Red"))

#Chlorides and Acidity  
plt3 <- ggplot(Exploratory, aes(y = volatile_acidity, x = chlorides, color = type)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c("White", "Red")) 

#Sulphates and Chlorides 

plt4 <- ggplot(Exploratory, aes(y = sulphates, x = chlorides, color = type)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c("White", "Red")) 

# Total Sulfur Dioxide and Acidity with Quality  
plt5 <- ggplot(Exploratory, aes(y = total_sulfur_dioxide, x = volatile_acidity, color = quality)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)')

ggarrange(plt1, plt2, plt3, plt4, plt5)


#######

# Scatters for cluster to verify boxplot cluster assumptions 
# Total Sulfur Dioxide and Acidity 
ggplot(Exploratory, aes(y = total_sulfur_dioxide, x = volatile_acidity, color = quality)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c(3:8))

# Free Sulfur Dioxide and Acidity 
ggplot(Exploratory, aes(y = free_sulfur_dioxide, x = volatile_acidity, color = quality)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c(3:8))

#Chlorides and Acidity  
ggplot(Exploratory, aes(y = volatile_acidity, x = chlorides, color = quality)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c(3:8)) 

#Sulphates and Chlorides 

ggplot(Exploratory, aes(y = sulphates, x = chlorides, color = quality)) + 
  geom_point() + theme_classic() + labs(title = 'Wine Scatter - Red (1) vs White (0)') + 
  scale_colour_discrete(labels = c(3:8)) 

######

# Scaling the data for standardization for K- Means 

Exploratory.V2 <- Exploratory[, 1:11]
Exploratory.V2.Scaled <- scale(Exploratory.V2)


# Optimal K-means 
library(cluster)
library(factoextra)
library(NbClust)

# Silhouette method
fviz_nbclust(Exploratory.V2.Scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(Exploratory.V2.Scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


NbClust(data = Exploratory.V2.Scaled, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = c('ward.D', 'ward.D2', 'single', 'complete', 'average'))


clust_2 <- kmeans(Exploratory.V2.Scaled, 2)
clust_2

clust_3 <- kmeans(Exploratory.V2.Scaled, 3)
clust_3

clust_5 <- kmeans(Exploratory.V2.Scaled, 5)
clust_5

# Plotting the clusters 
par(mfrow=c(1,3), cex.lab =1, cex.axis=1, cex.main=1)

clusplot(Exploratory.V2.Scaled, clust_2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = '2 Cluster Plot')

clusplot(Exploratory.V2.Scaled, clust_5$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = '5 Cluster Plot')

clusplot(Exploratory.V2.Scaled, clust_3$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = '3 Cluster Plot')

dev.off()



########################## Discrimination and Classification####################
##########################                                  ####################

library(MASS)

# Constant variance assumption check 

Training$quality <- as.factor(Training$quality)
Training$type <- as.factor(Training$type)

# Unique values in quality after splitting it into training and testing 

unique(Training$quality)
unique(Testing$quality)

q3 <- var(Training[Training$quality == 3, c(1:11)])
q4 <- var(Training[Training$quality == 4, c(1:11)])
q5 <- var(Training[Training$quality == 5, c(1:11)])
q6 <- var(Training[Training$quality == 6, c(1:11)])
q7 <- var(Training[Training$quality == 7, c(1:11)])
q8 <- var(Training[Training$quality == 8, c(1:11)])

round(q3/q4, 2)
round(q4/q8, 2)
round(q5/q7, 2)


# Box Test to confirm Unequal variances 

library(biotools)

boxM(Training[, 1:11], Training$quality)


# QDA Approach as variance covariance among group is different 

# QDA with proportionl priors / Holdout procedure

Training$quality <- factor(Training$quality, ordered = T)

Training$rating <- ifelse(Training$quality < 5, 'bad', ifelse(
  Training$quality < 7, 'average', 'good'))

Training$rating <- ordered(Training$rating,
                              levels = c('bad', 'average', 'good'))



# First Model - All variables 

library(MASS)

# QDA with proportional priors

fit.qda.1 <- qda(rating ~., data = Training[, c(1:11, 14)])
fit.qda.1

#  QDA with equal priors and 

fit.qda.2 <- qda(rating ~. , data = Training[, c(1:11, 14)], prior = rep(1/3, 3))
fit.qda.2

# QDA with subset of variables, proportional priors 

fit.qda.3 <- qda(rating ~ volatile_acidity + total_sulfur_dioxide
                 + density + alcohol, data = Training[, c(1:11, 14)])
fit.qda.3 

# QDA with subset of variables, equal priors 


fit.qda.4 <- qda(rating ~ volatile_acidity + total_sulfur_dioxide
                 + density + alcohol, data = Training[, c(1:11, 14)],
                 prior = rep(1/3, 3))
fit.qda.4

# Slecting better model based on confusion matrix 

table(Training$rating, predict(fit.qda.1, Testing)$class)
table(Training$rating, predict(fit.qda.2, Testing)$class)
table(Training$rating, predict(fit.qda.3, Testing)$class)
table(Training$rating, predict(fit.qda.4, Testing)$class)

# Confusion matrix and APER  

pred1 <- predict(fit.qda.1, Testing)
conf1 <- confusionmatrix(Training$rating, pred1$class)
n1 <- sum(conf1)
(aper1 <- (n1 - sum(diag(conf1)))/n1)


pred2 <- predict(fit.qda.2, Testing)
conf2 <- confusionmatrix(Training$rating, pred2$class)
n2 <- sum(conf2)
(aper2 <- (n2 - sum(diag(conf2)))/n2)


pred3 <- predict(fit.qda.3, Testing)
conf3 <- confusionmatrix(Training$rating, pred3$class)
n3 <- sum(conf3)
(aper3 <- (n3 - sum(diag(conf3)))/n3)


pred4 <- predict(fit.qda.4, Testing)
conf4 <- confusionmatrix(Training$rating, pred4$class)
n4 <- sum(conf4)
(aper4 <- (n4 - sum(diag(conf4)))/n4)


# Fitting a univariate model 
Training$quality <- as.numeric(Training$quality)
Training$type <- as.numeric(Training$type)

fit.lm.full <- lm(quality ~., data = Training)

fit.lm.full <- lm(quality ~ fixed_acidity + volatile_acidity + citric_acid + 
               residual_sugar + chlorides + free_sulfur_dioxide + 
               total_sulfur_dioxide + density + pH + 
               sulphates + alcohol + type, data = Training)

summary(fit.lm.full)


# Checking for Multicollinearity 

vif(fit.lm.full) # remove density as the vif is greater that 10 fo it 

fit.lm.full.v2 <- lm(quality ~ fixed_acidity + volatile_acidity + citric_acid + 
                       residual_sugar + chlorides + free_sulfur_dioxide + 
                       total_sulfur_dioxide + pH + 
                       sulphates + alcohol + type, data = Training) 

summary(fit.lm.full.v2)

# Diagnostic Plot of the full model 

par(mfrow = c (3,2))
plot(fit.lm.full)
hist(fit.lm.full$residuals, probability = TRUE)
dev.off()


library(ggplot2)
library(GGally)
ggpairs(Training, mapping=ggplot2::aes(colour = as.factor(Training$rating)),
        lower=list(combo=wrap("facethist",binwidth=1)),
        upper = list(continuous = wrap("cor", size = 2)))


# Variable subset selection with leaps package 

library(leaps)

full.model <- regsubsets(quality ~., data = Training[, c(1:7, 9:13)], nvmax = 20) # No interactions considered for this case 
reg.summary.full.model <- summary(full.model)
reg.summary.full.model



par(mfrow = c(2, 2))
plot(reg.summary.full.model$rss, xlab = "Number of Variables", ylab = "RSS", type = "b")

plot(reg.summary.full.model$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "b")

best_adj_r2 = which.max(reg.summary.full.model$adjr2)

points(best_adj_r2, reg.summary.full.model$adjr2[best_adj_r2],
       col = "red",cex = 2, pch = 20)

plot(reg.summary.full.model$cp, xlab = "Number of Variables", ylab = "Cp", type = 'b')

best_cp = which.min(reg.summary.full.model$cp)

points(best_cp, reg.summary.full.model$cp[best_cp], 
       col = "red", cex = 2, pch = 20)

plot(reg.summary.full.model$bic, xlab = "Number of Variables", ylab = "BIC", type = 'b')

best_bic = which.min(reg.summary.full.model$bic)

points(best_bic, reg.summary.full.model$bic[best_bic], 
       col = "red", cex = 2, pch = 20)

dev.off()

(AdjR2 <- round(coef(full.model, 7), 2))
(CP <- round(coef(full.model, 6), 2))
(BIC <- round(coef(full.model, 4), 2))


# AIC Step Function Method 

library(MASS)
lm1 <- step(fit.lm.full, direction = 'backward')
summary(lm1)

# The model selected by AIC method - 6 variable model - Agrees with the leap package also 

lm.AIC <- lm(formula = quality ~ volatile_acidity + citric_acid + free_sulfur_dioxide + 
               total_sulfur_dioxide + sulphates + alcohol, data = Training)
summary(lm.AIC)

# Removing insignificant terms
lm.AIC2 <-lm(formula = quality ~ volatile_acidity + free_sulfur_dioxide + 
               total_sulfur_dioxide + alcohol, data = Training)
summary(lm.AIC2)

par(mfrow = c (3,2))
plot(lm.AIC2)
hist(lm.AIC2$residuals, probability = TRUE)
dev.off()


# Prediction with new dataset 

lm.AIC2.Pred <- predict(lm.AIC2, Testing, interval = 'prediction', level = 0.95)
lm.AIC2.Pred



############# Multivariate Model ####################
#############                    ####################

multi.lm <- lm(cbind(quality, alcohol)~., data = Training[, 1:13])
summary(multi.lm)

# MANOVA Table 

result <- manova(multi.lm)
summary(result, test = 'Wilks', type = 'II')
summary(result, test = 'Pillai', type = 'II')


Resid <- multi.lm$resid
n <- nrow(Resid)
hat.sigma <- t(Resid) %*% Resid/n
hat.sigma

sqrt(diag(hat.sigma))


par(mfrow = c(3,2))
plot(fitted(multi.lm), rstandard(multi.lm), col = as.numeric(col(fitted(multi.lm))), pch = 19, ylim = c(-3, 4))
legend("topleft", legend = paste0("response ", 1:ncol(fitted(multi.lm))), pch = 19,
       col = 1:ncol(fitted(multi.lm)), text.col = 1:ncol(fitted(multi.lm)))


plot(multi.lm$fitted.values[, 1], multi.lm$residuals[, 1])
plot(multi.lm$fitted.values[, 2], multi.lm$residuals[, 2])

qqnorm(Resid[,1], main="qq-plot of residuals Response 1")
qqline(Resid[,1])

qqnorm(Resid[,2], main="qq-plot of residuals Response 2")
qqline(Resid[,2])

dev.off()


## Prediction 

# new dataset with intercept 

intercept <- rep(1, 300)
Testing.v2 <- cbind(intercept, Testing)

# Design Matrix 

X <- model.matrix(~ fixed_acidity + volatile_acidity + citric_acid + 
                    residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide + 
                    density + pH + sulphates + type, data = Training)
m = 2 # response variables 
hat.Beta <- multi.lm$coeff
hat.Beta
r = nrow(hat.beta) - 1 # regressors 
r
Res <- multi.lm$resid
Res
n <- nrow(Res)
n
hat.Sigma <- t(Res)%*%Res/n
hat.Sigma

multiplier <- sqrt(qf(0.95,m,n-r-m)*(m*(n-r-1))/(n-r-m))

x0 <- c(1, Testing$fixed_acidity[1], Testing$volatile_acidity[1], 
        Testing$citric_acid[1], Testing$residual_sugar[1], 
        Testing$chlorides[1], Testing$free_sulfur_dioxide[1], 
        Testing$total_sulfur_dioxide[1], Testing$density[1], 
        Testing$pH[1], Testing$sulphates[1], Testing$type[1])
x0%*%hat.Beta

i = 1
(x0%*%hat.Beta)[i] - sqrt(qf(0.95,m,n-r-m)*(m*(n-r-1))/(n-r-m))* sqrt((1 + x0%*%solve(t(X)%*%X)%*%x0)*hat.Sigma[i,i]*(n/(n-r-1)))
(x0%*%hat.Beta)[i] + sqrt(qf(0.95,m,n-r-m)*(m*(n-r-1))/(n-r-m))* sqrt((1 + x0%*%solve(t(X)%*%X)%*%x0)*hat.Sigma[i,i]*(n/(n-r-1)))

i = 2
(x0%*%hat.Beta)[i] - sqrt(qf(0.95,m,n-r-m)*(m*(n-r-1))/(n-r-m))* sqrt((1 + x0%*%solve(t(X)%*%X)%*%x0)*hat.Sigma[i,i]*(n/(n-r-1)))
(x0%*%hat.Beta)[i] + sqrt(qf(0.95,m,n-r-m)*(m*(n-r-1))/(n-r-m))* sqrt((1 + x0%*%solve(t(X)%*%X)%*%x0)*hat.Sigma[i,i]*(n/(n-r-1)))


# Question 6 

#Prediction error with discrimination and classificaiton 

Validation$quality <- factor(Validation$quality, ordered = T)

Validation$rating <- ifelse(Validation$quality < 5, 'bad', ifelse(
  Validation$quality < 7, 'average', 'good'))

Validation$rating <- ordered(Validation$rating,
                           levels = c('bad', 'average', 'good'))



pred3.validation <- predict(fit.qda.3, Validation)
conf3 <- confusionmatrix(Validation$rating, pred3.validation$class)
n3 <- sum(conf3)
(aper3 <- (n3 - sum(diag(conf3)))/n3)

# Prediction error / interval for multivariate model 

for (i in 1:m) {
pred.se <- sqrt((1 + x0%*%solve(t(X)%*%X)%*%x0)*hat.Sigma[i,i]*(n/(n-r-1)))
cat("Response", i, (x0%*%hat.Beta)[i] - multiplier*pred.se,
    (x0%*%hat.Beta)[i] + multiplier*pred.se, "\n")
}



## END ##
