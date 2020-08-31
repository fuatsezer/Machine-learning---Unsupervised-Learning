rm(list=ls()) ##geçmiþi temizlemek için

# PCA
# Importing the dataset
Wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),
                 header=FALSE)
customer_segment=Wine[,1]
Wine=Wine[,-1]
colnames(Wine)=c("Alcohol","Malic_Acid","Ash","Ash_Alcanity","Magnesium","Total_Phenols","Flavanoids","Nonflavanoid_Phenols","Proanthocyanins","Color_Intensity","Hue","OD280","Proline")

dim(Wine)
head(Wine)
summary(Wine)


install.packages("pastecs") ##daha ayrýntýlý tanýmlayýcý istatistikler elde etmek için
library(pastecs)
stat.desc(Wine)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
#library(caTools)
#set.seed(123)
#split = sample.split(Wine$Alcohol, SplitRatio = 0.8)
#training_set = subset(Wine, split == TRUE)
#test_set = subset(Wine, split == FALSE)


# Feature Scaling
#training_set[-14] = scale(training_set[-14])
#test_set[-14] = scale(test_set[-14])


install.packages("corrplot")
library("corrplot")
corr=cor((Wine), method = "pearson")
eigen=eigen(corr)
eigen$values
eigen$vectors

covv=cov(Wine)
eigen_cov=eigen(covv)
eigen_cov


# Applying PCA
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)
pca = preProcess(Wine, method = 'pca', pcaComp = 2)
pca = preProcess(Wine, method = 'pca', thresh = 0.50)




####pca ile ilgili daha ayrýntýlý analiz için farklý paket 
install.packages("stats")
library("stats")
wine.pca <- prcomp(Wine, center = TRUE, scale. = TRUE)
summary(wine.pca)
sqrt(eigen$values) ##korelasyon matrisinden elde edilen özdeðerlerin karekökü her bir özdeðer varyansý veriir karekökü ise standart sapmasýdýr.)


wine.pca_cov <- prcomp(Wine, center = TRUE, scale. = FALSE) ##varyans-kovaryans matrisi üzerinden pca yapýlýrsa 
summary(wine.pca_cov)
sqrt(eigen_cov$values) ##korelasyon matrisinden elde edilen 

plot(wine.pca)
screeplot(wine.pca, type='lines')
biplot(wine.pca)
biplot(wine.pca, choices = 6:7)


eigen$vectors[,1:2]
wine.pca$rotation[,1:2]


##dönüþtürülmüþ yeni veri seti W
scores=(scale(Wine))%*%(wine.pca$rotation) ### skor deðerleri
scores[1:5,1:2] ##1. ve 2. temel bileþenlerin ilk 5 skor deðeri
##veya
wine.pca$x  ## skor deðerleri
wine.pca$x[1:5,1:2] ##1. ve 2. temel bileþenlerin ilk 5 skor deðeri
##veya
predict(wine.pca) ## skor deðerleri
predict(wine.pca)[1:5,1:2] ##1. ve 2. temel bileþenlerin ilk 5 skor deðeri


cor(Wine) ##orjinal verinin korealasyon matrisi
t(predict(wine.pca))%*%predict(wine.pca)###PC skor deðerleri birbiri ile korele deðildir.


