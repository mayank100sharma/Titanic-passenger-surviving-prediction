#Read the file
tt <- read.csv("titanic.csv")

#Checking the dataset
str(tt)
#Settin the seeds
set.seed(17)

#Calling the random forest
tt.rf <- randomForest(Survived ~ . , data = tt, mtry = 2, importance = TRUE, do.trace = 100)
print(tt.rf)

#Installing ipred library to use errorest function for comparison
library(ipred)
set.seed(131)

#10 repitition of 10 fold-cross validation through SVM
error.RF <- numeric(10)
for(i in 1:10) error.RF[i] <- errorest(Survived ~ . , data = tt, model = randomForest, mtry = 2)$error

summary(error.RF)

#Installing e1071 to use SVM
library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for(i in 1:10) error.SVM[i] <- errorest(Survived ~ . , data = tt, model = svm, coat = 10, gamma = 1.5)$error

summary(error.SVM)

#Using importance variable to build simpler models
par(mfrow = c(2,2))

for (i in 1:4)
  plot(sort(tt.rf$importance[ ,i], dec = TRUE), type = "h", main = paste("Measure", i))
