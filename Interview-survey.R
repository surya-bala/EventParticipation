inputs <- read.csv("C:/Users/suryabala/Desktop/R project/Survey.csv")
set.seed(100)
trainingRowIndex <- sample(1:nrow(inputs),0.8*nrow(inputs))
trainingData <- inputs[trainingRowIndex,]
testingData <- inputs[-trainingRowIndex,]


##################### Decision tree ##################
####################### Method 1 #####################

library("rpart")
library("rpart.plot")
library("RColorBrewer")

tree <- rpart(Observed.Attendance ~  Industry + Interview.Type + Ques1+ Ques2 + Ques3 + Ques4 + Ques5, 
              method="class",
              parms=list(split='gini'),
              data = trainingData)
prp(tree, extra=1, faclen=0,  nn=T, box.palette="BlGnYl")
predictedValue <- predict(tree,testingData,type = "class")

confMat <- table(cbind(actuals=testingData$Observed.Attendance),cbind(predicteds=predictedValue))
accuracy1 <- (1 - (sum(diag(confMat))/sum(confMat)))*100
accuracy1

####################### Method 2 ######################

library(party)
output.tree <- ctree(Observed.Attendance ~  Industry + Interview.Type + Ques1+ Ques2 + Ques3 + Ques4 + Ques5, data = trainingData)
plot(output.tree)

predictedValue2 <- predict(output.tree,testingData)

confMat2 <- table(cbind(actuals=testingData$Observed.Attendance),cbind(predicteds=predictedValue2))
accuracy2 <- (1 - (sum(diag(confMat2))/sum(confMat2)))*100
accuracy2

print(c(accuracy1,accuracy2))
