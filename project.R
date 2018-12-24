library(caret)
# read train & test data
training <- read.csv('C:/Users/ibrahim/Desktop/P8 week 4/pml-training.csv')
testing  <- read.csv('C:/Users/ibrahim/Desktop/P8 week 4/pml-testing.csv')
training <- training[c(2:160)]
ColsNames <- names(training)
lenCol = 19622
count = 1
for (colName in ColsNames) {
  currentCol = training[colName]
  currentCol = currentCol[[colName]]
  lenNA   = length(subset(currentCol , is.na(currentCol)))
  if(is.factor(currentCol)){
    colAsStr <- as.character(currentCol)
    lenSpace <- length(subset(colAsStr , colAsStr == ""))
    lenNA = lenNA + lenSpace  
  }
  if(((lenNA / lenCol) * 100) < 30)
  {
    
    if(!is.character(currentCol)){
        if(exists('newTraining'))
        {
          colN[count] = colName
          newTraining <- cbind(newTraining , new = currentCol)  
        }
        else
        {
          colN = colName
          newTraining <- data.frame(new = currentCol)
        }
        colnames(newTraining) <- colN
        count = count + 1
    }
  }
}
intrain <- createDataPartition(newTraining$classe , p= 0.6,list = T)
trainSet <- newTraining[intrain[[1]],]
testSet <- newTraining[-intrain[[1]],]
newTesting <- testing[colnames(newTraining)[1:(dim(newTraining)[2] - 1)]]
mod_1  = train(classe ~ . , mathod = 'rf',data = trainSet)
pred_1 = predict(mod_1,testSet)
mod_2  = train(classe ~ . , mathod = 'glm',data = trainSet)
pred_2 = predict(mod_2,testSet)
mod_3  = train(classe ~ . , mathod = 'gam',data = trainSet)
pred_3 = predict(mod_3,testSet)


# get rf accuracy
acc_1 <- (table(as.character(pred_1) == as.character(testSet$classe))[2]/length(as.character(pred_1)))[[1]]
# get glm accuracy
acc_2 <- (table(as.character(pred_2) == as.character(testSet$classe))[2]/length(as.character(pred_1)))[[1]]
# get gam accuracy
acc_3 <- (table(as.character(pred_3) == as.character(testSet$classe))[2]/length(as.character(pred_1)))[[1]]
# get combine classification accuracy

#select the big accuracy and use it to predict class
if(acc_1 > acc_2 && acc_1 > acc_3 && acc_1 > acc_All){
  predict(mod_1,testing)
}else if(acc_2 > acc_3 && acc_2 > acc_All){
  predict(mod_2,testing)
}else{
  predict(mod_3,testing)
}