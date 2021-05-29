pacman::p_load(tidyverse, magrittr, caret, caretEnsemble, pROC, glmnet, rpart, e1071)

#read data in
df = read.csv("creditcard.csv")
df$Class = as.factor(df$Class)
df$Class = recode_factor(df$Class, "1"="Fraudulent", "0"="Normal")
summary(df)
?record_factor

#create training and testing sets from data
trainRowNums = createDataPartition(df$Class, p=0.8, list = F) %>% as.vector()
trainData = df[trainRowNums,]
testData = df[-trainRowNums,]

options(scipen = 999)
MODEL1 <- glm(Class ~., data = trainData, family ='binomial')
summary(MODEL1)

p1.train <-predict(MODEL1, data = trainData, type = "response")
p1.valid <-predict(MODEL1, newdata = testData, type = "response")
r1.train <-roc(trainData$Class, p1.train)

r1.valid <-roc(testData$Class, p1.valid)
m1.train.auc <- r1.train$auc
m1.valid.auc <- r1.valid$auc
m1.train.auc
m1.valid.auc

?trainControl

fitControl = trainControl(method = "cv",
                          number = 5,
                          sampling = "",
                          summaryFunction = twoClassSummary,
                          classProbs = T,
                          selectionFunction = "best",
                          savePredictions = T,
                          index = createResample(trainData$Class, times = 5))

models = caretList(Class ~ ., data = trainData, metric = "ROC",
                   tuneList = list(
                     cart = caretModelSpec(method = "rpart", tuneLength=10),
                     glm = caretModelSpec(method = "glm", family = "binomial"),
                     lasso = caretModelSpec(method = "glmnet", family="binomial",
                                            tuneGrid = expand.grid(.alpha=1,
                                                                  .lambda=seq(0.0001, 2, length=10)) )
                   ),
                   trControl = fitControl, 
                   preProcess = c("nzv", "center", "scale", "corr")
)

cvResults = resamples(models)
scales = list(x=list(relation="free"), y=list(relation="free"))

bwplot(cvResults, scales = scales)

confMatrix = lapply(models, predict, newdata = testData, type="raw") %>% 
  lapply(confusionMatrix, testData$Class) %>% 
  lapply("[[", "table")

confMatrix

