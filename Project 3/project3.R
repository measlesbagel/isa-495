setwd("~/Documents/School/RStudio Workspace.nosync/Project 3")

install.packages("pacman")
pacman::p_load(tidyverse, magrittr, lubridate, hms, iptools, corrplot, caretEnsemble, DMwR, caret, pROC, glmnet, rpart, e1071)

#add headers from text file
featuresDF = read.csv("featMat.csv", header = F)

colnames(featuresDF) = c('user id',
                         'doc id',
                         'inter-stroke time',
                         'stroke duration',
                         'start x',
                         'start y',
                         'stop x',
                         'stop y',
                         'direct end-to-end distance',
                         'mean resultant length',
                         'up/down/left/right flag',
                         'direction of end-to-end line',
                         'phone id',
                         '20% pairwise velocity',
                         '50% pairwise velocity',
                         '80% pairwise velocity',
                         '20% pairwise acc',
                         '50% pairwise acc',
                         '80% pairwise acc',
                         'median velocity at last 3 pts',
                         'largest deviation from end-to-end line',
                         '20% dev. from end-to-end line',
                         '50% dev. from end-to-end line',
                         '80% dev. from end-to-end line',
                         'average direction',
                         'length of trajectory',
                         'ratio end-to-end dist and length of trajectory',
                         'average velocity',
                         'median acceleration at first 5 points',
                         'mid-stroke pressure',
                         'mid-stroke area covered',
                         'mid-stroke finger orientation',
                         'change of finger orientation',
                         'phone orientation')

#str(featuresDF)
#sapply(featuresDF, class) %>% table()
#anything thats integer recode to factor or numeric
?recode_factor

featuresDF %<>% mutate_if(is.integer, as.factor)
user2DF <- featuresDF
user2DF$`user id` <- if_else(featuresDF$`user id` == '2', 'user', "not")
user2DF$`user id` <- as.factor(user2DF$`user id`)
summary(user2DF)
df <- user2DF

#Inter week
train1 = data.frame()
test1 = data.frame()

train1 = filter(df, df$`doc id` == "1" | df$`doc id` == "2" | df$`doc id` == "3")
test1 = filter(df, df$`doc id` == "6")

trainNA = na.omit(train1)
trainNA %<>% select(-'change of finger orientation', -'phone id', -'doc id')

testNA = na.omit(test1)
testNA %<>% select(-'change of finger orientation', -'phone id', -'doc id')

#Inter session
train2 = data.frame()
test2 = data.frame()

train2 = filter(df, df$`doc id` == "1" | df$`doc id` == "2")
test2 = filter(df, df$`doc id` == "3")

trainNA2 = na.omit(train2)
trainNA2 %<>% select(-'change of finger orientation', -'phone id', -'doc id')

testNA2 = na.omit(test2)
testNA2 %<>% select(-'change of finger orientation', -'phone id', -'doc id')

#Intra session
train3 = data.frame()
test3 = data.frame()

train3 = filter(df, df$`doc id` == "2")
test3 = filter(df, df$`doc id` == "2")

trainNA3 = na.omit(train3)
trainNA3 %<>% select(-'change of finger orientation', -'phone id', -'doc id')

testNA3 = na.omit(test3)
testNA3 %<>% select(-'change of finger orientation', -'phone id', -'doc id')

fitControl = trainControl(method = "repeatedcv",
                          number = 5,
                          sampling = "down",
                          summaryFunction = twoClassSummary,
                          classProbs = T,
                          selectionFunction = "best",
                          savePredictions = T,
                          index = createResample(trainNA$`user id`, times = 5))

fitControl2 = trainControl(method = "repeatedcv",
                          number = 5,
                          sampling = "down",
                          summaryFunction = twoClassSummary,
                          classProbs = T,
                          selectionFunction = "best",
                          savePredictions = T,
                          index = createResample(trainNA2$`user id`, times = 5))

fitControl3 = trainControl(method = "repeatedcv",
                          number = 5,
                          sampling = "down",
                          summaryFunction = twoClassSummary,
                          classProbs = T,
                          selectionFunction = "best",
                          savePredictions = T,
                          index = createResample(trainNA3$`user id`, times = 5))

models = caretList(`user id` ~ ., data = trainNA, metric = "ROC",
                   tuneList = list(
                     svm = caretModelSpec(method = "svmRadial", family = "binomial"),
                     knn = caretModelSpec(method = "knn")
                   ),
                   trControl = fitControl,
                   preProcess = c("nzv", "center", "scale", "corr")
)

models2 = caretList(`user id` ~ ., data = trainNA2, metric = "ROC",
                   tuneList = list(
                     svm = caretModelSpec(method = "svmRadial", family = "binomial"),
                     knn = caretModelSpec(method = "knn")
                   ),
                   trControl = fitControl2,
                   preProcess = c("nzv", "center", "scale", "corr")
)

models3 = caretList(`user id` ~ ., data = trainNA3, metric = "ROC",
                   tuneList = list(
                     svm = caretModelSpec(method = "svmRadial", family = "binomial"),
                     knn = caretModelSpec(method = "knn")
                   ),
                   trControl = fitControl3,
                   preProcess = c("nzv", "center", "scale", "corr")
)

cvResults = resamples(models)
scales = list(x=list(relation="free"), y=list(relation="free"))

bwplot(cvResults, scales = scales)

confMatrix = lapply(models, predict, newdata = testNA3, type="raw") %>% 
  lapply(confusionMatrix, testNA3$`user id`) %>% 
  lapply("[[", "table")

confMatrix

?lapply
warnings()
traceback()
#remove user id, phone id, and doc id from data training

summary(train1)
summary(featuresDF)
#if numbers go toward infinity change them to stop at some large number
#focus on vertical scrolling

