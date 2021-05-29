pacman::p_load(tidyverse, magrittr, jsonlite, igraph, robustHD, pROC, caret)

df = read.csv("Portmap.csv", stringsAsFactors = F)

df %<>% select(c(Source.IP, Destination.IP, Total.Fwd.Packets, Total.Backward.Packets, 
                Fwd.IAT.Total, Bwd.IAT.Total, Fwd.PSH.Flags,
                 Bwd.Header.Length, Fwd.Packets.s, 
                 Bwd.Packets.s, Min.Packet.Length, Max.Packet.Length, Average.Packet.Size, Active.Mean, Idle.Mean, SimillarHTTP,
                 Inbound, Label))

str(df)
summary(df)


df = select(df,-SimillarHTTP)
df = select(df,-Source.IP)
df = select(df,-Destination.IP)


# Started with 24 variables, ended with 15 for analysis. Dropped based on near-zero variance, low predictive power
# and negative values.
# Decided to drop IP variables rather than group them into a factor because there are a number of points in the data where the IP addresses do not
# match up to a location. Most likely because they are private. Also, converting them to numeric did not make much sense to us in
# regard to our own model.

# Changing varibales to factors
df$Inbound = as.factor(df$Inbound)
df$Fwd.PSH.Flags = as.factor(df$Fwd.PSH.Flags)
df$Label = recode_factor(df$Label, "1"="Portmap", "0"="Benign")

# Visualizing the data

ggplot(df,aes(x=Label, y=Active.Mean)) + geom_point()+ggtitle("Scatter Plot of Active Mean")
ggplot(df,aes(x=Average.Packet.Size)) + geom_histogram(bins=20) + ggtitle("Histogram: Average.Packet.Size")
ggplot(df,aes(x=Label, y=Average.Packet.Size)) + geom_point() + ggtitle("Scatter Plot: Average.Packet.Size")

df_grouped1 = df %>% group_by(Label) %>% select_if(is.numeric) %>% summarise_all(list(avg = mean, stdDev = sd)) 

df_grouped1

# Scaling the numeric variables
df2 = df %>% select_if(is.numeric) %>% scale() %>% data.frame()

# Adding categorical variables back into the dataframe
df2['Inbound']= df['Inbound']
df2['Fwd.PSH.Flags']= df['Fwd.PSH.Flags']
df2['Label']= df['Label']


### Partitioning the Data
trainRowNums = createDataPartition(df2$Label, p = 0.8, list = F) %>% as.vector()

trainData = df[trainRowNums,]
testData = df[-trainRowNums,]


### Model Building
options(scipen = 999)
MODEL1 <- glm(Label ~., data = trainData, family ='binomial')
summary(MODEL1)
summary(df2)


null <-glm(Label~1, data = trainData, family = "binomial")
full <-glm(Label~Total.Fwd.Packets + Fwd.PSH.Flags + Min.Packet.Length +
            Inbound + Active.Mean, data = trainData, family = "binomial")
MODEL2 <-step(null,list(lower =formula(null), upper =formula(full)),data = trainData, direction = "both", trace = 0)
summary(MODEL2)

MODEL3 = glm(Label~Total.Fwd.Packets + Fwd.PSH.Flags + Active.Mean + Inbound, data = trainData, family = "binomial")

p3.train <-predict(MODEL3, data = trainData, type = "response")
p3.valid <-predict(MODEL3, newdata = testData, type = "response")
r3.train <-roc(trainData$Label, p3.train)

r3.valid <-roc(testData$Label, p3.valid)
m3.train.auc <- r3.train$auc
m3.valid.auc <- r3.valid$auc
m3.train.auc
m3.valid.auc


plot(r3.valid)
