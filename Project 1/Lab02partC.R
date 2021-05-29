setwd("~/Documents/School/RStudio Workspace.nosync/Project 1")
pacman::p_load(tidyverse, magrittr, jsonlite, igraph, robustHD, caret, pROC, devtools)
install_github("ltorgo/DMwR2",ref="master")
MSSQLcolnames = read.csv("MSSQL.csv", nrows = 1, header = TRUE)
dfMSSQL = DMwR2::sampleCSV("MSSQL.csv", percORn = 0.15, header = TRUE)
colnames(dfMSSQL) <- colnames(MSSQLcolnames)

dfMSSQL %<>% select(c(Total.Fwd.Packets, Total.Backward.Packets, 
                 Fwd.IAT.Total, Bwd.IAT.Total, Fwd.PSH.Flags,
                 Bwd.Header.Length, Fwd.Packets.s, 
                 Bwd.Packets.s, Min.Packet.Length, Max.Packet.Length, Average.Packet.Size, Active.Mean, Idle.Mean,
                 Inbound, Label))

summary(dfMSSQL)

ggplot(dfMSSQL,aes(x=Average.Packet.Size)) + geom_histogram(bins=20)+ggtitle("MSSQL: Histogram of Average Packet Size")
ggplot(dfMSSQL,aes(x=Label, y=Active.Mean)) + geom_point()+ggtitle("MSSQL: Scatter Plot of Active Mean")

MSSQL_grouped = dfMSSQL %>% group_by(Label) %>% select_if(is.numeric) %>% summarise_all(list(avg = mean, stdDev = sd))
MSSQL_grouped = data.frame(MSSQL_grouped)
MSSQL_grouped
