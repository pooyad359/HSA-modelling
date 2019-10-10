library("readr")
file.name<-"rawoutput.csv"
results<-read_csv(file.name, na = "NA")

results[[1]]<-as.double(results[[1]])
results[is.na(results[[1]]),1]<-53.5
results[results[[1]]<49,1]<-51
results[results[[1]]>57,1]<-55.5
summary(results)

## SAVING OUTPUT AS CSV

head(results)
write.csv(df,file="ModelOutput.csv",row.names = F)
