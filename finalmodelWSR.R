## NOTE: THE FOLLOWING CODE READS THE DATA FROM CSV FILE AND SEND IT TO A WEB SERVICE BASED ON AN AZURE ML
## STUDIO MODEL. AT THE END THE TOP ROWS OF THE RESULTS WILL BE SHOWN BUT THE ENTIRE RESULT WILL BE SAVED
## IN "ModelOutput.csv". PLEASE MAKE SURE THE FOLLOWING PACKAGES ARE INSTALLED: "RCurl", "rjson", "readr"


library("RCurl")
library("rjson")
library("readr")

## PREPROCESSING
file.name<-"sample.csv" #<--------PLEASE ENTER THE NAME OF THE FILE HERE


data <-  read_csv(file.name, col_types = cols(timestamp = col_character(), WQI8100XCL1.CPV = col_character(), 
                                                 XI84201.PV = col_character(), XI84202.PV = col_character(), 
                                                 XI84123.PV = col_character(), XI84124.PV = col_character(), 
                                                 XI84125.PV = col_character(), FX87211.CPV1 = col_character(), 
                                                 FIC87211.PV = col_character(), FIC87211.SV = col_character(), 
                                                 FX87211.P01 = col_character(), FI87208.PV = col_character(), 
                                                 AIC88049.PV = col_character(), ZI88001.PV = col_character(), 
                                                 NIC88002.PV = col_character(), PIC88007.PV = col_character(), 
                                                 LIC88006.PV = col_character(), AIC88055.PV = col_character(), 
                                                 FIC88022.PV = col_character(), II88151.PV = col_character(), 
                                                 II88152.PV = col_character(), SI88033.PV = col_character(), 
                                                 SI88034.PV = col_character(), MQI88024.CPV = col_character(), 
                                                 FV88156.PV = col_character(), FV88043.PV = col_character(), 
                                                 FV88044.PV = col_character()), 
                    na = "NA")

names(data)<-NULL
model.input=list()
n<-nrow(data)

for(i in 1:n){
  model.input[[i]]<-as.list(data[i,])
  model.input[[i]][[1]]<-""
}

## SENDING DATA TO WEB SERVICE

# Accept SSL certificates issued by public Certificate Authorities
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

h = basicTextGatherer()
hdr = basicHeaderGatherer()


req = list(
  
  Inputs = list(
    
    
    "input1" = list(
      "ColumnNames" = list("timestamp", "WQI8100XCL1.CPV", "XI84201.PV", "XI84202.PV", "XI84123.PV", "XI84124.PV", "XI84125.PV", "FX87211.CPV1", "FIC87211.PV", "FIC87211.SV", "FX87211.P01", "FI87208.PV", "AIC88049.PV", "ZI88001.PV", "NIC88002.PV", "PIC88007.PV", "LIC88006.PV", "AIC88055.PV", "FIC88022.PV", "II88151.PV", "II88152.PV", "SI88033.PV", "SI88034.PV", "MQI88024.CPV", "FV88156.PV", "FV88043.PV", "FV88044.PV"),
      "Values" = model.input
    )                ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "phqFZcWOqJ0BkKXfUtPT8VgVso3eKt4H9HzqusLVcLrstG+gCsuZfEiRzu2CHjsdJVFuzOqUlNA3RnYCbK07sA==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

h$reset()
curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/99f71d5f9b7e4390b6a614c705502715/services/187910cb963e4fdba846648e2615a84d/execute?api-version=2.0&details=true",
            httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
            postfields=body,
            writefunction = h$update,
            headerfunction = hdr$update,
            verbose = TRUE
)

headers = hdr$value()
httpStatus = headers["status"]
if (httpStatus >= 400)
{
  print(paste("The request failed with status code:", httpStatus, sep=" "))
  
  # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
  print(headers)
}

## WEB SERVICE OUTPUT
result = h$value()
web.output<-fromJSON(result)
estimated.target<-web.output$Results$output1$value$Values

## POSTPROCESSING DATA
estimated.target<-as.double(estimated.target)
estimated.target[is.na(estimated.target)]<-53.5
estimated.target[estimated.target<49]<-51
estimated.target[estimated.target>57]<-55.5
summary(estimated.target)

## SAVING OUTPUT AS CSV
df<-data.frame(target=estimated.target)
head(df)
write.csv(df,file="ModelOutput.csv",row.names = F)
