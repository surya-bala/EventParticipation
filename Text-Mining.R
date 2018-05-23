Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_65')

library(qdap)
library(tm)
library(stringi)

txt <- readLines("C:/Users/suryabala/Desktop/R project/predict.txt")
sentence <- as.vector(sent_detect_nlp(txt))

######################### Multi-valued Attributes ##########################

clients <- c("Hospira","Aon Hewitt","UST","ANZ","Standard Chartered Bank","Pfizer","Aon hewitt Gurgaon","Astrazeneca","Flextronics",
             "Prodapt","Williams Lea","BarclaysHewitt","Woori Bank")
Client.name <- extract.value(sentence,clients)
Client.name

industries <- c("Pharmaceuticals","BFSI","Telecom",
                "IT Products and Services Electronics","IT")
Industry <- extract.value(sentence,industries)
Industry

types <- c("Scheduled walkin","Scheduled","Walkin")
Interview.Type <- extract.value(sentence,types)
Interview.Type
predictsData <- inputs[162,]

######################### Binary Attributes ###############################


negatives <- classify.sentence(sentence,negative.words)
positives <- sentence[! sentence %in% negatives]
positives

if(check.value(negatives,"permission")){
  Ques1 <- "No"
}else if(check.value(positives,"permission")){
  Ques1 <- "Yes"
}else{
  Ques1 <- "unknown"
}
Ques1 

if(check.value(negatives,"call three hours before the interview.")){
  Ques2 <- "No"
}else if(check.value(positives,"call three hours before the interview.")){
  Ques2 <- "Yes"
}else{
  Ques2 <- "unknown"
}
Ques2

if(check.value(negatives,c("alternative number","desk number"))){
  Ques3 <- "No"
}else if(check.value(positives,c("alternative number","desk number"))){
  Ques3 <- "Yes"
}else{
  Ques3 <- "unknown"
}
Ques3 

if(check.value(negatives,c("updated resume","resume","JD"))){
  Ques4 <- "No"
}else if(check.value(positives,c("updated resume","resume","JD"))){
  Ques4 <- "Yes"
}else{
  Ques4 <- "unknown"
}
Ques4 

if(check.value(negatives,c("call letter","offer letter"))){
  Ques5 <- "No"
}else if(check.value(positives,c("call letter","offer letter"))){
  Ques5 <- "Yes"
}else{
  Ques5 <- "unknown"
}
Ques5 

predictData <- data.frame(Client.name = Client.name, Industry = Industry, Interview.Type = Interview.Type, Ques1 = Ques1, Ques2 = Ques2, Ques3 = Ques3,Ques4 = Ques4, Ques5 = Ques5)
#inputs <- read.csv("C:/Users/suryabala/Desktop/R project/Survey.csv")
#outputs <- ctree(Observed.Attendance ~  Industry + Interview.Type + Ques1+ Ques2 + Ques3 + Ques4 + Ques5, data = inputs)
#plot(outputs)


predicted <- predict(output.tree,predictsData)
predicted

