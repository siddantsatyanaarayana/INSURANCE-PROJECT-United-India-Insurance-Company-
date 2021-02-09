library(readxl)
mydata <- read_xlsx("C:/Users/Siddant/Pictures/Analytics/Insurance_Data.xlsx")
View(mydata)
colSums(is.na(mydata))

mydata$Age <- ifelse(is.na(mydata$Age), median(mydata$Age, na.rm = T), mydata$Age)
sum(is.na(mydata$Age))
median(mydata$Family_Members)
mydata$Family_Members <- ifelse(is.na(mydata$Family_Members),median(mydata$Family_Members,na.rm = T), mydata$Family_Members)
sum(is.na(mydata$Family_Members))
table(mydata$Education)
mydata$Education <- ifelse(is.na(mydata$Education),"BD", mydata$Education)
sum(is.na(mydata$Education))
table(mydata$Occupation)
mydata$Occupation <- ifelse(is.na(mydata$Occupation),"SPT", mydata$Occupation)
sum(is.na(mydata$Occupation))
table(mydata$Job_Title)
mydata$Job_Title <- ifelse(is.na(mydata$Job_Title),"FH", mydata$Job_Title)
sum(is.na(mydata$Job_Title))
library(psych)
table(mydata$Occupation)
library(dplyr)
# grouping income in terms of occupation
x <- mydata%>%group_by(Occupation)%>%summarise(x=mean(Income,na.rm = T))
View(x)
y <- mydata%>%group_by(Occupation)%>%summarise(x=median(Income,na.rm = T))
View(y)
# replace SE and SFT income by median and SPT income by mean
mydata$Income[mydata$Occupation=="SE"] <- ifelse(is.na(mydata$Income[mydata$Occupation=="SE"]),2.316235,mydata$Income[mydata$Occupation=="SE"])
sum(is.na(mydata$Income))
median(mydata$Income[mydata$Occupation=="SE"])
mydata$Income[mydata$Occupation=="SFT"] <- ifelse(is.na(mydata$Income[mydata$Occupation=="SFT"]),2.000000,mydata$Income[mydata$Occupation=="SFT"])
sum(is.na(mydata$Income))
median(mydata$Income[mydata$Occupation=="SFT"])
mydata$Income[mydata$Occupation=="SPT"] <- ifelse(is.na(mydata$Income[mydata$Occupation=="SPT"]),6.889731,mydata$Income[mydata$Occupation=="SPT"])
sum(is.na(mydata$Income))
mean(mydata$Income[mydata$Occupation=="SPT"])
library(rpivotTable)
rpivotTable(mydata)
unique(mydata$Current_Product_Type)

# current_product

mydata$Current_Product[mydata$Current_Product_Type=="NA"||mydata$Current_Coverage==0] <- ifelse(is.na(mydata$Current_Product[mydata$Current_Product_Type=="NA"||mydata$Current_Coverage==0]),"No","Yes")
sum(is.na(mydata$Current_Product))
table(mydata$Current_Product)

#current_product_type

sum(is.na(mydata$Current_Product_Type))
table(mydata$Current_Product_Type)
mydata$Current_Product_Type[mydata$Current_Product=="No"||mydata$Current_Coverage==0] <- ifelse(is.na(mydata$Current_Product_Type[mydata$Current_Product=="No"||mydata$Current_Coverage==0]),"NA",mode(mydata$Current_Product_Type))

# current_coverage

sum(is.na(mydata$Current_Coverage))


# new product type

table(mydata$New_Product_Type)
sum(is.na(mydata$New_Product_Type))
mydata$New_Product_Type <- ifelse(is.na(mydata$New_Product_Type),"ANS",mydata$New_Product_Type)
p <- mydata%>%group_by(New_Product_Type)%>%summarise(x=mean(New_Coverage,na.rm = T))
View(p)
q <- mydata%>%group_by(New_Product_Type)%>%summarise(x=median(New_Coverage,na.rm = T))
View(q)
mydata$New_Coverage[mydata$New_Product_Type=="ANS"] <- ifelse(is.na(mydata$New_Coverage[mydata$New_Product_Type=="ANS"]),3e+06,mydata$New_Coverage[mydata$New_Product_Type=="ANS"])
mydata$New_Coverage[mydata$New_Product_Type=="END"] <- ifelse(is.na(mydata$New_Coverage[mydata$New_Product_Type=="END"]),3e+06,mydata$New_Coverage[mydata$New_Product_Type=="END"])
mydata$New_Coverage[mydata$New_Product_Type=="INV"] <- ifelse(is.na(mydata$New_Coverage[mydata$New_Product_Type=="INV"]),3e+06,mydata$New_Coverage[mydata$New_Product_Type=="INV"])
mydata$New_Coverage[mydata$New_Product_Type=="PMT"] <- ifelse(is.na(mydata$New_Coverage[mydata$New_Product_Type=="PMT"]),3e+06,mydata$New_Coverage[mydata$New_Product_Type=="PMT"])
mydata$New_Coverage[mydata$New_Product_Type=="TLE"] <- ifelse(is.na(mydata$New_Coverage[mydata$New_Product_Type=="TLE"]),2e+06,mydata$New_Coverage[mydata$New_Product_Type=="TLE"])
sum(is.na(mydata$New_Coverage))

# replacing missing values for rating

unique(mydata$Rating)
mydata$Rating[mydata$Current_Product_Type=="ANS"&!is.na(mydata$Current_Product_Type)] <- ifelse(is.na(mydata$Rating[mydata$Current_Product_Type=="ANS"&!is.na(mydata$Current_Product_Type)]),"Cold",mydata$Rating[mydata$Current_Product_Type=="ANS"&!is.na(mydata$Current_Product_Type)])
mydata$Rating[mydata$Current_Product_Type=="END"&!is.na(mydata$Current_Product_Type)] <- ifelse(is.na(mydata$Rating[mydata$Current_Product_Type=="END"&!is.na(mydata$Current_Product_Type)]),"Hot",mydata$Rating[mydata$Current_Product_Type=="END"&!is.na(mydata$Current_Product_Type)])
mydata$Rating[mydata$Current_Product_Type=="INV"&!is.na(mydata$Current_Product_Type)] <- ifelse(is.na(mydata$Rating[mydata$Current_Product_Type=="INV"&!is.na(mydata$Current_Product_Type)]),"Warm",mydata$Rating[mydata$Current_Product_Type=="INV"&!is.na(mydata$Current_Product_Type)])
mydata$Rating[mydata$Current_Product_Type=="ANS"&!is.na(mydata$Current_Product_Type)] <- ifelse(is.na(mydata$Rating[mydata$Current_Product_Type=="ANS"&!is.na(mydata$Current_Product_Type)]),"Cold",mydata$Rating[mydata$Current_Product_Type=="ANS"&!is.na(mydata$Current_Product_Type)])
