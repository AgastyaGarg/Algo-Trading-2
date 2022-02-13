install.packages("readxl")
library("readxl")
setwd("E:/Manipal Notes and Labs/Labs/7th sem/Data Analytics lab/Nitish_170911150/Lab3")
#1
data<-read_excel("Online Retail.xlsx")

#2
UKcust<-subset(data, Country == "United Kingdom", select = Quantity)
UKcust
sum(UKcust)

#3
data3<-subset(data,CustomerID == 13744 & as.Date(data$InvoiceDate) == "2011-02-20")
data3
total<-sum(data3$Quantity*data3$UnitPrice)
total

#4
data4<-subset(data, Country == "France", select = c(Quantity,Description))
data4
df4<-data.frame(Quantity=numeric(),Description=character())
for(i in 1:nrow(data4)){
  x<-data4[i,]
  if(x$Description %in% df4$Description)
    df4[which(df4$Description==x$Description),1]<-df4[which(df4$Description==x$Description),1]+x["Quantity"]
  else
    df4[nrow(df4)+1,]<-x                   
}
df4[which.max(df4$Quantity),]

#5
y<-as.Date(data$InvoiceDate)
data5<-subset(data,format(y,format="%Y")=="2011" & quarters.Date(y)=="Q2",select = c(Quantity,Description))
df5<-data.frame(Quantity=numeric(),Description=character())
for(i in 1:nrow(data5)){
  x<-data5[i,]
  if(x$Description %in% df5$Description)
    df5[which(df5$Description==x$Description),1]<-df5[which(df5$Description==x$Description),1]+x["Quantity"]
  else
    df5[nrow(df5)+1,]<-x                   
}
df5<-df5[order(df5$Quantity,decreasing = TRUE),]
head(df5)

#6
data6<-subset(data,format(as.Date(data$InvoiceDate),format="%Y - %m")=="2011 - 04",select=c(Quantity,UnitPrice))
prod<-data6$Quantity*data6$UnitPrice
avg<-sum(prod)/nrow(data6)
avg

#7
q3_2010<-subset(data,format(y,format="%Y")=="2010" & quarters.Date(y)=="Q3",select = c(Quantity,UnitPrice))
q3_2011<-subset(data,format(y,format="%Y")=="2011" & quarters.Date(y)=="Q3",select = c(Quantity,UnitPrice))
print(c("2010 : ",sum(q3_2010$Quantity*q3_2010$UnitPrice)))
print(c("2011 : ",sum(q3_2011$Quantity*q3_2011$UnitPrice)))

#8
data8<-subset(data,format(as.Date(data$InvoiceDate),format="%Y - %m")=="2010 - 12",select=CustomerID)
nrow(unique(data8))

#9
data9<-data[c("Quantity","UnitPrice","InvoiceDate")]
data9$InvoiceDate<-paste(format(data9$InvoiceDate,format="%Y"),quarters.Date(data9$InvoiceDate),sep=" ")
df9<-c()
for(i in unique(data9$InvoiceDate)){
  s<-subset(data9,data9$InvoiceDate==i,select=c(Quantity,UnitPrice))
  df9<-append(df9,sum(s$Quantity*s$UnitPrice))
}
plot(factor(unique(data9$InvoiceDate)),df9,type="p")

