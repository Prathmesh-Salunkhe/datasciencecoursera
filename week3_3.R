url1<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url1,'house.csv')

df<-read.csv('house.csv')
head(df)

agricultureLogical=df$ACR ==3 & df$AGS == 6
which(agricultureLogical)
df[which(agricultureLogical),]

library(jpeg)
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url2,'getdata_jeff')
img =readJPEG('getdata_jeff.jpg',native = TRUE)
img
quantile(img,c(0.3,0.8))

url3<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url3,'GDP.csv')
gdp<-read.csv("GDP.csv",skip = 4,nrows=190)

fgdp<-gdp[,c(1,2,4,5)]
colnames(fgdp)<-c('CountryCode', 'Ranking', 'Economy', 'GDP')
head(fgdp)


url4<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url4,"edu.csv")
edu<-read.csv("edu.csv")

merged <- merge(fgdp,edu,by='CountryCode',all = FALSE)

library(plyr)

arr <-arrange(merged,desc(Ranking))
arr[13,]


tapply(merged$Ranking,merged$Income.Group,mean)

library(Hmisc)
merged$rankgrp <- cut2(merged$Ranking,g=5)
tab = table(merged$rankgrp,merged$Income.Group)
tab[4]
