download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",destfile='dataOnline.csv')
df <-read.csv("dataOnline.csv")
df
nrow(df)
colnames(df)

sum(df$VAL == 24 & !is.na(df$VAL ))

furl="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(url=furl,destfile = 'NaturalGas.xlsx')

library(xlsx)
dat = read.xlsx('NaturalGas.xlsx',1,rowIndex = 18:23,colIndex = 7:15,header = TRUE)
sum(dat$Zip*dat$Ext,na.rm=T)

library(XML)
doc <-  xmlTreeParse("http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml",useInternal = TRUE)
rootnode <- xmlRoot(doc)
sum(xpathSApply(rootnode,"//zipcode",xmlValue) == 21231)

library(data.table)
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv','./Q5.csv','curl')

DT = fread('./Q5.csv')

tapply(DT$pwgtp15,DT$SEX,mean)


sapply(split(DT$pwgtp15,DT$SEX),mean)


rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]


mean(DT$pwgtp15,by=DT$SEX)


DT[,mean(pwgtp15),by=SEX]


mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)


