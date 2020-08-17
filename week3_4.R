#url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

#download.file(url1,'house.csv')

df<- read.csv('house.csv')
head(df)

names(df)

splited <- strsplit(names(df),'wgtp')
splited[123]

gdp<- read.csv('GDP.csv',skip = 4,nrows = 190)
gdp <-gdp[c(1,2,4,5)]
colnames(gdp) <- c('CountryCode' ,'Ranking','Economy','Total')
head(gdp)

gdp$Total <- as.numeric(gsub(',','',gdp$Total))
head(gdp)
mean(gdp$Total)


countryNames <- gdp$Economy
grep("^United",countryNames)

edu<-read.csv('edu.csv')
colnames(edu)

merged <- merge(gdp,edu,by='CountryCode')
colnames(merged)
merged$Special.Notes
length(grep("Fiscal year end: June 30",merged$Special.Notes))

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

DT <- data.table::data.table(timeCol = sampleTimes)

DT[(timeCol >= "2012-01-01") & (timeCol) < "2013-01-01", .N ]

DT[((timeCol >= "2012-01-01") & (timeCol < "2013-01-01")) & (weekdays(timeCol) == "Monday"), .N ]
