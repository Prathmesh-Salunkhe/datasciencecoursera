
NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

#Plot 1
total <- aggregate(Emissions~year,NEI,sum)
png('plot4_1.png')
plot(total$year,total$Emissions,type='o',xlab='Year',ylab = 'Emmission',main= 'Total PM25 emmission')
dev.off()

#Plot 2
balt = subset(NEI,NEI$fips == "24510")
totalbalt = aggregate(Emissions~year,balt,sum)

png('plot4_2.png')
plot(totalbalt$year,totalbalt$Emissions,type='o',xlab='Year',ylab = 'Emmission',main= 'Total Baltimore City PM25 emmission')
dev.off()

#Plot 3
library(ggplot2)
baltimoreType <- aggregate(Emissions ~ year + type, balt, sum)
png('plot4_3.png')
ggplot(baltimoreType, aes(year, Emissions, col = type)) +
  geom_line() +
  geom_point()+
  ggtitle('Baltimore emission by type')+
  xlab('Baltimore Emission')+
  ylab('Year')+
  scale_colour_discrete(name = "Type of sources") 
dev.off()

#Plot4
SCCcoal <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),]
NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC,]
totalCoal <- aggregate(Emissions ~ year + type, NEIcoal, sum)

png('plot4_4.png')
ggplot(totalCoal, aes(year, Emissions, col = type)) +
  geom_line() +
  geom_point() +
  ggtitle(expression("Total US" ~ PM[2.5] ~ "Coal Emission by Type and Year")) +
  xlab("Year") +
  ylab(expression("US " ~ PM[2.5] ~ "Coal Emission")) +
  scale_colour_discrete(name = "Type of sources") +
  theme(legend.title = element_text(face = "bold"))
dev.off()


#Plot5
baltimoreMotor <- subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")
baltimoreMotorAGG <- aggregate(Emissions ~ year, baltimoreMotor, sum)
png('plot4_5.png')
ggplot(baltimoreMotorAGG, aes(year, Emissions)) +
  geom_line(col = "steelblue3") +
  geom_point(col = "steelblue3") +
  ggtitle(expression("Baltimore " ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) +
  xlab("Year") +
  ylab(expression(~PM[2.5]~ "Motor Vehicle Emissions"))
dev.off()

#Plot6
baltLosAngelesMotors <- subset(NEI, NEI$fips %in% c("24510","06037") & NEI$type == "ON-ROAD")
baltLosAngelesMotorsAGG <- aggregate(Emissions ~ year + fips, baltLosAngelesMotors, sum)
png('plot4_6.png')
ggplot(baltLosAngelesMotorsAGG, aes(year, Emissions, col = fips)) +
  geom_line() +
  geom_point() +
  ggtitle(expression("Baltimore and Los Angeles" ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) +
  labs(x = "Year", y = expression(~PM[2.5]~ "Motor Vehicle Emissions") ) +
  scale_colour_discrete(name = "City", labels = c("Los Angeles", "Baltimore")) +
  theme(legend.title = element_text(face = "bold"))
dev.off()
