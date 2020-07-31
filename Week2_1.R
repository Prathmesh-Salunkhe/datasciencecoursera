df=read.csv("hw1_data.csv")
head(df)
nrow(df)
tail(df)
tail(df,2)
df[47,"Ozone"]

sum(is.na(df$Ozone))

subset = df[df$Ozone > 31 & df$Temp >90,]
subset
nrow(subset)
mean(df$Solar.R , na.rm = TRUE)

mean(df$Ozone ,na.rm = TRUE )

mean(df[df$Month == 6,"Temp"])

may <-df[df$Month == 5 , "Ozone"]
class(may)
max(may,na.rm = TRUE)


x <- list(2, "a", "b", TRUE)
x[[2]]
class(x[[2]])
