library(datasets)
data(iris)
?iris
head(iris)

mean = mean(iris[iris$Species=="virginica","Sepal.Length"])
mean

apply(iris[iris$Sepal.Length],2,mean)

l
data(mtcars)
head(mtcars)

tapply(mtcars$mpg,mtcars$cyl,mean)

meanhp <-sapply(split(mtcars$hp,mtcars$cyl),mean)
meanhp
round(abs(meanhp['8']-meanhp['4']))
      