pollutantmean <- function(directory,pollutant,id = 1:332){

              filesD<-list.files(directory,full.names = TRUE)
              df<-data.frame()
              
              for( i in id){
                read<-read.csv(filesD[i],header=TRUE)
                df<-rbind(df,read)
              
              }
              
              m<-mean(df[,pollutant],na.rm =TRUE)
             
  
  
}

mean = pollutantmean("specdata","nitrate",1:10)
mean


complete<- function(directory,id = 1:322){
  
  filesD<-list.files(directory,full.names = TRUE)
  df<-data.frame()
 
  for( i in id){
    read<-read.csv(filesD[i],header=TRUE)
    read<-na.omit(read)
    nobs<-nrow(read)
    df<-rbind(df,data.frame(i,nobs))
    
  }
  colnames(df)<-c("id","nobs")
  return (df)
  
}

complete("specdata",1:10)


corr <-function(directory,threshold=0)
  {
  
  filesd = list.files(directory,full.names=TRUE)
  dat <- vector(mode = 'numeric',length = 0)
 
   for (i in 1:length(filesd)){
   
      read<-read.csv(filesd[i],header=TRUE)
    read=na.omit(read)
   
     if(nrow(read)>=threshold){
      dat<-c(dat,cor(read$nitrate,read$sulfate))
    }
    
   }
  return(dat)
}

dat=corr("specdata",10)
dat

#quiz
ans=pollutantmean("specdata", "sulfate", 1:10)
ans
ans =pollutantmean("specdata", "nitrate", 70:72)
ans
ans=pollutantmean("specdata", "sulfate", 34)
ans
ans=pollutantmean("specdata", "nitrate")
ans
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
