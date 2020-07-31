outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

colnames(outcome)


func1<-function(){
  outcome[, 11] <- as.numeric(outcome[, 11])
  hist(outcome[, 11])
  
}

func1()

best <- function(state, outcome) {
  
  df <- read.csv("outcome-of-care-measures.csv",colClasses = "character",header = TRUE)
  
  if(!state %in% df$State){
    stop('invalid state')
  }
  
  if(outcome == 'heart attack'){
    data<- as.data.frame(cbind(df[,2],df[,7],df[,11],stringsAsFactors = FALSE))
  }
  else if(outcome == 'heart failure'){
    data<- as.data.frame(cbind(df[,2],df[,7],df[,17],stringsAsFactors = FALSE))
  }
  else if(outcome == 'pneumonia'){
    data<- as.data.frame(cbind(df[,2],df[,7],df[,23],stringsAsFactors = FALSE))
  } 
  else{
    stop("invalid outcome")
  }
  
  colnames(data) <- c("hospital", "state", "outcome")
  
  
  data <- data[(data[, "state"] == state), ]
  
  data[, "outcome"] <- as.numeric(data[, "outcome"])
  
  data <- data[!is.na(data[, "outcome"]), ]
  
  data <- data[order(data[, "outcome"]), ]
  
  Names <- data[data[, "outcome"] == min(data[,"outcome"]),1]
  
  sort(Names)[1]
  
}


rankhospital <- function(state, outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack
                               outcomes[, 17],  # heart failure
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numberic, gets a warning
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## convert num argument to valid rank
  
  if(num == "best") {
    num <- 1 
  }
  
  if (num == "worst") {
    num <- nrow(hRates) 
  }
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
  
  ## Get names of hospital 
  
  hRates[num,1] 
}

rankhospital("MD", "heart failure", 1 )

rankall <- function(outcome, num = "best") {
  data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
  
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  unique.states <- sort(unique(data$State))
  # print(unique.states)
  
  result.df <- list()
  
  for(state in unique.states) {
    data.state <- data[data$State == state, ]
    data.state[, i] <- as.numeric(x=data.state[, i])
    data.state <- data.state[complete.cases(data.state), ]
    
    # print(num)
    
    if(num == "best") {
      numrank = 1
    }
    else if(num == "worst") {
      numrank = nrow(data.state)
      # if(state == 'WI') {
      #   print(num)
      #   print('WI num')
      # }
    }
    else if(is.numeric(x=num)) {
      # print(num)
      if(num < 1 || num > nrow(data.state)) {
        result.df <- rbind(result.df, list(NA, state))
        print(state)
        next
      }
      else numrank <- num
      # print(num)
    }
    else {
      stop('invalid num')
    }
    
    # print(num)
    data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
    
    # if(state == 'WI') {
    #  print(data.state[, c(2,i)])
    #  print(numrank)
    #  print(nrow(data.state))
    # }
    
    return.names <- data.state[numrank, ]$Hospital.Name
    
    # print(return.names[1])
    
    result.df <- rbind(result.df, list(return.names[1], state))
    # print(result.df)
  }
  
  result.df <- as.data.frame(x=result.df)
  colnames(x=result.df) <- c('hospital', 'state')
  
  result.df
}

rankall("heart attack")


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
