rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  dec <- FALSE
  if (num != "worst") {
    if (num == "best") {
      num <-1
    } else
    {
      num <- num

    }
  } else {
    num <-1
    dec <- TRUE
  }
    
  
  temp <- NULL
  cdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  dis <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,dis)) { stop("invalid outcome")}
  
  ustates <- c(unique(cdata$State))
  if (!is.element(state,ustates)) { stop("invalid state")}
  
  temp <- subset(cdata, cdata$State == state)
  
  
  if (outcome == "heart attack") {
    test <- temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),temp$Hospital.Name, na.last=TRUE, decreasing = dec),]
    test[num,]$Hospital.Name
    } 
  else if (outcome == "heart failure") {
    test <- temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),temp$Hospital.Name, na.last=TRUE, decreasing = dec),]
    test[num,]$Hospital.Name  
    } 
  else if (outcome == "pneumonia") {
    test <- temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),temp$Hospital.Name, na.last=TRUE, decreasing = dec),]
    test[num,]$Hospital.Name  
  } 
}