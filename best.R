best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  temp <- NULL
  cdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  dis <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,dis)) { stop("invalid outcome")}
  
  ustates <- c(unique(cdata$State))
  if (!is.element(state,ustates)) { stop("invalid state")}
  
  temp <- subset(cdata, cdata$State == state)
  if (outcome == "heart attack") {
    head(temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),temp$Hospital.Name, na.last=TRUE),],1)$Hospital.Name
  } 
  else if (outcome == "heart failure") {
    head(temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),temp$Hospital.Name, na.last=TRUE),],1)$Hospital.Name
  } 
  else if (outcome == "pneumonia") {
    head(temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),temp$Hospital.Name, na.last=TRUE),],1)$Hospital.Name
  } 
}