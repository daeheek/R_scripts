rankall <- function(outcome, num = "best") {
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
  ustates <- ustates[order(ustates)]
  ##sdata<-split(cdata, cdata$State)
  m <- data.frame(hospital= character(), state=character())
  q <- data.frame(hospital= character(), state=character())
 fin <- lapply( ustates, FUN = function(x = ustates,y = outcome,z =num, dec1 =dec){
  
  temp <- subset(cdata, cdata$State == x)
  
  if (y == "heart attack") {
    test <- temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),temp$Hospital.Name, na.last=TRUE, decreasing = dec1),]
    hospital <- test[z,]$Hospital.Name
    m <- data.frame(hospital= hospital, state = x)
    ##m <- rbind(m,cbind(hospital , state =x))
  } 
  else if (y == "heart failure") {
    test <- temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),temp$Hospital.Name, na.last=TRUE, decreasing = dec1),]
    hospital <- test[z,]$Hospital.Name
    m <- data.frame(hospital= hospital, state = x)
    ##m <- rbind(m,r)
  } 
  else if (y == "pneumonia") {
    test <- temp[order(suppressWarnings(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),temp$Hospital.Name, na.last=TRUE, decreasing = dec1),]
    hospital <- test[z,]$Hospital.Name
    m <- data.frame(hospital= hospital, state = x)
    ##m <- rbind(m,r)
  } 
  
  return(m)
  }
  
  
)

cnt <- length(fin)
ct <-1
while (ct < cnt+1) {
  q<- rbind(q,data.frame(hospital=fin[[ct]]$hospital,state=fin[[ct]]$state))
  ct <- ct+1
}
return(q)
}
