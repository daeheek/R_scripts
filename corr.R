corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  files <-list.files(path =directory)

  cr <- c()
  for(n in 1:length(files)) {
    filename <-paste(directory, files[n], sep = "/")
    mydata <- read.csv(filename)  
    mydata <- mydata[complete.cases(mydata),]
    if ( nrow(mydata) > threshold ) {
      cr <- c(cr, cor(mydata$sulfate, mydata$nitrate) )
    }
  }
  return(cr)
}