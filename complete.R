
complete <- function(directory, id = 1:332) {
  files <-list.files(path =directory)
  mydata = NULL
  s <- vector()
  if (length(id) >1) {
    for(n in seq(id[])) {
      filename <-paste(directory, files[id[n]], sep = "/")
      mydata = read.csv(filename)  
      s[n] <- sum(complete.cases(mydata))
      comps <- cbind(id=id[n],nobs=s[n])
      d <- rbind(d,comps) 
      }
      
  } else
  {
    filename <-paste(directory, files[id], sep = "/")
    mydata = read.csv(filename) 
    s <- sum(complete.cases(mydata))
    comps <- cbind(id=id,nobs=s)
    d <- rbind(d,comps) 
  }
  return(d)
}
