
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <-list.files(path =directory)
  mydata = NULL
  if (length(id) >1) {
  for(n in seq(id[])) {
    filename <-paste(directory, files[id[n]], sep = "/")
    mydata = rbind(mydata,read.csv(filename))   }
} else
{
  filename <-paste(directory, files[id], sep = "/")
  mydata = rbind(mydata,read.csv(filename))   
}
  mean(mydata[,pollutant] , na.rm=TRUE)

}
