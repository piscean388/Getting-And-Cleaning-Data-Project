pollutantmean <- function(directory, pollutant, id = 1:332) {

  ## get names of files in directory
  ## directory is assumed to be in current working directory
  filenames <- list.files(paste(getwd(), "/", directory, sep=""))
  
  ## initialise empty data frame
  pollutantData <- data.frame(date = as.Date(character()),
    sulfate=numeric(),
    nitrate=numeric()
  )
  
  for(i in id) {
    filename <- paste(getwd(), "/", directory, "/", filenames[i], sep="")
    fileData <- read.csv(filename)
    pollutantData <- rbind(pollutantData, fileData)
  }
  
  result <- colMeans(pollutantData[pollutant], na.rm=TRUE)
  
  result

}