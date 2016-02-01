corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## get names of files in directory
  ## directory is assumed to be in current working directory
  filenames <- list.files(paste(getwd(), "/", directory, sep=""))
  
  ## initialise empty vector
  correlations <- numeric()
  
  completeCases <- complete(directory)
  
  for(i in 1:nrow(completeCases)) {
    filename <- paste(getwd(), "/", directory, "/", filenames[i], sep="")
    fileData <- read.csv(filename)
    
    if(completeCases[i,"nobs"] > threshold) {
      correlation <- cor(fileData["sulfate"], fileData["nitrate"], use="complete.obs")
      correlations <- rbind(correlations, correlation)
    }
  
  }
  
  correlations
}
