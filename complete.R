complete <- function(directory, id = 1:332) {
  
  ## get names of files in directory
  ## directory is assumed to be in current working directory
  filenames <- list.files(paste(getwd(), "/", directory, sep=""))
  
  ## initialise empty data frame
  completeCases <- data.frame(id = integer(),
                              nobs = integer()
  )
  
  for(i in id) {
    filename <- paste(getwd(), "/", directory, "/", filenames[i], sep="")
    fileData <- read.csv(filename)
    completeCasesData <- complete.cases(fileData)
    completeCases <- rbind(completeCases,data.frame(id=i, nobs=length(completeCasesData[completeCasesData])))
    
  }
  
  completeCases
  
}