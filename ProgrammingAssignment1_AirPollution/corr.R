buildFilename <- function(dir, filename) {
  
  if (filename < 10) {
    return(paste(paste(dir, paste("/", paste0("00", filename), sep=""), sep=""), "csv", sep="."));
  } else if (filename < 100) {
    return(paste(paste(dir, paste("/", paste0("0", filename), sep=""), sep=""), "csv", sep="."));
  } else {
    return(paste(paste(dir, paste("/", paste0(filename), sep=""), sep=""), "csv", sep="."));
  }
  
}

corr <- function(directory, threshold = 0) {
  
  compl <- complete(directory, 1:332)
  id <- compl$id[compl$nobs>threshold]
  corrVector <- c()
  for (i in id) {
    data <- read.csv(buildFilename(directory, i), header = T)
    corrVector <- rbind(corrVector, cor(data$sulfate, data$nitrate, use="complete.obs"))
  }
  return(corrVector)
  
}