buildFilename <- function(dir, filename) {
  
  if (filename < 10) {
    return(paste(paste(dir, paste("/", paste0("00", filename), sep=""), sep=""), "csv", sep="."));
  } else if (filename < 100) {
    return(paste(paste(dir, paste("/", paste0("0", filename), sep=""), sep=""), "csv", sep="."));
  } else {
    return(paste(paste(dir, paste("/", paste0(filename), sep=""), sep=""), "csv", sep="."));
  }
  
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  data <- data.frame(row.names(c("Date","sulfate","nitrate","ID")))
  for (i in id) {
    data <- rbind(data, read.csv(buildFilename(directory, i), header = T))
  }
  return(mean(data[[pollutant]][complete.cases(data[[pollutant]])]))
  
}