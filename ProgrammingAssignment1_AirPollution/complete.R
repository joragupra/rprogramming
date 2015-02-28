buildFilename <- function(dir, filename) {
  
  if (filename < 10) {
    return(paste(paste(dir, paste("/", paste0("00", filename), sep=""), sep=""), "csv", sep="."));
  } else if (filename < 100) {
    return(paste(paste(dir, paste("/", paste0("0", filename), sep=""), sep=""), "csv", sep="."));
  } else {
    return(paste(paste(dir, paste("/", paste0(filename), sep=""), sep=""), "csv", sep="."));
  }
  
}

complete <- function(directory, id = 1:332) {
  
  compl <- c()
  for (i in id) {
    data <- read.csv(buildFilename(directory, i), header = T)
    current <- c(i, dim(subset(data, complete.cases(data)))[[1]])
    compl <- rbind(compl, current)
  }
  return(data.frame(id = compl[,1], nobs = compl[,2]))
}