analyse_file <- function(input) {
  file_contents <- 0
  
  if (is.character(input)) {
    if (!file.exists(input)) {
      stop("File doesn't exist :(");
    }
    file_contents <- read.csv(input)
  }
  else if (is.data.frame(input)) {
    file_contents <- input
  }
  else {
    stop("Unknown input type")
  }
  
  We = c()
  Ae = c()
  MT = c()
  ID = c()
  TP = c()
  
  for (i in 1:4) {
    index <- (i - 1) * 10;
    We[i] <- sd(file_contents[index:(index + 10), 'dx']) * 4.133
    Ae[i] <- mean(file_contents[index:(index + 10), 'Ae'])
    ID[i] <- log2(Ae[i] / We[i] + 1)
    MT[i] <- mean(file_contents[index:(index + 10), 'MT.ms.'])
    TP[i] <- ID[i] / MT[i] * 1000
  }
  
  result <- data.frame('We' = We, 'Ae' = Ae, 'ID' = ID, 'MT' = MT, 'TP' = TP)
  
  return(result)
}

result <- analyse_file("data/FittsTaskOne-P1-C01-B01.sd1")