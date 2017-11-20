analyse_file <- function(input, n_trial = 10) {
  file_contents <- 0
  
  if (is.character(input)) {
    if (!file.exists(input)) {
      stop("File doesn't exist :(")
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
  Er = c()
  
  for (i in 1:4) {
    index <- (i - 1) * n_trial + 1
    last_index <- index + n_trial - 1
    
    We[i] <- sd(file_contents[index:last_index, 'dx']) * 4.133
    Ae[i] <- mean(file_contents[index:last_index, 'Ae'])
    ID[i] <- log2(Ae[i] / We[i] + 1)
    MT[i] <- mean(file_contents[index:last_index, 'MT.ms.'])
    TP[i] <- ID[i] / MT[i] * 1000
    Er[i] <- sum(file_contents[index:last_index, 'Errors']) / n_trial * 100
  }
  
  result <- data.frame('We' = We, 'Ae' = Ae, 'ID' = ID, 'MT' = MT, 'TP' = TP, 'Er' = Er)
  
  return(result)
}

c1 <- analyse_file("data/FittsTaskOne-P1-C01-B01.sd1")
c2 <- analyse_file("data/FittsTaskOne-P1-C02-B01.sd1")

t.test(c1$ID, c2$ID)