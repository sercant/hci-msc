# Author
# Sercan Turkmen @sercant

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

  We <- c()
  Ae <- c()
  MT <- c()
  ID <- c()
  TP <- c()
  Er <- c()

  for (i in 1:4) {
    index <- (i - 1) * n_trial + 1
    last_index <- index + n_trial - 1

    We[i] <- sd(file_contents[index:last_index, "dx"]) * 4.133
    Ae[i] <- mean(file_contents[index:last_index, "Ae"])
    ID[i] <- log2(Ae[i] / We[i] + 1)
    MT[i] <- mean(file_contents[index:last_index, "MT.ms."])
    TP[i] <- ID[i] / MT[i] * 1000
    Er[i] <- sum(file_contents[index:last_index, "Errors"]) / n_trial * 100
  }

  result <- data.frame(
    "We" = We,
    "Ae" = Ae,
    "ID" = ID,
    "MT" = MT,
    "TP" = TP,
    "Er" = Er)

  return(result)
}

merge_or_create <- function(dframe_1, dframe_2) {
  result <- NULL

  if (is.null(dframe_1)) {
    if (!is.null(dframe_2)) result <- dframe_2
  }
  else {
    if (is.null(dframe_2)) result <- dframe_1
    else result <- rbind(dframe_1, dframe_2)
  }

  return(result)
}

participant_count <- 2

c1 <- NULL
c2 <- NULL

for (i in 1:participant_count) {
  c1_analyzed <- analyse_file(
    paste("data/FittsTaskOne-P", i, "-C01-B01.sd1", sep = ""))
  c2_analyzed <- analyse_file(
    paste("data/FittsTaskOne-P", i, "-C02-B01.sd1", sep = ""))

  c1 <- merge_or_create(c1, c1_analyzed)
  c2 <- merge_or_create(c2, c2_analyzed)
}

t.test(c1$TP, c2$TP)