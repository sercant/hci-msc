# Author
# Sercan Turkmen @sercant

library("devtools")
library("hcitools")

setwd(".")

analyse_file <- function(input, n_trial = 10) {
  file_contents <- 0

  # chech if the parameter is string or already read data.frame
  # if it's a string check if the file exists and read
  # fail if it's an unknow type
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

  # initialize aggregated variables
  We <- c()
  Ae <- c()
  MT <- c()
  ID <- c()
  TP <- c()
  Er <- c()

  # loop over the 4 test trials as in excel file
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

  # form the resulting data.frame
  result <- data.frame(
    "We" = We,
    "Ae" = Ae,
    "ID" = ID,
    "MT" = MT,
    "TP" = TP,
    "Er" = Er)

  return(result)
}

# formats the data path according to the parameters
get_file_name <- function(participant_no,
                          condition_no,
                          participant_no_width = 1,
                          condition_no_width = 2) {
  return(
    paste(
      "data/FittsTaskOne-P",
      formatC(participant_no, width = participant_no_width, flag = "0"), # assures 00, 01 ...
      "-C",
      formatC(condition_no, width = condition_no_width, flag = "0"), # assures 00, 01 ...
      "-B01.sd1",
      sep = ""))
}

# handy function to rbind two data.frames
# if the first parameter is returns second parameter
# if both variable is not null rbinds them together
rbind_or_create <- function(dframe_1, dframe_2) {
  result <- dframe_1

  if (is.null(dframe_1)) {
    if (!is.null(dframe_2)) result <- dframe_2
  }
  else if (!is.null(dframe_2)) {
    result <- rbind(dframe_1, dframe_2)
  }

  return(result)
}

# enter participant number here
participant_count <- 2

# initialize conditions
c1 <- NULL
c2 <- NULL

# loop over each participant
for (i in 1:participant_count) {
  # get analysed data with the first condition
  c1_analyzed <- analyse_file(get_file_name(i, 1))
  # get analysed data with the second condition
  c2_analyzed <- analyse_file(get_file_name(i, 2))

  # rbind them
  c1 <- rbind_or_create(c1, c1_analyzed)
  c2 <- rbind_or_create(c2, c2_analyzed)
}

# t-test for TP
test_TP <- t.test(c1$TP, c2$TP)

# t-test for ER
test_ER <- t.test(c1$Er / 100, c2$Er / 100)

print(test_TP)
print(test_ER)

