library(hcitools)

data = read.csv("data.csv")

BBC <- subset(data, website == "BBC")
CNN <- subset(data, website == "CNN")

BBC_questionnaire <- subset(BBC, select = c(Q1, Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
BBC_score <- devtools::install_github("vkostakos/hcitools")(BBC_questionnaire, name = "SUS")
BBC_score

CNN_questionnaire <- subset(CNN, select = c(Q1, Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
CNN_score <- questionnaire.analyse(CNN_questionnaire, name = "SUS")
CNN_score

BBC_male_questionnaire <- subset(BBC, gender == "male", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
BBC_score_male <- questionnaire.analyse(BBC_male_questionnaire, name = "SUS")
BBC_score_male

BBC_female_questionnaire <- subset(BBC, gender == "female", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
BBC_score_female <- questionnaire.analyse(BBC_female_questionnaire, name = "SUS")
BBC_score_female

CNN_male_questionnaire <- subset(CNN, gender == "male", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
CNN_score_male <- questionnaire.analyse(CNN_male_questionnaire, name = "SUS")
CNN_score_male

CNN_female_questionnaire <- subset(CNN, gender == "female", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
CNN_score_female <- questionnaire.analyse(CNN_female_questionnaire, name = "SUS")
CNN_score_female

BBC_finnish_questionnaire <- subset(BBC, finnish == "yes", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
BBC_score_finnish <- questionnaire.analyse(BBC_finnish_questionnaire, name = "SUS")
BBC_score_finnish

BBC_int_questionnaire <- subset(BBC, finnish == "no", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
BBC_score_int <- questionnaire.analyse(BBC_int_questionnaire, name = "SUS")
BBC_score_int

CNN_finnish_questionnaire <- subset(CNN, finnish == "yes", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
CNN_score_finnish <- questionnaire.analyse(CNN_finnish_questionnaire, name = "SUS")
CNN_score_finnish

CNN_int_questionnaire <- subset(CNN, finnish == "no", select = c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10))
CNN_score_int <- questionnaire.analyse(CNN_int_questionnaire, name = "SUS")
CNN_score_int
