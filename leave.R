



##loading data into package
library(readxl)
Data <- read_excel("C:/Users/Heather Handy/Desktop/leaveprob/PrevforPackage1.xlsx")
library(devtools)
use_data(Data, internal = TRUE, overwrite = TRUE)

##function for the probability of resigning
leaveprob <- function(disciplined, strategic, tolerant, persuasive, reserved, composed, high_stress) {

  ####GLM model used to create the probability value
  model <- glm(resigned ~ disciplined + strategic + tolerant + persuasive + reserved + composed + high_stress, data = Data, family=binomial)


  newdata = data.frame(disciplined, strategic, tolerant, persuasive, reserved, composed, high_stress)
  likelihood_resign <- predict(model, newdata, type="response")
  return (likelihood_resign)
}

