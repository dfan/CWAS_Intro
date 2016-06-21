library(RMySQL)
library(truncnorm)

con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "PheWAS", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
df <- dbReadTable(conn = con, name = "data")

# Introduces some dependence into comorbidities by dividing into subpopulation of diabetics
# that's "sicker" and one that's "less sick". Here all of cohort is diabetic. Function called only on diabetics
simulateComorbid <- function(n) {
  # one-coin flip for each comorbidity, rows of data frame are people
  # vectorized to remove for-loop
  return(sapply(runif(n), function(x) rbinom(n = 5, size = 1,c(0.77, 0.3, 0.2, 0.3, 0.4) * (x > 0.5) + c(0.57, 0.1, 0, 0.1, 0.2) * (x < 0.5))))
}

# hypertension(401), retinopathy(362.0), coronary heart disease(411), myocardial infarction(410), congestive heart failure(428)
# vectorized for speed
simulateDiabeticCohort <- function(n) {
  # transpose to n x nrow(df)
  mat <- as.data.frame(t(replicate(n, rbinom(n = nrow(df), size = 1, rtruncnorm(1, a = 0, b = 1, mean = 0.1, sd = 0.01)))))
  names(mat) <- paste("i", df$icd9, sep = "")
  # everyone in the case group is a diabetic.
  mat$i250 <- 1
  comorbidities <- simulateComorbid(n)
  mat$i401 <- comorbidities[1,]
  mat$i362.0 <- comorbidities[2,]
  mat$i411 <- comorbidities[3,]
  mat$i410 <- comorbidities[4,]
  mat$i428 <- comorbidities[5,] 
  return(mat)
}


diabICD9s <- simulateDiabeticCohort(1000)
# apply mean() to columns (denoted by 2) to double-check that probabilities add up correctly
apply(diabICD9s[ , c("i401", "i362.0", "i411", "i410", "i428")], 2, mean)


# Sample from normal distribution truncated to range 0 - 1, use as probabilty in a loaded coin flip
simulateControlCohort <- function(n) {
  mat <- as.data.frame(t(replicate(n, rbinom(n = nrow(df), size = 1, rtruncnorm(1, a = 0, b = 1, mean = 0, sd = 0.0001)))))
  names(mat) <- paste("i", df$icd9, sep = "")
  # all are non-diabetic
  mat$i250 <- 0
  return(mat)
}

normICD9s <- simulateControlCohort(1000)
# ,2 means apply to columns
apply(normICD9s[ , c("i401", "i362.0", "i411", "i410", "i428")], 2, mean)


# Takes two data frames as parameters: rows are individuals and columns are ICD9 codes
# performs unadjusted logistic regression and returns odds ratio
oddsRatio <- function(case, control) {
  # convert to factor because data is categorical and discrete
  combo <- lapply(rbind(case, control), as.factor)
  #case <- lapply(case[, c("i401", "i362.0", "i411", "i410", "i428")], as.factor)
  #control <- lapply(control[, c("i401", "i362.0", "i411", "i410", "i428")], as.factor)
  #combo <- as.data.frame(lapply(rbind(case, control), as.factor))
  logReg <- glm(combo$i250 ~ combo$i401 + combo$i362.0 + combo$i411 + combo$i410 + combo$i428, data = combo, family = "binomial")
  # ~. or !names(combo) == "i250"
  #caseLR <- glm(case$i250 ~ case$i401 + case$i362.0 + case$i411 + case$i410 + case$i428, data = case, family = "binomial")
  #controlLR <- glm(control$i250 ~ control$i401 + control$i362.0 + control$i411 + control$i410 + control$i428, data = control, family = "binomial")
  
  # reverse the log to get odds-ratio
  #exp(coef(caseLR))
  #exp(coef(controlLR))
  exp(coef(logReg))
  return(logReg)
}

# see average number of conditions each person has
sum(as.matrix(simulateDiabeticCohort(1000))) / 1000
sum(as.matrix(simulateControlCohort(1000))) / 1000


# If n < 50 ish there might not not be two categorical levels (either all 0s or all 1s but not both)
# Output for n = 50: 4.893392e+09  6.750515e+08  7.247342e+08  1.139874e+09  1.697405e+09 
oddsRatio(simulateDiabeticCohort(1000), simulateControlCohort(1000))

# Always disconnect at the end
dbDisconnect(con)
