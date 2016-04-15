# Libraries importing
# --------------------

library(ggplot2)
library(dplyr)
library(knitr)
library(stargazer)
library(MASS)
library(pracma)
library(tidyr)
library(lubridate)
library(XML)
library(pastecs)
library(corrplot)
library(rpart)
library(RWeka)
library(grid)
library(partykit)
library(randomForest)
library(e1071)
library(kernlab)
library(compare)
library(caret)
library(klaR)

# ----------------------------------------------------------

# Adding seed value
# -----------------

constant.seed <- 42

# -----------------------------------------------------------

# Reading the hepatitis data
# --------------------------

hepatitis.data <- read.csv("/home/mohammed/10th Semester/DataScience/Assignment/hepatitis.data", header = F, na.strings = c('?', 'NA'))
names(hepatitis.data)
str(hepatitis.data)
summary(hepatitis.data)

names(hepatitis.data)[1] <- "target"
names(hepatitis.data)

# Prepare the factors
# -------------------

for (x in c(1,3:14,20)) {
  hepatitis.data[,x] <- as.factor(hepatitis.data[,x])
}




# -----------------------------------------------------------

# Reading the diabetes data
# --------------------------

spect.data <- read.csv("/home/mohammed/10th Semester/DataScience/Assignment/SPECT.train", header = F, na.strings = c('?', 'NA'))
names(spect.data)
str(spect.data)
summary(spect.data)

# Prepare the factors
# -------------------

names(spect.data)[1] <- "target"
names(spect.data)

for (x in c(1:23)) {
  spect.data[,x] <- as.factor(spect.data[,x])
}

str(spect.data)


# -----------------------------------------------------------

# Reading the diabetes data
# --------------------------

diabetes.data <- read.csv("/home/mohammed/10th Semester/DataScience/Assignment/pima-indians-diabetes.data", header = F, na.strings = c('?', 'NA'))
names(diabetes.data)
str(diabetes.data)
summary(diabetes.data)

# Prepare the factors
# -------------------

names(hepatitis.data)[9] <- "target"
names(hepatitis.data)

diabetes.data[,9] <- as.factor(diabetes.data[,9])


