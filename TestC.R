# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2016-04-10 19:41:41 x86_64-pc-linux-gnu 

# Rattle version 4.1.0 user 'root'

# This log file captures all Rattle interactions as R commands. 

# this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 

# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2016-04-10 19:43:56 x86_64-pc-linux-gnu 

# Load the data.

crs$dataset <- read.csv("file:///home/mohammed/10th Semester/DataScience/Assignment/sonar.all-data", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-04-10 19:43:57 x86_64-pc-linux-gnu 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 207 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 144 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 31 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 32 observations

# The following variable selections have been noted.

crs$input <- c("X0.0200", "X0.0371", "X0.0428", "X0.0207",
               "X0.0954", "X0.0986", "X0.1539", "X0.1601",
               "X0.3109", "X0.2111", "X0.1609", "X0.1582",
               "X0.2238", "X0.0645", "X0.0660", "X0.2273",
               "X0.3100", "X0.2999", "X0.5078", "X0.4797",
               "X0.5783", "X0.5071", "X0.4328", "X0.5550",
               "X0.6711", "X0.6415", "X0.7104", "X0.8080",
               "X0.6791", "X0.3857", "X0.1307", "X0.2604",
               "X0.5121", "X0.7547", "X0.8537", "X0.8507",
               "X0.6692", "X0.6097", "X0.4943", "X0.2744",
               "X0.0510", "X0.2834", "X0.2825", "X0.4256",
               "X0.2641", "X0.1386", "X0.1051", "X0.1343",
               "X0.0383", "X0.0324", "X0.0232", "X0.0027",
               "X0.0065", "X0.0159", "X0.0072", "X0.0167",
               "X0.0180", "X0.0084", "X0.0090", "X0.0032")

crs$numeric <- c("X0.0200", "X0.0371", "X0.0428", "X0.0207",
                 "X0.0954", "X0.0986", "X0.1539", "X0.1601",
                 "X0.3109", "X0.2111", "X0.1609", "X0.1582",
                 "X0.2238", "X0.0645", "X0.0660", "X0.2273",
                 "X0.3100", "X0.2999", "X0.5078", "X0.4797",
                 "X0.5783", "X0.5071", "X0.4328", "X0.5550",
                 "X0.6711", "X0.6415", "X0.7104", "X0.8080",
                 "X0.6791", "X0.3857", "X0.1307", "X0.2604",
                 "X0.5121", "X0.7547", "X0.8537", "X0.8507",
                 "X0.6692", "X0.6097", "X0.4943", "X0.2744",
                 "X0.0510", "X0.2834", "X0.2825", "X0.4256",
                 "X0.2641", "X0.1386", "X0.1051", "X0.1343",
                 "X0.0383", "X0.0324", "X0.0232", "X0.0027",
                 "X0.0065", "X0.0159", "X0.0072", "X0.0167",
                 "X0.0180", "X0.0084", "X0.0090", "X0.0032")

crs$categoric <- NULL

crs$target  <- "R"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-04-10 19:44:18 x86_64-pc-linux-gnu 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

#============================================================
# Rattle timestamp: 2016-04-10 19:45:52 x86_64-pc-linux-gnu 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation sonar.all-data using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2016-04-10 19:46:10 x86_64-pc-linux-gnu 

# Principal Components Analysis (on numerics only).

pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.

pc

# Summarise the importance of the components found.

summary(pc)

# Display a plot showing the relative importance of the components.

plot(pc, main="")
title(main="Principal Components Importance sonar.all-data",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.

biplot(pc, main="")
title(main="Principal Components sonar.all-data",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2016-04-10 19:46:27 x86_64-pc-linux-gnu 

# GGobi Data Exploration 

# The 'rggobi' package provides the 'rggobi' function.

library(rggobi, quietly=TRUE)

#============================================================
# Rattle timestamp: 2016-04-10 19:50:11 x86_64-pc-linux-gnu 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.
library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(R) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 0.08 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:50:46 x86_64-pc-linux-gnu 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(R ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.12 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:50:46 x86_64-pc-linux-gnu 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(R ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 1.29 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:50:58 x86_64-pc-linux-gnu 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
crs$rf <- randomForest::randomForest(R ~ .,
                                     data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=7,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 14.00 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:51:12 x86_64-pc-linux-gnu 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(R) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 0.07 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:51:12 x86_64-pc-linux-gnu 

# Regression model 

# Build a Regression model.

crs$glm <- glm(R ~ .,
               data=crs$dataset[crs$train, c(crs$input, crs$target)],
               family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(crs$glm$y, crs$glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 0.61 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:51:13 x86_64-pc-linux-gnu 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(R) ~ .,
                 data=crs$dataset[crs$sample,c(crs$input, crs$target)],
                 size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
            paste(crs$nnet$n, collapse="-"),
            length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
            paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
            names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
            sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.27 secs

#============================================================
# Rattle timestamp: 2016-04-10 19:51:51 x86_64-pc-linux-gnu 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset, type="class")

# Generate the confusion matrix showing counts.

table(crs$dataset$R, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset$R, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Ada Boost model.

# Obtain the response from the Ada Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset)

# Generate the confusion matrix showing counts.

table(crs$dataset$R, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset$R, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset)$R, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(crs$dataset)$R, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset)$R, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(crs$dataset)$R, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- as.vector(ifelse(predict(crs$glm, type="response", newdata=crs$dataset) > 0.5, "R", "M"))

# Generate the confusion matrix showing counts.

table(crs$dataset$R, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset$R, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset, type="class")

# Generate the confusion matrix showing counts.

table(crs$dataset$R, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset$R, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2016-04-10 19:52:19 x86_64-pc-linux-gnu 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rpart model on sonar.all-data.

crs$pr <- predict(crs$rpart, newdata=crs$dataset)[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset$R)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on sonar.all-data.

crs$pr <- predict(crs$ada, newdata=crs$dataset, type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset$R)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CCCC00FF", lty=2, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rf model on sonar.all-data.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset)$R)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CC00FF", lty=3, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on sonar.all-data.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset)$R)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CCCCFF", lty=4, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the glm model on sonar.all-data.

crs$pr <- predict(crs$glm, type="response", newdata=crs$dataset)

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset$R)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#0000CCFF", lty=5, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the nnet model on sonar.all-data.

crs$pr <- predict(crs$nnet, newdata=crs$dataset)

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset$R)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC00CCFF", lty=6, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","rf","ksvm","glm","nnet"), col=rainbow(6, 1, .8), lty=1:6, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  sonar.all-data",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2016-04-10 19:53:23 x86_64-pc-linux-gnu 

# Score a dataset. 

# Obtain probability scores for the Decision Tree model on sonar.all-data.

crs$pr <- predict(crs$rpart, newdata=crs$dataset, type="class")

# Obtain probability scores for the Ada Boost model on sonar.all-data.

crs$pr <- predict(crs$ada, newdata=crs$dataset)

# Obtain probability scores for the Random Forest model on sonar.all-data.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset))

# Obtain probability scores for the SVM model on sonar.all-data.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset))

# Obtain probability scores for the Linear model on sonar.all-data.

crs$pr <- as.vector(ifelse(predict(crs$glm, type="response", newdata=crs$dataset) > 0.5, "R", "M"))

# Obtain probability scores for the Neural Net model on sonar.all-data.

crs$pr <- predict(crs$nnet, newdata=crs$dataset, type="class")

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset, select=c("R"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="/home/mohammed/10th Semester/DataScience/Assignment/sonar-data_score_idents.csv", row.names=FALSE)