# The purpose of the project: 
# to build a classification model using R to predict customer churn
# (probability and classification) for a Telco company.

# Install packages
install.packages(c("forecast",
                   "ggplot2",
                   "gplots",
                   "reshape",
                   "GGally",
                   "MASS",
                   "reshape",
                   "gains",
                   "caret",
                   "e1071",
                   "rpart",
                   "rpart.plot",
                   "pROC"))


# Library packages
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
library(reshape)
library(gains)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(pROC)
# set working directory

# Obtain the dataset to be used in the analysis
tcc.df <- read.csv("Telco-Customer-Churn.csv", header = TRUE)
# 1 Data exploration ====
# |--- 1.1 Explore the general status ====
dim(tcc.df) # 7043 obs. of 21 variables
# Exploring and understanding the dataset and meaning of variables
summary(tcc.df)
str(tcc.df)
# Exploring the column names
t(t(names(tcc.df)))
# |---1.2 Remove the rows with missing values ====
tcc.df <- na.omit(tcc.df) # 11/7043 NAs from new customers, use deletion, not interpolation.
# |---1.3 Remove "customerID" variable since it is unrelated ====
tcc.df <- tcc.df[,-1]
# |---1.4 Handle categorical variables ====
tcc.df[,2] <- factor(tcc.df$SeniorCitizen)
categorical <- c() # to collect the column ordinals of categorical variables
for(n in 1:length(tcc.df)){
  ifelse(class(tcc.df[,n]) == "character", categorical <- c(categorical,n), tcc.df[,n])
}
tcc.df[,categorical] <- lapply(X = tcc.df[,categorical], FUN = function(x){factor(x)})

dim(tcc.df) #7032 obs. of 20 variables
str(tcc.df)
# |---1.5 Explore individual variables====
#check the transfer, and further understanding variables
summary(tcc.df)
# Explore the relationship between variables VS "Churn"
# function 1 for pivot tables and bar chart
xvy <- function(iv){
  # generate the Pivot tables of a categorical IV VS. DV
  xcol <- tcc.df[,which(colnames(tcc.df) == iv)]
  ycol <- tcc.df$Churn
  explore.df <- data.frame(iv = xcol, Churn = ycol)
  plt <-ggplot(explore.df, aes(x = xcol, fill=ycol)) +
    geom_bar(position="fill") + 
    labs(title=paste(iv,'VS. Churn'),y='proportion') +
    theme(plot.title = element_text(hjust = 0.5)) 
  print(plt)
  mlt <- melt(explore.df, id = 'iv', measure.vars = 'Churn')
  head(mlt)
  pvtable <- cast(mlt, iv ~ value, length, margins = c("grand_row", "grand_col"))
  print(pvtable)
  pvtable$No <- pvtable$No/(pvtable$`(all)`)*100
  pvtable$Yes <- pvtable$Yes/(pvtable$`(all)`)*100
  pvtable$`(all)` <- pvtable$`(all)`/(pvtable$`(all)`)*100
  print(pvtable)
}
# function 2 for the test of independence
# (use significance level a=0.05)
test.of.independence <- function(iv){ 
  tbl <- table(tcc.df[c("Churn", iv)])
  n <- margin.table(tbl)
  p.i <- margin.table(tbl, 1) / n
  p.j <- margin.table(tbl, 2) / n
  tbl.e <- p.i %o% p.j * n
  tbl.test <- (tbl - tbl.e) ^ 2 / tbl.e
  chisq <- margin.table(tbl.test)
  df <- (dim(tbl)[1] - 1) * (dim(tbl)[2] - 1)
  alpha <- 0.05
  # (p-value approach:)
  print(paste("Null Hypothesis (H0): customer churn is independent of", iv))
  print(paste("Alternative Hypothesis (Ha): customer churn is not independent of",iv))
  p.value <- pchisq(chisq, df = df, lower.tail = FALSE)
  print(paste("p.value =", p.value, "and alpha =", alpha))
  print(paste("p.value <= alpha is ", p.value <= alpha, ".", sep = ""))
  print(paste("H0 is", ifelse(p.value <= alpha, "", " not"), " rejected.", sep = ""))
  # (Critical value approach:)
  print(paste("Null Hypothesis (H0): customer churn is independent of", iv))
  print(paste("Alternative Hypothesis (Ha): customer churn is not independent of",iv))
  chisq.alpha.rt <- qchisq(alpha, df = df, lower.tail = FALSE)
  print(paste("chisq =", chisq, "and chisq.alpha.rt =", chisq.alpha.rt))
  print(paste("Rejection region: [", chisq.alpha.rt, ", +inf)", sep = ""))
  print(paste("chisq >= chisq.alpha.rt is ", chisq >= chisq.alpha.rt, ".", sep = ""))
  print(paste("H0 is ", ifelse(chisq >= chisq.alpha.rt, "", "not "), "rejected.", sep = ""))
}

# |---|---Variable 1 :"gender" ====
xvy("gender")
test.of.independence("gender")
# Summary: This variable may not have a significant effect on "Churn", so we'll delete it before building models.

# |---|---Variable 2 :"SeniorCitizen" ====
xvy("SeniorCitizen")
test.of.independence("SeniorCitizen")

# |---|---Variable 3 :"Partner" ====
xvy("Partner")
test.of.independence("Partner")

# |---|---Variable 4 :"Dependents" ====
xvy("Dependents")
test.of.independence("Dependents")
# Summary: It may have a significant effect on "Churn".

# |---|---Variable 5 :"tenure" ====
# Boxplot
ggplot(tcc.df) + geom_boxplot(aes(x = as.factor(Churn), y = tenure)) +
  xlab("Churn")
# Conditional Mean VS "Churn"
data.for.plot <- aggregate(tcc.df$tenure, by = list(tcc.df$Churn), FUN = mean)
names(data.for.plot) <- c("Churn", "MeanTenure")
ggplot(data.for.plot) + geom_bar(aes(x = Churn, y = MeanTenure), stat = "identity") +
  labs(title=paste("Conditional Mean of Tenure")) + theme(plot.title = element_text(hjust = 0.5)) 

# |---|---Variable 6 :"PhoneService" ====
xvy("PhoneService")
test.of.independence("PhoneService")
# Summary: This variable may not have a significant effect on "Churn", so we'll delete it before building models.

# |---|---Variable 7 :"MultipleLines" ====
xvy("MultipleLines")
test.of.independence("MultipleLines")
# There is a significant difference between "No"|"No phone service" and "Yes",
#     but there is no significant difference between "No" and "No phone service".
#     group "No" and "No phone service"
tcc.df[,7] <- factor(ifelse(tcc.df$MultipleLines == 'Yes', 'Yes', '"No"|"No phone service"'))

# |---|---Variable 8 :"InternetService" ====
xvy("InternetService")
test.of.independence("InternetService")

# |---|---Variable 9 :"OnlineSecurity" ====
# depend on "InternetService". Group "No" and "No internet service"
tcc.df[,9] <- factor(ifelse(tcc.df$OnlineSecurity == 'Yes', 'Yes', '"No"|"No internet service"'))
xvy("OnlineSecurity")
test.of.independence("OnlineSecurity")

# |---|---Variable 10 :"OnlineBackup" ====
# depend on "InternetService". Group "No" and "No internet service"
tcc.df[,10] <- factor(ifelse(tcc.df$OnlineBackup == 'Yes', 'Yes', '"No"|"No internet service"'))
xvy("OnlineBackup")
test.of.independence("OnlineBackup")

# |---|---Variable 11 :"DeviceProtection" ====
# depend on "InternetService". Group "No" and "No internet service"
tcc.df[,11] <- factor(ifelse(tcc.df$DeviceProtection == 'Yes', 'Yes', '"No"|"No internet service"'))
xvy("DeviceProtection")
test.of.independence("DeviceProtection")

# |---|---Variable 12 :"TechSupport" ====
# depend on "InternetService". Group "No" and "No internet service"
tcc.df[,12] <- factor(ifelse(tcc.df$TechSupport == 'Yes', 'Yes', '"No"|"No internet service"'))
xvy("TechSupport")
test.of.independence("TechSupport")

# |---|---Variable 13 :"StreamingTV" ====
# depend on "InternetService". Group "No" and "No internet service"
tcc.df[,13] <- factor(ifelse(tcc.df$StreamingTV == 'Yes', 'Yes', '"No"|"No internet service"'))
xvy("StreamingTV")
test.of.independence("StreamingTV")

# |---|---Variable 14 :"StreamingMovies" ====
# depend on "InternetService". Group "No" and "No internet service"
tcc.df[,14] <- factor(ifelse(tcc.df$StreamingMovies == 'Yes', 'Yes', '"No"|"No internet service"'))
xvy("StreamingMovies")
test.of.independence("StreamingMovies")

# |---|---Variable 15 :"Contract"====
xvy("Contract")
test.of.independence("Contract")

# |---|---Variable 16 :"PaperlessBilling" ====
xvy("PaperlessBilling")
test.of.independence("PaperlessBilling")

# |---|---Variable 17 :"PaymentMethod"  ====
xvy("PaymentMethod")
test.of.independence("PaymentMethod")
# There is a significant difference between "Electronic check" and other three.
# , but there is no significant difference between the other three.
# group "Bank transfer (automatic)", "Credit card (automatic)" and "Mailed check"
tcc.df[,17] <- factor(ifelse(tcc.df$PaymentMethod == 'Electronic check', 'Electronic check', 'other methods'))
# |---|---Variable 18 :"MonthlyCharges" ====
# Boxplot
ggplot(tcc.df) + geom_boxplot(aes(x = as.factor(Churn), y = MonthlyCharges)) +
  xlab("Churn")
# Conditional Mean VS "Churn"
data.for.plot <- aggregate(tcc.df$MonthlyCharges, by = list(tcc.df$Churn), FUN = mean)
names(data.for.plot) <- c("Churn", "MeanMonthlyCharges")
ggplot(data.for.plot) + geom_bar(aes(x = Churn, y = MeanMonthlyCharges), stat = "identity") +
  labs(title=paste("Conditional Mean of MonthlyCharges")) + theme(plot.title = element_text(hjust = 0.5)) 

# |---|---Variable 19 :"TotalCharges" ====
# Boxplot
ggplot(tcc.df) + geom_boxplot(aes(x = as.factor(Churn), y = TotalCharges)) +
  xlab("Churn")
# Conditional Mean VS "Churn"
data.for.plot <- aggregate(tcc.df$TotalCharges, by = list(tcc.df$Churn), FUN = mean)
names(data.for.plot) <- c("Churn", "MeanTotalCharges")
ggplot(data.for.plot) + geom_bar(aes(x = Churn, y = MeanTotalCharges), stat = "identity") +
  labs(title=paste("Conditional Mean of TotalCharges")) + theme(plot.title = element_text(hjust = 0.5)) 
# Check the correlation between Continuous Variables
cor(tcc.df$TotalCharges, tcc.df$MonthlyCharges)
cor(tcc.df$TotalCharges, tcc.df$tenure)
cor(tcc.df$tenure, tcc.df$MonthlyCharges)
# We will remove "TotalCharges", which is highly correlated with other two variables.

# |---|---Variable 20 :"Churn" ====
c <- data.frame(table(tcc.df$Churn))
c.no <- c[1,2]/(c[1,2]+c[2,2])*100
c.yes <- c[2,2]/(c[1,2]+c[2,2])*100
c.df <- data.frame("churn"=c("No", "Yes"), "percent"=c(c.no, c.yes))
ggplot(c.df, aes(churn, percent), fill = percent)+geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+xlab("Churn")+ylab("Percent")+ggtitle("Churn Percentage")

# 2 # Data processing ====
# Use Inter Quartile Range (IQR) method to identify outliers of all continuous variables.
# function for detecting outlier
detect.outlier <- function(iv){
  x <- tcc.df[,which(colnames(tcc.df) == iv)]
  iqrval <- IQR(x)
  lowerLimit <- (quantile(x, 0.25, type = 6, na.rm = TRUE)) - 1.5 * iqrval
  upperLimit <- (quantile(x, 0.75, type = 6, na.rm = TRUE)) + 1.5 * iqrval
  print(paste("IQR:", iqrval,",", "Lower limit:", lowerLimit,",", "Upper limit:", upperLimit))
  outlier.num <- length(x[x < lowerLimit | x > upperLimit])
  print(paste("There are", outlier.num, "ourliers in the variable-", iv))
}

# tenure
detect.outlier('tenure')
# MonthlyCharges
detect.outlier('MonthlyCharges')
# TotalCharges
detect.outlier('TotalCharges')
# No need for removing outliers.

# 3 Data and demention reduction ====
# not fit for this data
# change "tenure" and "MonthlyCharges" from continous to categorical variables
tcc.df$tenure <- factor(cut(tcc.df$tenure,breaks = c(0,12,24,48,60,max(tcc.df$tenure)),
                            labels = c('< 12M','12-24M','24-48M','48-60M','> 60M')))
tcc.df$MonthlyCharges <- factor(cut(tcc.df$MonthlyCharges,breaks = c(0,35,70,90,max(tcc.df$MonthlyCharges)),
                                    labels = c('< $35','$35-70','$70-90','> $90')))
# Delete unrelated variables('gender and 'PhoneService') and put DV("Churn) as the 1st column
t(t(names(tcc.df)))
tcc.df <- tcc.df[, names(tcc.df)[c(20,2:5,7:18)]]
t(t(names(tcc.df)))
summary(tcc.df)

# 4 Partition the data (10-fold cross validation) ====
data.partition <- function(dataframe.for.partition){
  rowindex <- 1:nrow(dataframe.for.partition)
  row.index.list <- list()
  set.seed(100)
  for(n in 10:1){
    sample.list <- list(sample(rowindex, length(rowindex)/n, replace = FALSE))
    row.index.list[(11-n)] <- sample.list
    rowindex <- setdiff(rowindex, unlist(sample.list))
    print(paste("number of left index :", length(rowindex), "number of index in this fold:", length(unlist(sample.list))))
  }
  return(row.index.list)
}
row.index.list <- data.partition(tcc.df)
# make sure row.index.list include all the row index in random index
all(sort(unlist(row.index.list)) == 1:nrow(tcc.df))


# 5 Model building ====
# |---- 5.1 Use Classification tree to find out important variables====
sub.df <- tcc.df
variable.importance.list <- list()
for(i in 1:10){  
  valid.df <- sub.df[row.index.list[[i]],]
  train.df <- sub.df[-row.index.list[[i]],]
  tcc.default.ct <- rpart(Churn ~ ., data = train.df, method = "class", control = rpart.control(xval = 10))
  variable.importance.list[i] <- list(names(tcc.default.ct$variable.importance))
}
variable.importance.list # find out the (best-pruned) variable importance is variable.importance.list[[7]]
table(unlist(variable.importance.list))
## Re-do Step 4, Use new dataframe with important variables.
tcc.df <- tcc.df[,c(1,which(names(tcc.df) %in% names(table(unlist(variable.importance.list)))))]
row.index.list <- data.partition(tcc.df)
# make sure row.index.list include all the row index in random index
all(sort(unlist(row.index.list)) == 1:nrow(tcc.df))

# |---- 5.2 Build Classification tree Model====
sub.df <- tcc.df
ct.valid.result.df<- data.frame()
ct.train.result.df<- data.frame()
for(i in 1:10){  
  valid.df <- sub.df[row.index.list[[i]],]
  train.df <- sub.df[-row.index.list[[i]],]
  tcc.default.ct <- rpart(Churn ~ ., data = train.df, method = "class", control = rpart.control(xval = 10))
  # Classification
  valid.pred.class <- predict(tcc.default.ct, valid.df[, -1], type = "class")
  train.pred.class <- predict(tcc.default.ct, train.df[, -1], type = "class")
  valid.pred.prob <- predict(tcc.default.ct, valid.df[, -1], type = "prob")
  train.pred.prob <- predict(tcc.default.ct, train.df[, -1], type = "prob")
  # set datasets for training result and validation result
  valid.sub.fold <- data.frame('prob' = valid.pred.prob, 'class' = valid.pred.class, 'actual' = valid.df[, 1])
  train.sub.fold <- data.frame('prob' = train.pred.prob, 'class' = train.pred.class, 'actual' = train.df[, 1])
  ct.valid.result.df <- rbind(ct.valid.result.df, valid.sub.fold)
  ct.train.result.df <- rbind(ct.train.result.df, train.sub.fold)
}

# Plot tree
prp(tcc.default.ct, type = 1, extra = 1, under = TRUE, split.font = 2,
    under.font = 1, nn.font = 3,varlen = -10,
    box.col = ifelse(tcc.default.ct$frame$var == "<leaf>", "gray", "white"))

# |---- 5.3 Build logistic regression model ====
sub.df <- tcc.df
logit.valid.result.df<- data.frame()
logit.train.result.df<- data.frame()
for(i in 1:10){  
  valid.df <- sub.df[row.index.list[[i]],]
  train.df <- sub.df[-row.index.list[[i]],]
  logit.reg <- glm(Churn ~ ., data = train.df, family = "binomial") 
  # Compute propensity
  valid.pred.prob <- predict(logit.reg, valid.df[, -1], type = "response")
  train.pred.prob <- predict(logit.reg, train.df[, -1], type = "response")
  # Classification
  valid.pred.class <- ifelse(valid.pred.prob >= 0.5, 'Yes', 'No')
  train.pred.class <- ifelse(train.pred.prob >= 0.5,'Yes', 'No')
  # set datasets for training result and validation result
  valid.sub.fold <- data.frame('prob' = valid.pred.prob, 'class' = valid.pred.class, 'actual' = valid.df[, 1])
  train.sub.fold <- data.frame('prob' = train.pred.prob, 'class' = train.pred.class, 'actual' = train.df[, 1])
  logit.valid.result.df <- rbind(logit.valid.result.df, valid.sub.fold)
  logit.train.result.df <- rbind(logit.train.result.df, train.sub.fold)
}

# |---- 5.4 Build Naive Bayes Model ====
sub.df <- tcc.df
nb.valid.result.df<- data.frame()
nb.train.result.df<- data.frame()
for(i in 1:10){ 
  valid.df <- sub.df[row.index.list[[i]],]
  train.df <- sub.df[-row.index.list[[i]],]
  tcc.nb <- naiveBayes(Churn ~ ., data = train.df)
  # Compute propensity
  valid.pred.prob <- predict(tcc.nb, valid.df[, -1], type = "raw")
  train.pred.prob <- predict(tcc.nb, train.df[, -1], type = "raw")
  # Classification
  valid.pred.class <- predict(tcc.nb, valid.df[, -1], type = "class")
  train.pred.class <- predict(tcc.nb, train.df[, -1], type = "class")
  # set datasets for training result and validation result
  valid.sub.fold <- data.frame('prob' = valid.pred.prob, 'class' = valid.pred.class, 'actual' = valid.df[, 1])
  train.sub.fold <- data.frame('prob' = train.pred.prob, 'class' = train.pred.class, 'actual' = train.df[, 1])
  nb.valid.result.df <- rbind(nb.valid.result.df, valid.sub.fold)
  nb.train.result.df <- rbind(nb.train.result.df, train.sub.fold)
}

# 6 Model evaluation ====
# |---- 6.1 Evaluation of Classification tree Model====
# accuracy, sensitivity, specificity, precision, FDR, FOR
ct.valid <- confusionMatrix(factor(ct.valid.result.df$class),
                            factor(ct.valid.result.df$actual))
ct.train <- confusionMatrix(factor(ct.train.result.df$class),
                            factor(ct.train.result.df$actual))
ct.confusion <-list('valid' = c(ct.valid$overall[1],ct.valid$byClass[1],
                                ct.valid$byClass[2], ct.valid$byClass[3], 1- ct.valid$byClass[3], 
                                1 - ct.valid$byClass[4]), 'train' = c(ct.train$overall[1],ct.train$byClass[1], ct.train$byClass[2], ct.train$byClass[3], 
                                                                      1- ct.train$byClass[3], 1 - ct.train$byClass[4]))
names(ct.confusion[[1]]) <- c('accuracy', 'sensitivity', 'specificity','precision', 'FDR', 'FOR')
names(ct.confusion[[2]]) <- c('accuracy', 'sensitivity', 'specificity','precision', 'FDR', 'FOR')
ct.confusion
# ROC chart, AUC of ROC curve
p <- ifelse(ct.valid.result.df$class == 'Yes', 1, 0); r <- ifelse(ct.valid.result.df$actual == 'Yes', 1, 0)
ct.valid.roc <- roc(predictor = p, response = r, levels = c(0,1), direction='<')
p <- ifelse(ct.train.result.df$class == 'Yes', 1, 0); r <- ifelse(ct.train.result.df$actual == 'Yes', 1, 0)
ct.train.roc <- roc(predictor = p, response = r, levels = c(0,1), direction='<')
plot.roc(ct.train.roc, col = 'blue', print.auc = TRUE)
plot.roc(ct.valid.roc, add = TRUE, col = 'red', print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)
# Lift chart
lift.train <- lift(relevel(ct.train.result.df$actual, ref = "Yes") ~ prob.Yes, data = ct.train.result.df)
lift.valid <- lift(relevel(ct.valid.result.df$actual, ref = "Yes") ~ prob.Yes, data = ct.valid.result.df)
xyplot(lift.train, plot = "gain", main = "train")
xyplot(lift.valid, plot = "gain", main = "valid", col = 'red')
par(mfcol = c(2, 2))
gain <- gains(ifelse(ct.train.result.df$actual == "Yes", 1, 0), ct.train.result.df$prob.Yes, groups = dim(ct.train.result.df)[1])
plot(c(0, gain$cume.pct.of.total * sum(ifelse(ct.train.result.df$actual == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l", main = "train")
lines(c(0, sum(ifelse(ct.train.result.df$actual == "Yes", 1, 0))) ~ c(0, dim(ct.train.result.df)[1]), col = "gray", lty = 2)
gain <- gains(ifelse(ct.train.result.df$actual == "Yes", 1, 0), ct.train.result.df$prob.Yes)
barplot(gain$mean.resp / mean(ifelse(ct.train.result.df$actual == "Yes", 1, 0)), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart of train")
gain <- gains(ifelse(ct.valid.result.df$actual == "Yes", 1, 0), ct.valid.result.df$prob.Yes, groups = dim(ct.valid.result.df)[1])
plot(c(0, gain$cume.pct.of.total * sum(ifelse(ct.valid.result.df$actual == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l", main = "valid")
lines(c(0, sum(ifelse(ct.valid.result.df$actual == "Yes", 1, 0))) ~ c(0, dim(ct.valid.result.df)[1]), col = "gray", lty = 2)
gain <- gains(ifelse(ct.valid.result.df$actual == "Yes", 1, 0), ct.valid.result.df$prob.Yes)
barplot(gain$mean.resp / mean(ifelse(ct.valid.result.df$actual == "Yes", 1, 0)), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart of valid")
par(mfcol = c(1, 1))

# |---- 6.2 Evaluation of Logistic Regression Model ====
# accuracy, sensitivity, specificity, precision, FDR, FOR
logit.valid <- confusionMatrix(factor(logit.valid.result.df$class),
                               factor(logit.valid.result.df$actual))
logit.train <- confusionMatrix(factor(logit.train.result.df$class),
                               factor(logit.train.result.df$actual))
logit.confusion <-list('valid' = c(logit.valid$overall[1],logit.valid$byClass[1],
                                   logit.valid$byClass[2], logit.valid$byClass[3], 1- logit.valid$byClass[3], 
                                   1 - logit.valid$byClass[4]), 'train' = c(logit.train$overall[1],logit.train$byClass[1], 
                                                                            logit.train$byClass[2], logit.train$byClass[3], 
                                                                            1- logit.train$byClass[3], 1 - logit.train$byClass[4]))
names(logit.confusion[[1]]) <- c('accuracy', 'sensitivity', 'specificity','precision', 'FDR', 'FOR')
names(logit.confusion[[2]]) <- c('accuracy', 'sensitivity', 'specificity','precision', 'FDR', 'FOR')
logit.confusion
# ROC chart, AUC of ROC curve
p <- ifelse(logit.valid.result.df$class == 'Yes', 1, 0); r <- ifelse(logit.valid.result.df$actual == 'Yes', 1, 0)
logit.valid.roc <- roc(predictor = p, response = r, levels = c(0,1), direction='<')
p <- ifelse(logit.train.result.df$class == 'Yes', 1, 0); r <- ifelse(logit.train.result.df$actual == 'Yes', 1, 0)
logit.train.roc <- roc(predictor = p, response = r, levels = c(0,1), direction='<')
plot.roc(logit.train.roc, col = 'blue', print.auc = TRUE)
plot.roc(logit.valid.roc, add = TRUE, col = 'red', print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)
# Lift chart
lift.train <- lift(relevel(logit.train.result.df$actual, ref = "Yes") ~ prob, data = logit.train.result.df)
lift.valid <- lift(relevel(logit.valid.result.df$actual, ref = "Yes") ~ prob, data = logit.valid.result.df)
xyplot(lift.train, plot = "gain", main = "train")
xyplot(lift.valid, plot = "gain", main = "valid", col = 'red')
par(mfcol = c(2, 2))
gain <- gains(ifelse(logit.train.result.df$actual == "Yes", 1, 0), logit.train.result.df$prob, groups = dim(logit.train.result.df)[1])
plot(c(0, gain$cume.plogit.of.total * sum(ifelse(logit.train.result.df$actual == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l", main = "train")
lines(c(0, sum(ifelse(logit.train.result.df$actual == "Yes", 1, 0))) ~ c(0, dim(logit.train.result.df)[1]), col = "gray", lty = 2)
gain <- gains(ifelse(logit.train.result.df$actual == "Yes", 1, 0), logit.train.result.df$prob)
barplot(gain$mean.resp / mean(ifelse(logit.train.result.df$actual == "Yes", 1, 0)), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart of train")
gain <- gains(ifelse(logit.valid.result.df$actual == "Yes", 1, 0), logit.valid.result.df$prob, groups = dim(logit.valid.result.df)[1])
plot(c(0, gain$cume.plogit.of.total * sum(ifelse(logit.valid.result.df$actual == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l", main = "valid")
lines(c(0, sum(ifelse(logit.valid.result.df$actual == "Yes", 1, 0))) ~ c(0, dim(logit.valid.result.df)[1]), col = "gray", lty = 2)
gain <- gains(ifelse(logit.valid.result.df$actual == "Yes", 1, 0), logit.valid.result.df$prob)
barplot(gain$mean.resp / mean(ifelse(logit.valid.result.df$actual == "Yes", 1, 0)), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart of valid")
par(mfcol = c(1, 1))


# |---- 6.3 Evaluation Naive Bayes Model ====
# accuracy, sensitivity, specificity, precision, FDR, FOR
nb.valid <- confusionMatrix(factor(nb.valid.result.df$class),
                            factor(nb.valid.result.df$actual))
nb.train <- confusionMatrix(factor(nb.train.result.df$class),
                            factor(nb.train.result.df$actual))
nb.confusion <-list('valid' = c(nb.valid$overall[1],nb.valid$byClass[1],
                                nb.valid$byClass[2], nb.valid$byClass[3], 1- nb.valid$byClass[3], 
                                1 - nb.valid$byClass[4]), 'train' = c(nb.train$overall[1],nb.train$byClass[1], nb.train$byClass[2], nb.train$byClass[3], 
                                                                      1- nb.train$byClass[3], 1 - nb.train$byClass[4]))
names(nb.confusion[[1]]) <- c('accuracy', 'sensitivity', 'specificity','precision', 'FDR', 'FOR')
names(nb.confusion[[2]]) <- c('accuracy', 'sensitivity', 'specificity','precision', 'FDR', 'FOR')
nb.confusion
# ROC chart, AUC of ROC curve
p <- ifelse(nb.valid.result.df$class == 'Yes', 1, 0); r <- ifelse(nb.valid.result.df$actual == 'Yes', 1, 0)
nb.valid.roc <- roc(predictor = p, response = r, levels = c(0,1), direction='<')
p <- ifelse(nb.train.result.df$class == 'Yes', 1, 0); r <- ifelse(nb.train.result.df$actual == 'Yes', 1, 0)
nb.train.roc <- roc(predictor = p, response = r, levels = c(0,1), direction='<')
plot.roc(nb.train.roc, col = 'blue', print.auc = TRUE)
plot.roc(nb.valid.roc, add = TRUE, col = 'red', print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)
# Lift chart
lift.train <- lift(relevel(nb.train.result.df$actual, ref = "Yes") ~ prob.Yes, data = nb.train.result.df)
lift.valid <- lift(relevel(nb.valid.result.df$actual, ref = "Yes") ~ prob.Yes, data = nb.valid.result.df)
xyplot(lift.train, plot = "gain", main = "train")
xyplot(lift.valid, plot = "gain", main = "valid", col = 'red')
par(mfcol = c(2, 2))
gain <- gains(ifelse(nb.train.result.df$actual == "Yes", 1, 0), nb.train.result.df$prob.Yes, groups = dim(nb.train.result.df)[1])
plot(c(0, gain$cume.pnb.of.total * sum(ifelse(nb.train.result.df$actual == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l", main = "train")
lines(c(0, sum(ifelse(nb.train.result.df$actual == "Yes", 1, 0))) ~ c(0, dim(nb.train.result.df)[1]), col = "gray", lty = 2)
gain <- gains(ifelse(nb.train.result.df$actual == "Yes", 1, 0), nb.train.result.df$prob.Yes)
barplot(gain$mean.resp / mean(ifelse(nb.train.result.df$actual == "Yes", 1, 0)), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart of train")
gain <- gains(ifelse(nb.valid.result.df$actual == "Yes", 1, 0), nb.valid.result.df$prob.Yes, groups = dim(nb.valid.result.df)[1])
plot(c(0, gain$cume.pnb.of.total * sum(ifelse(nb.valid.result.df$actual == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l", main = "valid")
lines(c(0, sum(ifelse(nb.valid.result.df$actual == "Yes", 1, 0))) ~ c(0, dim(nb.valid.result.df)[1]), col = "gray", lty = 2)
gain <- gains(ifelse(nb.valid.result.df$actual == "Yes", 1, 0), nb.valid.result.df$prob.Yes)
barplot(gain$mean.resp / mean(ifelse(nb.valid.result.df$actual == "Yes", 1, 0)), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart of valid")
par(mfcol = c(1, 1))

# 7 Model Deployment - logistic regression ====
# From Step 6, we know that logistic regression could be the best model for this case.
set.seed(666)
train.index <- sample(c(1:dim(tcc.df)[1]), dim(tcc.df)[1] * 0.6)
train.df <- tcc.df[train.index, ]
valid.df <- tcc.df[-train.index, ]
# Build logistic regression model
logit.reg <- glm(Churn ~ ., data = train.df, family = "binomial")
options(scipen = 999)
summary(logit.reg)
# Prediction
# Compute propensity
pred.prob <- predict(logit.reg, valid.df[, -1], type = "response")
# Classification
pred.class <- ifelse(pred.prob >= 0.5, 'Yes','No' )
# Evaluating classification performance, Churn = "Yes" is the class of interest
confusionMatrix(factor(pred.class, levels = c('Yes','No')),
                factor(valid.df$Churn, levels = c('Yes', 'No')))
# Lift chart
gain <- gains(ifelse(valid.df$Churn == "Yes", 1, 0), pred.prob, groups = length(pred.prob))
plot(c(0, gain$cume.pct.of.total * sum(ifelse(valid.df$Churn == "Yes", 1, 0))) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative # of responses", main = "Lift chart", type = "l")
lines(c(0, sum(ifelse(valid.df$Churn == "Yes", 1, 0))) ~ c(0, dim(valid.df)[1]), lty = 2)
# Decile-wise lift chart
gain <- gains(ifelse(valid.df$Churn == "Yes", 1, 0), pred.prob)
heights <- gain$mean.resp/mean(ifelse(valid.df$Churn == "Yes", 1, 0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0, 3.5),
                     xlab = "Percentile", ylab = "Mean Response",
                     main = "Decile-wise lift chart")
text(midpoints, heights + 0.5, labels = round(heights, 1), cex = 0.8)


# Optional try: further explore ====
# Through all above, we know:
# 1. The accuracy is around 0.8, and the AUC is around 0.7.  We could try new variables from original ones to improve it.
# 2. When 'Positive' Class is Yes, the sensitivity is not good enough to predict "Yes" for "Churn".  We could try oversampling to improve it.
# |---- 7.1 to generate new variable ====
# generate "ChargeInCreased" which influencing customer decisions
tccnew.df <- read.csv("Telco-Customer-Churn.csv", header = TRUE)
tccnew.df <- na.omit(tccnew.df)
tcc.df[,"ChargeInCreased"] <- ifelse(tccnew.df$TotalCharges/tccnew.df$tenure < tccnew.df$MonthlyCharges, 'Yes', 'No')
# |---- 7.2 Oversampling ====
yes.rowindex <- which(tcc.df$Churn == 'Yes')
no.rowindex <- which(tcc.df$Churn == 'No')
set.seed(100)
yes.train.index <- sample(yes.rowindex, length(yes.rowindex)/2, replace = FALSE)
no.train.index <- sample(no.rowindex, length(yes.rowindex)/2, replace = FALSE)
train.index <- c(yes.train.index,no.train.index)
yes.valid.index<- setdiff(yes.rowindex, yes.train.index)
no.rowindex <- setdiff(no.rowindex, no.train.index)
no.valid.index <- sample(no.rowindex, length(yes.valid.index), replace = FALSE)  
valid.index <- c(yes.valid.index,no.valid.index)
train.df <- tcc.df[train.index, ]
valid.df <- tcc.df[valid.index, ]
# Build logistic regression model
logit.reg <- glm(Churn ~ ., data = train.df, family = "binomial")
options(scipen = 999)
summary(logit.reg)
# Prediction
# Compute propensity
pred.prob <- predict(logit.reg, valid.df[, -1], type = "response")
# Classification
pred.class <- ifelse(pred.prob >= 0.5, 'Yes','No' )
# Evaluating classification performance, Churn = "Yes" is the class of interest
cm <- confusionMatrix(factor(pred.class, levels = c('Yes','No')),
                factor(valid.df$Churn, levels = c('Yes', 'No')))
cm.table <- cm$table
# oversampling weight
c <- data.frame(table(tcc.df$Churn))
c.yes <- c[2,2]/(c[1,2]+c[2,2])*100
c.no <- c[1,2]/(c[1,2]+c[2,2])*100
cm.table[,1] <- cm.table[,1]/(50/c.yes)
cm.table[,2] <- cm.table[,1]/(50/c.no)
confusionMatrix(cm.table)
