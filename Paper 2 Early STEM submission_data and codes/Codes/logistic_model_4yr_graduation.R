library(tidyr)    # to use drop_na
library(caTools)  # for logistic regression

setwd("~/Desktop/EARLY STEM 2/Codes");
df_original<-read.csv("Cohort_September_2021_EA_PVA_MR.csv");

#===============================================================================
# choose cohort here
#===============================================================================
first_year<-2006
last_year<-2015
total_years<-last_year-first_year+1

df <- df_original[which(df_original$Primary_Cohort <= last_year),] 
#===============================================================================
# Remove the empty values, and force everything as numeric variables
#===============================================================================
df <- df[which(df$Primary_Cohort==as.integer(df$Primary_Cohort)),]
#===============================================================================
# Select Engineering students only
#===============================================================================
df_ENG <-df[which(df$Acad_Prog=='ENG'),]
# The next line gives the high risk students
df_ENG <- df_ENG[which(df_ENG$M_minus_P_minus==1 | df_ENG$M_minus_P_minus_plus==1),] 
df_ENG <- df_ENG[which(df_ENG$Intervention==1),] # 1 for post-intervention and 0 for pre-intervention

#=================================================================================================================================================================================
logistic4yrdata <- df_ENG %>%
  dplyr::select(Graduated_within_4_years,Sex,Math_Percent,Physics_Percent,Cum_GPA_Yr2,CM131_1,CM132_1,MA131_1,MA132_1,PH131_1,PH132_1,HS_Percentile_Rank,SAT_combined,Majority)
logistic4yrdata <- logistic4yrdata %>% drop_na()

#=======================================================================================================================================================================================
#===========================================================================Model1======================================================================================================
#Run the logistic regression model for 10 times
set.seed(1)
train_index <- sample(1:nrow(logistic4yrdata), size = 0.8*nrow(logistic4yrdata))
trainlogistic4yr = logistic4yrdata[train_index,]
testlogistic4yr = logistic4yrdata[-train_index,]
logistic_4yr_model <- glm(Graduated_within_4_years ~ ., data = trainlogistic4yr, family = "binomial")
summary(logistic_4yr_model)
predict_4yr_reg <- predict(logistic_4yr_model, testlogistic4yr, type = "response")
predict_4yr_reg <- ifelse(predict_4yr_reg >0.5, 1, 0)
predict_4yr_reg <- as.factor(predict_4yr_reg)
expected_values <- as.factor(testlogistic4yr$Graduated_within_4_years)

#==============================Evaluating model accuracy using confusion matrix (Accuracy, Sensitivity and Specificity =================================================================
library(caret)
results <- confusionMatrix(data=predict_4yr_reg, reference = expected_values)
results
#================================Creating ROC curve====================================================================================================
library(pROC)
test_prob = predict(logistic_4yr_model, newdata = testlogistic4yr, type = "response")
test_roc = roc(testlogistic4yr$Graduated_within_4_years ~ test_prob, plot = TRUE, print.auc = TRUE)

#===========================================================================================================================================================================
#================================================================== Model1 VIF calculation ==============================================================================================================
X <- model.matrix(logistic_4yr_model)[,-1]
# Compute the VIF for each predictor variable
vif <- numeric(ncol(X))
for (i in 1:ncol(X)) {
  y <- X[, i]
  X_i <- X[, -i]
  fit <- lm(y ~ X_i)
  rsq <- summary(fit)$r.squared
  vif[i] <- 1 / (1 - rsq)
}
# Create a table with variable names and VIF values
vif_table <- data.frame(variable = colnames(X), vif = vif)
print(vif_table)


#======================================================== Update model (remove the less significant feature)============================================================================
model2 = update(logistic_4yr_model, ~.-Cum_GPA_Yr2) 
summary(model2)
predict_model2 <- predict(model2, testlogistic4yr, type = "response")
predict_model2 <- ifelse(predict_model2 >0.5494, 1, 0)
predict_model2 <- as.factor(predict_model2)

#==============================Evaluating model accuracy using confusion matrix (Accuracy, Sensitivity and Specificity =================================================================
library(caret)
results <- confusionMatrix(data=predict_model2, reference = expected_values)
results
#================================Creating ROC curve====================================================================================================
library(pROC)
test_prob = predict(model2, newdata = testlogistic4yr, type = "response")
jpeg("AUROC_predict_4year_graduation_postintervention.jpeg",width=500,height=550)
test_roc = roc(testlogistic4yr$Graduated_within_4_years ~ test_prob, plot = TRUE, print.auc = TRUE)
dev.off()

X <- model.matrix(model2)[,-1]
# Compute the VIF for each predictor variable
vif <- numeric(ncol(X))
for (i in 1:ncol(X)) {
  y <- X[, i]
  X_i <- X[, -i]
  fit <- lm(y ~ X_i)
  rsq <- summary(fit)$r.squared
  vif[i] <- 1 / (1 - rsq)
}
# Create a table with variable names and VIF values
vif_table <- data.frame(variable = colnames(X), vif = vif)
print(vif_table)