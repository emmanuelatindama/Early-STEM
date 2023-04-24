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



#====================================================================================================================================================================================================
logistic6yrdata <- df_ENG %>%
  dplyr::select(Graduated_within_6_years,Sex,Math_Percent,Physics_Percent,Cum_GPA_Yr2,CM131_1,CM132_1,MA131_1,MA132_1,PH131_1,PH132_1,HS_Percentile_Rank,SAT_combined,Majority)
logistic6yrdata <- logistic6yrdata %>% drop_na()
##Sample the dataset. The return for this is row nos.
#set.seed(7)
train_index <- sample(1:nrow(logistic6yrdata), size = 0.75*nrow(logistic6yrdata))
trainlogistic6yr = logistic6yrdata[train_index,]
testlogistic6yr = logistic6yrdata[-train_index,]

logistic_6yr_model <- glm(Graduated_within_6_years~., data = trainlogistic6yr, family = "binomial")
summary(logistic_6yr_model)

# Predict test data based on model
predict_6yr_reg <- predict(logistic_6yr_model, testlogistic6yr, type = "response")
predict_6yr_reg <- ifelse(predict_6yr_reg >0.5, 1, 0)

# Evaluating model accuracy using confusion matrix
table(testlogistic6yr$Graduated_within_6_years, predict_6yr_reg)

accuracy <- mean(predict_6yr_reg == testlogistic6yr$Graduated_within_6_years)
print(paste('Accuracy =', accuracy))
#===========================================================================================================================================================================
#============================================ VIF calculation ==============================================================================================================
X <- model.matrix(logistic_6yr_model)[,-1]
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
model2 = update(logistic_6yr_model, ~.-Cum_GPA_Yr2) 
summary(model2)
predict_model2 <- predict(model2, testlogistic6yr, type = "response")
predict_model2 <- ifelse(predict_model2 >0.5, 1, 0)
# Evaluating model accuracy using confusion matrix
table(testlogistic6yr$Graduated_within_6_years, predict_model2)
accuracy <- mean(predict_model2 == testlogistic6yr$Graduated_within_6_years)
print(paste('Accuracy =', accuracy))

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

