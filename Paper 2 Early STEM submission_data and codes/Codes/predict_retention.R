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
# The next line gives the high risk students (comment these lines to obtain low risk students only)
df_ENG <- df_ENG[which(df_ENG$M_minus_P_minus==1 | df_ENG$M_minus_P_minus_plus==1),]
df_ENG <- df_ENG[which(df_ENG$Intervention==1),] #set to 0 or 1 for pre or post intervention

# # The next line gives the low risk students
# #(when analysing low-risk students, treatment group has 0 observations, so do not run t-tests)
# df_ENG <- df_ENG[which(df_ENG$M_minus_P_minus==0 | df_ENG$M_minus_P_minus_plus==0),] 
# df_ENG <- df_ENG[which(df_ENG$Intervention==0),] #set to 0 or 1 for pre or post intervention


# Treatment group are the high-risk(Delayed) ENG students who took ES110 in their first semester from 2011 onward
df_treatment <- df_ENG[which(df_ENG$ES110_in_1st_semester==1 & df_ENG$PH131_in_1st_semester==0 & df_ENG$PH131_in_1st_year==1 &  df_ENG$Primary_Cohort>=2011 & df_ENG$Clarkson_School==0),]
# The early control group is the group of high-risk ENG students before 2011 who did not take ES110 in sem 1
df_control <-df_ENG[which(df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1 & df_ENG$Primary_Cohort<=2010 & df_ENG$Clarkson_School==0),]

#=================================================================================================================================================================================================
#==================================================================================================================================================================================================
logisticretentiondata <- df_ENG %>%
  dplyr::select(Retained_STEM_in_year2,Sex,Math_Percent,Physics_Percent,CM131_1,MA131_1,ES110_1,HS_Percentile_Rank,SAT_combined,Majority)
logisticretentiondata <- logisticretentiondata %>% drop_na()

library("PerformanceAnalytics") #this package is used to create the correlation matrix
my_data <- logisticretentiondata
chart.Correlation(my_data, histogram=TRUE, pch=19)

#=======================================================================================================================================================================================
#===========================================================================Model1======================================================================================================
set.seed(2)
#option 1, guarantee at least 30% of each group in the target variable
train_idx <- c()
test_idx <- c()
for (i in unique(logisticretentiondata$Retained_STEM_in_year2)) {
  class_idx <- which(logisticretentiondata$Retained_STEM_in_year2 == i)
  n_class <- length(class_idx)
  n_test <- round(0.4 * n_class)
  test_idx <- c(test_idx, sample(class_idx, n_test))
  train_idx <- c(train_idx, setdiff(class_idx, test_idx))
}
trainretentiondata <- logisticretentiondata[train_idx, ]
testretentiondata <- logisticretentiondata[test_idx, ]
#============================================================================================================================================================

# #option2
# train_index <- sample(1:nrow(logisticretentiondata), size = 0.8*nrow(logisticretentiondata))
# trainretentiondata = logisticretentiondata[train_index,]
# testretentiondata = logisticretentiondata[-train_index,]
#============================================================================================================================================================

logistic_retention_model <- glm(Retained_STEM_in_year2 ~ ., data = trainretentiondata, family = "binomial")
summary(logistic_retention_model)
predict_4yr_reg <- predict(logistic_retention_model, testretentiondata, type = "response")
predict_4yr_reg <- ifelse(predict_4yr_reg >0.5, 1, 0)
predict_4yr_reg <- as.factor(predict_4yr_reg)
expected_values <- as.factor(testretentiondata$Retained_STEM_in_year2)

#==============================Evaluating model accuracy using confusion matrix (Accuracy, Sensitivity and Specificity =================================================================
library(caret)
results <- confusionMatrix(data=predict_4yr_reg, reference = expected_values)
results
#================================Creating ROC curve====================================================================================================
library(pROC)
test_prob = predict(logistic_retention_model, newdata = testretentiondata, type = "response")
jpeg("AUROC_predict_1styear_retention_preintervention.jpeg",width=500,height=550)
test_roc = roc(testretentiondata$Retained_STEM_in_year2 ~ test_prob, plot = TRUE, print.auc = TRUE)
dev.off()

#===================================================== Model1 VIF calculation ====================================================
X <- model.matrix(logistic_retention_model)[,-1]
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
#======================================================================================================================================================================











#=======================================Linear Regression Model to predict Degree_GPA========================================================================================================
mydata <- df_ENG %>%
  dplyr::select(Degree_GPA,Sex,CM131_1,CM132_1,MA131_1,MA132_1,PH131_1,PH132_1,Term_1_Term_GPA,Cum_GPA_Yr2,HS_Percentile_Rank,SAT_combined,Minority)
#my_data <- mydata %>% drop_na()
head(mydata, 3)
#==========================================================Sample the dataset. The return for this is row nos===================================================================
set.seed(1)
row.number <- sample(1:nrow(mydata), 0.8*nrow(mydata))
train = mydata[row.number,]
test = mydata[-row.number,]

#Let’s make a linear regression model.
model1 = lm(Degree_GPA~., data=train)
summary(model1)

#remove the less significant feature
model2 = update(model1, ~.-Cum_GPA_Yr2)
summary(model2)



#PROPORTION Z TESTS#
#=============================================================================================================================================================
# Select Engineering students only
#===============================================================================
df_ENG <-df[which(df$Acad_Prog=='ENG'),]
# The next line gives all high risk students
df_ENG <- df_ENG[which(df_ENG$M_minus_P_minus==1 | df_ENG$M_minus_P_minus_plus==1),] 
#df_ENG <- df_ENG[which(df_ENG$Sex_female==1),] #select only females
#df_ENG <- df_ENG[which(df_ENG$Minority==1),] #select only minorities

# Treatment group are the high-risk(Delayed) ENG students who took ES110 in their first semester from 2011 onward
df_treatment <- df_ENG[which(df_ENG$ES110_in_1st_semester==1 & df_ENG$PH131_in_1st_semester==0 & df_ENG$PH131_in_1st_year==1 &  df_ENG$Primary_Cohort>=2011 & df_ENG$Clarkson_School==0),]
# The early control group is the group of high-risk ENG students before 2011 who did not take ES110 in sem 1
df_control <-df_ENG[which(df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1 & df_ENG$Primary_Cohort<=2010 & df_ENG$Clarkson_School==0),]

#=================================================================================================================================================================================================
# Two proportions z test of treatment vs control_early for 4 year 
GPA_treatment_sum<- mean(df_treatment$Degree_GPA,na.rm=TRUE) # number of treatment students who graduated in 4 years
GPA_control_sum<- mean(df_control$Degree_GPA,na.rm=TRUE) # number of early control students who graduated in 4 years

total_treatment_sum<- nrow(df_treatment)# number of treatment students
total_control_sum<- nrow(df_control)# number of early control students

x_time<-c(GPA_treatment_sum, GPA_control_sum) # this changes with quantity
n<-c(total_treatment_sum, total_control_sum) # this is the same for all proportions

# H_null: treatment_proportion = control_proportion
# H_a   : treatment_proportion < control_proportion
result_time <- t.test(x=df_treatment$Degree_GPA, y = df_control$Degree_GPA,
                      alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

if(result_time$p.value < 0.05){
  sprintf("Treatment group has a better GRADUATED GPA than the control group with p value %1.3f.", result_time$p.value)
}else{
  sprintf("Treatment group DOES NOT have better GRADUATED GPA than the control group with p value %1.3f.", result_time$p.value)
}
