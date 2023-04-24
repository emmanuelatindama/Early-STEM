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
df_ENG <- df_ENG[which(df_ENG$Intervention==0 & df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1),] 

# Treatment group are the high-risk(Delayed) ENG students who took ES110 in their first semester from 2011 onward
df_treatment <- df_ENG[which(df_ENG$ES110_in_1st_semester==1 & df_ENG$PH131_in_1st_semester==0 & df_ENG$PH131_in_1st_year==1 &  df_ENG$Primary_Cohort>=2011 & df_ENG$Clarkson_School==0),]
# The early control group is the group of high-risk ENG students before 2011 who did not take ES110 in sem 1
df_control <-df_ENG[which(df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1 & df_ENG$Primary_Cohort<=2010 & df_ENG$Clarkson_School==0),]


#=================================================================================================================================================================================================
#==================================================================================================================================================================================================
logistic4yrdata <- df_ENG %>%
  dplyr::select(Retained_STEM_in_year2,Sex,Math_Percent,Physics_Percent,CM131_1,MA131_1,PH131_1,HS_Percentile_Rank,SAT_combined,Majority,Minority)

library("PerformanceAnalytics") #this package is used to create the correlation matrix
my_data <- logistic4yrdata
chart.Correlation(my_data, histogram=TRUE, pch=19)



#============================================================================================================================================================================================================================
#===========================================================================Model1============================================================================================================================================
for (i in 1:10) {
  #Run the logistic regression model for 10 times
  set.seed(i)
  train_index <- sample(1:nrow(logistic4yrdata), size = 0.8*nrow(logistic4yrdata))
  trainlogistic4yr = logistic4yrdata[train_index,]
  testlogistic4yr = logistic4yrdata[-train_index,]
  logistic_4yr_model <- glm(Retained_STEM_in_year2 ~ ., data = trainlogistic4yr, family = "binomial")
  summary(logistic_4yr_model)
  predict_4yr_reg <- predict(logistic_4yr_model, testlogistic4yr, type = "response")
  predict_4yr_reg <- ifelse(predict_4yr_reg >0.5, 1, 0)
}
# Evaluating model accuracy using confusion matrix
table(testlogistic4yr$Retained_STEM_in_year2, predict_4yr_reg)
accuracy <- mean(predict_4yr_reg == testlogistic4yr$Retained_STEM_in_year2)
print(paste('Accuracy =', accuracy))
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
model2 = update(logistic_4yr_model, ~.-Minority) #update the previous model by removing a factor (Minority)
summary(model2)
predict_model2 <- predict(model2, testlogistic4yr, type = "response")
predict_model2 <- ifelse(predict_model2 >0.5494, 1, 0)
# Evaluating model accuracy using confusion matrix
table(testlogistic4yr$Retained_STEM_in_year2, predict_model2)
accuracy <- mean(predict_model2 == testlogistic4yr$Retained_STEM_in_year2)
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
#====================================================================================================================================================




#=======================================Linear Regression Model========================================================
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
