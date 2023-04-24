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



#=======================================================================================================================================================================================================================
#====================================================================================================================================================================================================
data <- df_ENG %>%
  dplyr::select(Graduated_within_4_years,Sex,CM131_1,MA131_1,MA132_1,PH131_1,PH132_1,HS_Percentile_Rank,SAT_Math_Convert,SAT_RW_Convert,Minority,Majority)
data <- data %>% drop_na()

standardize <- function(x) {
  return((x - mean(x)) / sd(x))
}
# Standardize predictor variables
data[,c("Sex","CM131_1","MA131_1","MA132_1","PH131_1","PH132_1","HS_Percentile_Rank","SAT_Math_Convert","SAT_RW_Convert","Minority","Majority")] <- sapply(data[,c("Sex","CM131_1","MA131_1","MA132_1","PH131_1","PH132_1","HS_Percentile_Rank","SAT_Math_Convert","SAT_RW_Convert","Minority","Majority")], standardize)

# Set the seed for reproducibility
set.seed(1)

# Split the data into training and testing sets (80% training, 20% testing)
split <- sample.split(data$Graduated_within_4_years, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Fit the logistic regression model with cross-validation
cv_model <- cv.glmnet(
  as.matrix(train_data[, c("Sex","CM131_1","MA131_1","MA132_1","PH131_1","PH132_1","HS_Percentile_Rank","SAT_Math_Convert","SAT_RW_Convert","Minority","Majority")]), # Predictor variables
  train_data$y,      # Response variable
  family = "binomial", # Specify the binomial family for logistic regression
  alpha = 1,        # Use L1 regularization (Lasso)
  nfolds = 5,       # Number of folds for cross-validation
  type.measure = "class", # Use classification accuracy as the performance metric
  parallel = TRUE   # Use parallel computing for cross-validation
)


#============================================================================================================================================================================================================================
#===========================================================================Model1============================================================================================================================================








# Predict the classes on the testing data
predicted_probabilities <- predict(cv_model, newx = as.matrix(test_data[, c("x1", "x2", "x3")]))
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Compute the accuracy of the model
accuracy <- sum(predicted_classes == test_data$y) / nrow(test_data)
print(paste("Model Accuracy:", round(accuracy, 3)))




