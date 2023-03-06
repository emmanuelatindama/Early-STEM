# ===================================================================
# The response variable is the duration for completion of the degree
# ===================================================================
library(plotrix)
library(ggplot2) 
library(gridExtra)
library(dplyr)

setwd("~/Desktop/Early STEM submission_data/Codes");
#source("myRfunctions.R")
#ccc()
#source("myRfunctions.R") # Redo the source, since ccc wiped out everything
#libraries() # get the necessary libraries
df_original<-read.csv("Cohort_September_2021_EA_PVA_MR.csv");
# Get and clean the Excel data unless you already have it in the workspace

#===============================================================================
# Remove the empty values, and force everything as numeric variables
#===============================================================================

# choose cohort here
first_year<-2006
last_year<-2015
total_years<-last_year-first_year+1


df <- df_original[which(df_original$Primary_Cohort <= last_year),] 
df <- df[which(df$Primary_Cohort==as.integer(df$Primary_Cohort)),]
df_ENG <-df[which(df$Acad_Prog=='ENG'),]
#the next line gives the high risk students
df_ENG <- df_ENG[which(df_ENG$M_minus_P_minus==1 | df_ENG$M_minus_P_minus_plus==1),]

#=========================Let's define all the relevant groups here!==========================

# Treatment group are the high-risk(Delayed) ENG students who took ES110 in their first semester from 2011 onward
df_treatment <- df_ENG[which(df_ENG$ES110_in_1st_semester==1 & df_ENG$PH131_in_1st_semester==0 & df_ENG$PH131_in_1st_year==1 &  df_ENG$Primary_Cohort>=2011 & df_ENG$Clarkson_School==0),]

# The early control group is the group of high-risk ENG students before 2011 who did not take ES110 in sem 1
df_control <-df_ENG[which(df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1 & df_ENG$Primary_Cohort<=2010 & df_ENG$Clarkson_School==0),]

# Next, we drop all international students 
df_treatment <- df_treatment[which(df_treatment$International == 0 ),]
df_control <- df_control[which(df_control$International == 0 ),]


# All Q3 ENG students who did not take ES110. # Needed for proportion test
df_control_all <- df_control
total_treatment_sum<- nrow(df_treatment)# number of treatment students
total_control_sum<- nrow(df_control_all)# number of early control students


#===============================================================================================================================
# combine the control group in 2006-2010 with treatment group (2011-2015)
df <- rbind(df_control_all, df_treatment)
df_min_SAT = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Minority==1),]
df_maj_SAT = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Majority==1),]
df_min_SAT_4y = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Minority==1 & df$Graduated_in_4_Yrs==1),]
df_maj_SAT_4y = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Majority==1 & df$Graduated_in_4_Yrs==1),]
df_min_SAT_6y = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Minority==1 & df$Graduated_in_6_Yrs==1),]
df_maj_SAT_6y = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Majority==1 & df$Graduated_in_6_Yrs==1),]

df_min_rank = df[which(df$HS_Percentile_Rank>0 & df$Minority==1),]
df_maj_rank = df[which(df$HS_Percentile_Rank>0 & df$Majority==1),]
df_min_rank_4y = df[which(df$HS_Percentile_Rank>0 & df$Minority==1 & df$Graduated_in_4_Yrs==1),]
df_maj_rank_4y = df[which(df$HS_Percentile_Rank>0 & df$Majority==1 & df$Graduated_in_4_Yrs==1),]
df_min_rank_6y = df[which(df$HS_Percentile_Rank>0 & df$Minority==1 & df$Graduated_in_6_Yrs==1),]
df_maj_rank_6y = df[which(df$HS_Percentile_Rank>0 & df$Majority==1 & df$Graduated_in_6_Yrs==1),]



Primary_Cohort<-first_year:last_year # list of years 2006 to 2015
df_visual <- data.frame(matrix(ncol = 1, nrow = total_years))# create a dataframe to store data

colnames(df_visual)<-c("year") # name the first column as year
df_visual$year<-Primary_Cohort # enter years 2006 to 2011 in the 1st column, year
options(digits = 5) # displays 3 digits after the decimal point



for (i in 1:total_years){
  year<-2005+i
  # I think this should use 
  df_year<-df[df$Primary_Cohort==year & df$SAT_Math_Convert>0,] # this is important step
  df_rank <- df_year[complete.cases(df_year$HS_Percentile_Rank), ]
  
  
  df_visual$num_students[i]<-nrow(df_year)
  df_visual$num_females[i] = sum(df_year$Sex_female)
  df_visual$num_females_4yrs[i] = sum(df_year$Sex_female * df_year$Graduated_within_4_years)
  df_visual$num_females_6yrs[i] = sum(df_year$Sex_female * df_year$Graduated_within_6_years)
  df_visual$num_males[i] = sum(df_year$Sex)
  df_visual$num_males_4yrs[i] = sum(df_year$Sex * df_year$Graduated_within_4_years)
  df_visual$num_males_6yrs[i] = sum(df_year$Sex * df_year$Graduated_within_6_years)
  df_visual$num_minority[i] <- sum(df_year$Minority)
  df_visual$num_minority_4yrs[i] <- sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs))
  df_visual$num_minority_6yrs[i] <- sum((df_year$Minority)*(df_year$Graduated_in_6_Yrs))
  df_visual$num_majority[i] <- sum(df_year$Majority)
  df_visual$num_majority_4yrs[i] <- sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs))
  df_visual$num_majority_6yrs[i] <- sum((df_year$Majority)*(df_year$Graduated_in_6_Yrs))
  df_visual$num_f_minority[i] = sum(df_year$Sex_female *df_year$Minority)
  df_visual$num_f_minority_4yrs[i] = sum(df_year$Sex_female * df_year$Graduated_within_4_years * df_year$Minority)
  df_visual$num_f_minority_6yrs[i] = sum(df_year$Sex_female * df_year$Graduated_within_6_years * df_year$Minority)
  df_visual$num_f_majority[i] = sum(df_year$Sex_female *df_year$Majority)
  df_visual$num_f_majority_4yrs[i] = sum(df_year$Sex_female * df_year$Graduated_within_4_years * df_year$Majority)
  df_visual$num_f_majority_6yrs[i] = sum(df_year$Sex_female * df_year$Graduated_within_6_years * df_year$Majority)
  df_visual$num_m_minority[i] = sum(df_year$Sex *df_year$Minority)
  df_visual$num_m_minority_4yrs[i] = sum(df_year$Sex * df_year$Graduated_within_4_years * df_year$Minority)
  df_visual$num_m_minority_6yrs[i] = sum(df_year$Sex * df_year$Graduated_within_6_years * df_year$Minority)
  df_visual$num_m_majority[i] = sum(df_year$Sex *df_year$Majority)
  df_visual$num_m_majority_4yrs[i] = sum(df_year$Sex * df_year$Graduated_within_4_years * df_year$Majority)
  df_visual$num_m_majority_6yrs[i] = sum(df_year$Sex * df_year$Graduated_within_6_years * df_year$Majority)
  #Percentages
  df_visual$per_female[i]<-signif(100*df_visual$num_females[i]/df_visual$num_students[i], 3)
  df_visual$per_male[i]<-signif(100*df_visual$num_males[i]/df_visual$num_students[i], 3)
  df_visual$per_female_4yrs[i]<-signif(100*df_visual$num_females_4yrs[i]/df_visual$num_females[i], 3)
  df_visual$per_male_4yrs[i]<-signif(100*df_visual$num_males_4yrs[i]/df_visual$num_males[i], 3)
  df_visual$per_female_6yrs[i] <- signif(100*df_visual$num_females_6yrs[i]/df_visual$num_females[i], 3)
  df_visual$per_male_6yrs[i] <- signif(100*df_visual$num_males_6yrs[i]/df_visual$num_males[i], 3)
  df_visual$per_minority[i]<-signif(100*df_visual$num_minority[i]/df_visual$num_students[i], 3)
  df_visual$per_majority[i]<-signif(100*df_visual$num_majority[i]/df_visual$num_students[i], 3)
  df_visual$per_minority_4yrs[i]<-signif(100*df_visual$num_minority_4yrs[i]/df_visual$num_minority[i], 3)
  df_visual$per_majority_4yrs[i]<-signif(100*df_visual$num_majority_4yrs[i]/df_visual$num_majority[i], 3)
  df_visual$per_minority_6yrs[i]<-signif(100*df_visual$num_minority_6yrs[i]/df_visual$num_minority[i], 3)
  df_visual$per_majority_6yrs[i]<-signif(100*df_visual$num_majority_6yrs[i]/df_visual$num_majority[i], 3)
  
  df_visual$per_m_minority[i]<-signif(100*df_visual$num_m_minority[i]/df_visual$num_minority[i], 3)
  df_visual$per_f_minority[i]<-signif(100*df_visual$num_f_minority[i]/df_visual$num_minority[i], 3)
  df_visual$per_m_majority[i]<-signif(100*df_visual$num_m_majority[i]/df_visual$num_majority[i], 3)
  df_visual$per_f_majority[i]<-signif(100*df_visual$num_f_majority[i]/df_visual$num_majority[i], 3)
  
  df_visual$per_f_minority_4yrs[i]<-signif(100*df_visual$num_f_minority_4yrs[i]/df_visual$num_minority_4yrs[i], 3)
  df_visual$per_m_minority_4yrs[i]<-signif(100*df_visual$num_m_minority_4yrs[i]/df_visual$num_minority_4yrs[i], 3)
  df_visual$per_f_majority_4yrs[i]<-signif(100*df_visual$num_f_majority_4yrs[i]/df_visual$num_majority_4yrs[i], 3)
  df_visual$per_m_majority_4yrs[i]<-signif(100*df_visual$num_m_majority_4yrs[i]/df_visual$num_majority_4yrs[i], 3)
  df_visual$per_f_minority_6yrs[i]<-signif(100*df_visual$num_f_minority_6yrs[i]/df_visual$num_minority_6yrs[i], 3)
  df_visual$per_m_minority_6yrs[i]<-signif(100*df_visual$num_m_minority_6yrs[i]/df_visual$num_minority_6yrs[i], 3)
  df_visual$per_f_majority_6yrs[i]<-signif(100*df_visual$num_f_majority_6yrs[i]/df_visual$num_majority_6yrs[i], 3)
  df_visual$per_m_majority_6yrs[i]<-signif(100*df_visual$num_m_majority_6yrs[i]/df_visual$num_majority_6yrs[i], 3)
  
}

#NOTE: we subtract a number in order to zoom in on the graph
#=========================================Beginning of Plots========================================================
# 1. Proportion of Female & Male High Risk students (and graduation proportions) Histogram
png("HR_f_m_prop.png",width=780,height=880)

gender_rates <- data.frame(cbind(df_visual$year, df_visual$per_male)) 
plot_gender <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = gender_rates[which(gender_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = gender_rates[which(gender_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of males in HR") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=gender_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

f_gender_rates <- data.frame(cbind(df_visual$year, df_visual$per_female)) 
f_plot_gender <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = f_gender_rates[which(f_gender_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = f_gender_rates[which(f_gender_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of females in HR") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=f_gender_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

gender_4y_rates <- data.frame(cbind(df_visual$year, df_visual$per_male_4yrs)) 
plot_gender_4y <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = gender_4y_rates[which(gender_4y_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = gender_4y_rates[which(gender_4y_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of males in HR grad 4yrs") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=gender_4y_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

f_gender_4y_rates <- data.frame(cbind(df_visual$year, df_visual$per_female_4yrs)) 
f_plot_gender_4y <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = f_gender_4y_rates[which(f_gender_4y_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = f_gender_4y_rates[which(f_gender_4y_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of females in HR grad 4yrs") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=f_gender_4y_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

gender_grad_rates <- data.frame(cbind(df_visual$year, df_visual$per_male_6yrs)) 
plot_gender_grad <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = gender_grad_rates[which(gender_grad_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = gender_grad_rates[which(gender_grad_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of males in HR grad 6yrs") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=gender_grad_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

f_gender_grad_rates <- data.frame(cbind(df_visual$year, df_visual$per_female_6yrs)) 
f_plot_gender_grad <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = f_gender_grad_rates[which(f_gender_grad_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = f_gender_grad_rates[which(f_gender_grad_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of females in HR grad 6yrs") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=f_gender_grad_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

table_f_m_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), f=df_visual$num_females,
                                f_p=df_visual$per_female, male=df_visual$num_males, m_p=df_visual$per_male))
table_4yr = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), f_4yr=df_visual$num_females_4yrs ,
                             f_4y_p= df_visual$per_female_4yrs, m_4yr=df_visual$num_males_4yrs, m_4y_p= df_visual$per_male_4yrs))
table_6yr = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), f_6yr=df_visual$num_females_6yrs ,
                            f_6y_p= df_visual$per_f_6yrs, m_6yr=df_visual$num_males_6yrs, m_6y_p= df_visual$per_male_6yrs))
grid.arrange(f_plot_gender, plot_gender, table_f_m_num, f_plot_gender_4y, plot_gender_4y, table_4yr, f_plot_gender_grad, plot_gender_grad, table_6yr, ncol=3, nrow=3) #this creates the 2x2 grid

dev.off()



# # Two proportions z test of increase in proportion of minority vs majority  
# n_min_maj = c(100, 100)
# x_min_maj<-c(mean(df_visual$per_minority[6:10])-mean(df_visual$per_minority[1:5]), mean(df_visual$per_majority[6:10])-mean(df_visual$per_majority[1:5]))
# result_min_maj<-prop.test(x_min_maj, n_min_maj, alternative = "greater", correct = TRUE)
# if(result_min_maj$p.value < 0.05){
#   sprintf("minority proportion increase is greater than majority proportion increase with p value %1.3f.", result_min_maj$p.value)
# }else{
#   sprintf("minority proportion increase is NOT greater than majority proportion increase with p value %1.3f.", result_min_maj$p.value)
# }

# Two proportions test of increase in proportion of min_maj 4yr graduation rates treatment vs control  
n_min_maj_4yrs = c(100, 100)
x_min_maj_4yrs<-c(mean(df_visual$per_minority_4yrs[6:10])-mean(df_visual$per_minority_4yrs[1:5]), mean(df_visual$per_majority_4yrs[6:10])-mean(df_visual$per_majority_4yrs[1:5]))
result_min_maj_4yrs<-prop.test(x_min_maj_4yrs, n_min_maj_4yrs, alternative = "greater", correct = TRUE)
if(result_min_maj_4yrs$p.value < 0.05){
  sprintf("minority 4yr graduation rate increase is greater than majority 4yr graduation rate increase with p value %1.3f.", result_min_maj_4yrs$p.value)
}else{
  sprintf("minority 4yr graduation rate increase is NOT greater than majority 4yr graduation rate increase with p value %1.3f.", result_min_maj_4yrs$p.value)
}

# Two proportions z test of increase in proportion of min_maj 6yr graduation rates treatment vs control  
n_min_maj_6yrs = c(100, 100)
x_min_maj_6yrs<-c(mean(df_visual$per_minority_6yrs[6:10])-mean(df_visual$per_minority_6yrs[1:5]), mean(df_visual$per_majority_6yrs[6:10])-mean(df_visual$per_majority_6yrs[1:5]))
result_min_maj_6yrs<-prop.test(x_min_maj_6yrs, n_min_maj_6yrs, alternative = "greater", correct = TRUE)
if(result_min_maj_6yrs$p.value < 0.05){
  sprintf("minority 6yr graduation rate increase is greater than majority 6yr graduation rate increase with p value %1.3f.", result_min_maj_6yrs$p.value)
}else{
  sprintf("minority 6yr graduation rate increase is NOT greater than majority 6yr graduation rate increase with p value %1.3f.", result_min_maj_6yrs$p.value)
}


# Two proportions z test of increase in proportion of f_m 4yr graduation rates treatment vs control  
n_f_m_4yrs = c(100, 100)
x_f_m_4yrs<-c(mean(df_visual$per_female_4yrs[6:10])-mean(df_visual$per_female_4yrs[1:5]), mean(df_visual$per_male_4yrs[6:10])-mean(df_visual$per_male_4yrs[1:5]))
result_f_m_4yrs<-prop.test(x_f_m_4yrs, n_f_m_4yrs, alternative = "greater", correct = TRUE)
if(result_f_m_4yrs$p.value < 0.05){
  sprintf("female 4yr graduation rate increase is greater than male 4yr graduation rate increase with p value %1.3f.", result_f_m_4yrs$p.value)
}else{
  sprintf("female 4yr graduation rate increase is NOT greater than male 4yr graduation rate increase with p value %1.3f.", result_f_m_4yrs$p.value)
}

# Two proportions z test of increase in proportion of f_m 6yr graduation rates treatment vs control  
n_f_m_6yrs = c(100, 100)
x_f_m_6yrs<-c(mean(df_visual$per_female_6yrs[6:10])-mean(df_visual$per_female_6yrs[1:5]), mean(df_visual$per_male_6yrs[6:10])-mean(df_visual$per_male_6yrs[1:5]))
result_f_m_6yrs<-prop.test(x_f_m_6yrs, n_f_m_6yrs, alternative = "greater", correct = TRUE)
if(result_f_m_6yrs$p.value < 0.05){
  sprintf("female 6yr graduation rate increase is greater than male 6yr graduation rate increase with p value %1.3f.", result_f_m_6yrs$p.value)
}else{
  sprintf("female 6yr graduation rate increase is NOT greater than male 6yr graduation rate increase with p value %1.3f.", result_f_m_6yrs$p.value)
}


# a) Two proportions z test of increase in proportion of minority females vs minority males 4yr graduation rates treatment vs control  
n_f_m_min_4yrs = c(100, 100)
x1 = sum(df_visual$num_f_minority_4yrs[6:10])/sum(df_visual$num_f_minority[6:10]) - sum(df_visual$num_f_minority_4yrs[1:5])/sum(df_visual$num_f_minority[1:5])
x2 = sum(df_visual$num_m_minority_4yrs[6:10])/sum(df_visual$num_m_minority[6:10]) - sum(df_visual$num_m_minority_4yrs[1:5])/sum(df_visual$num_m_minority[1:5])
x_f_m_min_4yrs<-c(x1*100, x2*100)
result_f_m_min_4yrs<-prop.test(x_f_m_min_4yrs, n_f_m_min_4yrs, alternative = "greater", correct = TRUE)
if(result_f_m_min_4yrs$p.value < 0.05){
sprintf("female minority 4yr graduation rate increase is greater than male minority 4yr graduation rate increase with p value %1.3f.", result_f_m_min_4yrs$p.value)
}else{
sprintf("female minority 4yr graduation rate increase is NOT greater than male minority 4yr graduation rate increase with p value %1.3f.", result_f_m_min_4yrs$p.value)
}


# b) Two proportions z test of increase in proportion of minority females vs majority females 4yr graduation rates treatment vs control  
n_f_min_f_maj_4yrs = c(100, 100)
x1 = sum(df_visual$num_f_minority_4yrs[6:10])/sum(df_visual$num_f_minority[6:10]) - sum(df_visual$num_f_minority_4yrs[1:5])/sum(df_visual$num_f_minority[1:5])
x2 = sum(df_visual$num_f_majority_4yrs[6:10])/sum(df_visual$num_f_majority[6:10]) - sum(df_visual$num_f_majority_4yrs[1:5])/sum(df_visual$num_f_majority[1:5])
x_f_min_f_maj_4yrs<-c(x1*100, x2*100)
result_f_min_f_maj_4yrs<-prop.test(x_f_min_f_maj_4yrs, n_f_min_f_maj_4yrs, alternative = "greater", correct = TRUE)
if(result_f_min_f_maj_4yrs$p.value < 0.05){
  sprintf("female minority 4yr graduation rate increase is greater than female majority 4yr graduation rate increase with p value %1.3f.", result_f_min_f_maj_4yrs$p.value)
}else{
  sprintf("female minority 4yr graduation rate increase is NOT greater than female majority 4yr graduation rate increase with p value %1.3f.", result_f_min_f_maj_4yrs$p.value)
}


# c) Two proportions z test of increase in proportion of minority males vs majority males 4yr graduation rates treatment vs control  
n_m_min_m_maj_4yrs = c(100, 100)
x1 = sum(df_visual$num_m_minority_4yrs[6:10])/sum(df_visual$num_m_minority[6:10]) - sum(df_visual$num_m_minority_4yrs[1:5])/sum(df_visual$num_m_minority[1:5])
x2 = sum(df_visual$num_m_majority_4yrs[6:10])/sum(df_visual$num_m_majority[6:10]) - sum(df_visual$num_m_majority_4yrs[1:5])/sum(df_visual$num_m_majority[1:5])
x_m_min_m_maj_4yrs<-c(x1*100, x2*100)
result_m_min_m_maj_4yrs<-prop.test(x_m_min_m_maj_4yrs, n_m_min_m_maj_4yrs, alternative = "greater", correct = TRUE)
if(result_m_min_m_maj_4yrs$p.value < 0.05){
  sprintf("male minority 4yr graduation rate increase is greater than male majority 4yr graduation rate increase with p value %1.3f.", result_f_min_f_maj_4yrs$p.value)
}else{
  sprintf("male minority 4yr graduation rate increase is NOT greater than male majority 4yr graduation rate increase with p value %1.3f.", result_f_min_f_maj_4yrs$p.value)
}


# d) Two proportions z test of increase in proportion of majority females vs majority males 4yr graduation rates treatment vs control  
n_f_m_maj_4yrs = c(100, 100)
x1 = sum(df_visual$num_f_majority_4yrs[6:10])/sum(df_visual$num_f_majority[6:10]) - sum(df_visual$num_f_majority_4yrs[1:5])/sum(df_visual$num_f_majority[1:5])
x2 = sum(df_visual$num_m_majority_4yrs[6:10])/sum(df_visual$num_m_majority[6:10]) - sum(df_visual$num_m_majority_4yrs[1:5])/sum(df_visual$num_m_majority[1:5])
x_f_m_maj_4yrs<-c(x1*100, x2*100)
result_f_m_maj_4yrs<-prop.test(x_f_m_maj_4yrs, n_f_m_maj_4yrs, alternative = "greater", correct = TRUE)
if(result_f_m_maj_4yrs$p.value < 0.05){
  sprintf("female majority 4yr graduation rate increase is greater than male majority 4yr graduation rate increase with p value %1.3f.", result_f_m_maj_4yrs$p.value)
}else{
  sprintf("female majority 4yr graduation rate increase is NOT greater than male majority 4yr graduation rate increase with p value %1.3f.", result_f_m_maj_4yrs$p.value)
}


