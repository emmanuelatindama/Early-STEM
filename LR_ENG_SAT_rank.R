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
#the next line gives the Low Risk students
df_ENG <- df_ENG[which(df_ENG$M_minus_P_minus==0 | df_ENG$M_minus_P_minus_plus==0),]

#=========================Let's define all the relevant groups here!==========================

# Treatment group are the low_risk ENG students  from 2011 onward
df_treatment <- df_ENG[which(df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1 & df_ENG$Primary_Cohort>=2011 & df_ENG$Clarkson_School==0),]

# The early control group is the low risk students before 2011
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
df_min_SAT_ot = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Minority==1 & df$Graduated_in_4_Yrs==1),]
df_maj_SAT_ot = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Majority==1 & df$Graduated_in_4_Yrs==1),]
df_min_SAT_6y = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Minority==1 & df$Graduated_in_6_Yrs==1),]
df_maj_SAT_6y = df[which(df$SAT_Math_Convert>0 & df$SAT_RW_Convert>0 & df$Majority==1 & df$Graduated_in_6_Yrs==1),]

df_min_rank = df[which(df$HS_Percentile_Rank>0 & df$Minority==1),]
df_maj_rank = df[which(df$HS_Percentile_Rank>0 & df$Majority==1),]
df_min_rank_ot = df[which(df$HS_Percentile_Rank>0 & df$Minority==1 & df$Graduated_in_4_Yrs==1),]
df_maj_rank_ot = df[which(df$HS_Percentile_Rank>0 & df$Majority==1 & df$Graduated_in_4_Yrs==1),]
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
  
  #Males_percentage
  df_visual$male_per[i]<-signif(100*sum(df_year$Sex)/df_visual$num_students[i], 3)
  #Females_percentage
  df_visual$female_per[i]<-signif(100*sum(df_year$Sex_female)/df_visual$num_students[i], 3)
  
  df_visual$num_males[i] = sum(df_year$Sex)
  df_visual$num_females[i] = sum(df_year$Sex_female)
  df_visual$num_males_ontime[i] = sum(df_year$Sex * df_year$Graduated_within_4_years)
  df_visual$num_females_ontime[i] = sum(df_year$Sex_female * df_year$Graduated_within_4_years)
  df_visual$num_males_grad[i] = sum(df_year$Sex * df_year$Graduated_within_6_years)
  df_visual$num_females_grad[i] = sum(df_year$Sex_female * df_year$Graduated_within_6_years)
  
  #Males_graduating_ontime
  df_visual$per_male_ontime[i]<-signif(100*df_visual$num_males_ontime[i]/df_visual$num_males[i], 3)
  #Females_graduating_ontime
  df_visual$per_female_ontime[i]<-signif(100*df_visual$num_females_ontime[i]/df_visual$num_females[i], 3)
  
  #Males_graduating_within_6_years
  df_visual$per_male_grad[i] <- signif(100*df_visual$num_males_grad[i]/df_visual$num_males[i], 3)
  #Females_graduating_within_6_years
  df_visual$per_female_grad[i] <- signif(100*df_visual$num_females_grad[i]/df_visual$num_females[i], 3)
  
  
  #Majority_groups_percentage
  df_visual$majority_per[i]<-signif(100*sum(df_year$Majority)/df_visual$num_students[i], 3)
  #Majority_groups_graduating_within_6_years
  df_visual$per_majority_grad[i]<-signif(100*sum((df_year$Majority)*(df_year$Graduated_within_6_years))/sum(df_year$Majority), 3)
  #Majority_groups_graduating_within_ontime
  df_visual$per_majority_ontime[i]<-signif(100*sum((df_year$Majority)*(df_year$Graduated_within_4_years))/sum(df_year$Majority), 3)
  #SAT Math scores of majority Low Risk students
  df_visual$SATmath_majority[i] <- signif(sum((df_year$Majority)*(df_year$SAT_Math_Convert))/sum(df_year$Majority),3)
  #SAT Math scores of majority Low Risk Students who graduate ontime
  df_visual$SATmath_majority_ontime[i] <- signif(sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs)*(df_year$SAT_Math_Convert))/sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs)),3)
  #=================================================================================================================================================================================================
  #SAT Verbal scores of majority Low Risk students
  df_visual$SATverbal_majority[i] <- signif(sum((df_year$Majority)*(as.numeric(paste(df_year$SAT_RW_Convert))))/sum(df_year$Majority),3)
  #SAT Verbal scores of majority Low Risk Students who graduate ontime
  df_visual$SATverbal_majority_ontime[i] <- signif(sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs)*(as.numeric(paste(df_year$SAT_RW_Convert))))/sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs)),3)
  #=================================================================================================================================================================================================
  # #HS GPA of majority Low Risk students
  # df_visual$GPA_majority[i] <- signif(sum((df_year$Majority)*(df_year$HS_GPA))/sum(df_year$Majority), 3)
  # #HS GPA of majority Low Risk Students who graduate ontime
  # df_visual$GPA_majority_ontime[i] <- signif(sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs)*(df_year$Majority)*(df_year$HS_GPA))/sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs)), 3)
  #HS rank of majority Low Risk students
  
  df_visual$rank_majority[i] <- signif(sum((df_rank$Majority)*(df_rank$HS_Percentile_Rank))/sum(df_rank$Majority), 3)
  #HS rank of majority Low Risk Students who graduate ontime
  df_visual$rank_majority_ontime[i] <- signif(sum((df_rank$Majority)*(df_rank$Graduated_in_4_Yrs)*(df_rank$Majority)*(df_rank$HS_Percentile_Rank))/sum((df_rank$Majority)*(df_rank$Graduated_in_4_Yrs)), 3)
  
  df_visual$num_majority_who_graduated_ontime[i] <- sum((df_year$Majority)*(df_year$Graduated_in_4_Yrs))
  df_visual$num_majority_who_graduated[i] <- sum((df_year$Majority)*(df_year$Graduated_in_6_Yrs))
  df_visual$num_majority[i] <- sum(df_year$Majority)
  
  #Minority_groups_percentage
  df_visual$minority_per[i]<-signif(100*sum(df_year$Minority)/df_visual$num_students[i], 3)
  #Minority_groups_graduating_within_6_years
  df_visual$per_minority_grad[i]<-signif(100*sum((df_year$Minority)*(df_year$Graduated_within_6_years))/sum(df_year$Minority), 3)
  #Minority_groups_graduating_within_ontime
  df_visual$per_minority_ontime[i]<-signif(100*sum((df_year$Minority)*(df_year$Graduated_within_4_years))/sum(df_year$Minority), 3)
  #SAT Math scores of minority Low Risk students
  df_visual$SATmath_minority[i] <- signif(sum((df_year$Minority)*(df_year$SAT_Math_Convert))/sum(df_year$Minority),3)
  #SAT Math scores of minority Low Risk Students who graduate ontime
  df_visual$SATmath_minority_ontime[i] <- signif(sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs)*(df_year$SAT_Math_Convert))/sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs)),3)
  #=================================================================================================================================================================================================
  #SAT Verbal scores of minority Low Risk students
  df_visual$SATverbal_minority[i] <- signif(sum((df_year$Minority)*(as.numeric(paste(df_year$SAT_RW_Convert))))/sum(df_year$Minority),3)
  #SAT Verbal scores of minority Low Risk Students who graduate ontime
  df_visual$SATverbal_minority_ontime[i] <- signif(sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs)*(as.numeric(paste(df_year$SAT_RW_Convert))))/sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs)),3)
  #=================================================================================================================================================================================================
  # #HS GPA of minority Low Risk students
  # df_visual$GPA_minority[i] <- signif(sum((df_year$Minority)*(df_year$HS_GPA))/sum(df_year$Minority), 3)
  # #HS GPA of minority Low Risk Students who graduate ontime
  # df_visual$GPA_minority_ontime[i] <- signif(sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs)*(df_year$Minority)*(df_year$HS_GPA))/sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs)), 3)
  #HS rank of minority Low Risk students
  df_visual$rank_minority[i] <- signif(sum((df_rank$Minority)*(df_rank$HS_Percentile_Rank))/sum(df_rank$Minority), 3)
  #HS rank of minority Low Risk Students who graduate ontime
  df_visual$rank_minority_ontime[i] <- signif(sum((df_rank$Minority)*(df_rank$Graduated_in_4_Yrs)*(df_rank$Minority)*(df_rank$HS_Percentile_Rank))/sum((df_rank$Minority)*(df_rank$Graduated_in_4_Yrs)), 3)
  
  df_visual$num_minority_who_graduated_ontime[i] <- sum((df_year$Minority)*(df_year$Graduated_in_4_Yrs))
  df_visual$num_minority_who_graduated[i] <- sum((df_year$Minority)*(df_year$Graduated_in_6_Yrs))
  df_visual$num_minority[i] <- sum(df_year$Minority)
  
  
  
  #Graduated_within_4_years
  df_visual$four_yr_graduation_num[i]<-sum(df_year$Graduated_within_4_years)
  df_visual$four_yr_graduation_percent[i]<-signif(100*sum(df_year$Graduated_within_4_years)/df_visual$num_students[i], 3)
  
  #Graduated_within_6_years
  df_visual$six_yr_graduation_num[i]<-sum(df_year$Graduated_within_6_years)
  df_visual$six_yr_graduation_percent[i]<-signif(100*sum(df_year$Graduated_within_6_years)/df_visual$num_students[i], 3)
  
  
  #Finished_program_at_CU
  df_visual$finished_in_CU_num[i]<-sum(df_year$Finished_program_at_CU)
  df_visual$finished_in_CU_percent[i]<-signif(100*sum(df_year$Finished_program_at_CU)/df_visual$num_students[i], 3)
  
  #Entered_in_STEM
  df_visual$STEM_entry_num[i]<-sum(df_year$Entered_in_STEM)
  df_visual$STEM_entry_percent[i]<-signif(100*sum(df_year$Entered_in_STEM)/df_visual$num_students[i], 3)
  
  #Retained_STEM_in_year2
  df_visual$STEM_retention_in_2yr_num[i]<-sum(df_year$Retained_STEM_in_year2)
  df_visual$STEM_retention_in_2yr_percent[i]<-signif(100*sum(df_year$Retained_STEM_in_year2)/df_visual$STEM_entry_num[i], 3)
  
  
  #Graduated_in_STEM
  df_visual$STEM_graduates_num[i]<-sum(df_year$Graduated_in_STEM)
  df_visual$STEM_graduates_percent[i]<-signif(100*sum(df_year$Graduated_in_STEM)/df_visual$num_students[i], 3)
  
  #Entered_in_STEM_Grad_in_STEM
  df_visual$STEM_in_STEM_out_num[i]<-sum(df_year$Entered_in_STEM_Grad_in_STEM)
  df_visual$STEM_in_STEM_out_percent[i]<-signif(100*sum(df_year$Entered_in_STEM_Grad_in_STEM)/df_visual$STEM_entry_num[i], 3)
  
}

#NOTE: we subtract a number in order to zoom in on the graph
#=========================================Beginning of Plots========================================================




#============================================================================================================================
# Visualize the control and treatment data for SAT Verbal score
#============================================================================================================================
# 1. distribution of minority and majority math scores                                  
png("LR_ENG_SATmath_boxplot.png",width=780,height=880)
min_SAT = ggplot(df_min_SAT, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Minority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT = ggplot(df_maj_SAT, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Majority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group = ggplot(df_min_SAT, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Math Scores", title = "4yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group = ggplot(df_maj_SAT, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(y = "SAT Math Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Math Scores", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(y = "SAT Math Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT, maj_SAT, table_min_maj_num, min_SAT_group, maj_SAT_group, table_prop, min_SAT_group_6y,
             maj_SAT_group_6y, table_6y, ncol=3,nrow=3)
dev.off()

# 2. distribution of minority and majority verbal scores 
png("LR_ENG_SATverbal_boxplot.png",width=780,height=880)
min_SAT = ggplot(df_min_SAT, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Minority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT = ggplot(df_maj_SAT, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Majority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group = ggplot(df_min_SAT, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Verbal Scores", title = "4yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group = ggplot(df_maj_SAT, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Verbal Scores", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT, maj_SAT, table_min_maj_num, min_SAT_group, maj_SAT_group, table_prop, 
             min_SAT_group_6y, maj_SAT_group_6y, table_6y, ncol=3,nrow=3)
dev.off()

# 3. distribution of minority and majority math scores of those who graduated on time
png("LR_grad_ENG_SATmath_boxplot.png",width=780,height=880)
min_SAT_ot = ggplot(df_min_SAT_ot, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Minority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT_ot = ggplot(df_maj_SAT_ot, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Majority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group_ot = ggplot(df_min_SAT_ot, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Math Scores", title = "4yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_ot = ggplot(df_maj_SAT_ot, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Math Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Math Scores", title = "6yyr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Math Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT_ot, maj_SAT_ot, table_min_maj_num, min_SAT_group, maj_SAT_group, table_prop, min_SAT_group_6y, 
             maj_SAT_group_6y,table_6y, ncol=3,nrow=3)
dev.off()

# 4. distribution of minority and majority verbal scores of those who graduated on time
png("LR_grad_ENG_SATverbal_boxplot.png",width=780,height=880)
min_SAT_ot = ggplot(df_min_SAT_ot, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Minority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT_ot = ggplot(df_maj_SAT_ot, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Majority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group_ot = ggplot(df_min_SAT_ot, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "4yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_ot = ggplot(df_maj_SAT_ot, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT_ot, maj_SAT_ot, table_min_maj_num, min_SAT_group, maj_SAT_group, table_prop,min_SAT_group_6y,
             maj_SAT_group_6y, table_6y, ncol=3,nrow=3)
dev.off()







#============================================================================================================================
# Visualize the control and treatment data for SAT Verbal score
#============================================================================================================================
# 1. distribution of minority and majority math scores                                  
png("LR_ENG_SATmath_boxplot.png",width=780,height=880)
min_SAT = ggplot(df_min_SAT, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Minority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT = ggplot(df_maj_SAT, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Majority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group = ggplot(df_min_SAT, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Math Scores", title = "Control v Treatment\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group = ggplot(df_maj_SAT, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(y = "SAT Math Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Math Scores", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(y = "SAT Math Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(300,800))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT, maj_SAT, table_min_maj_num, min_SAT_group, maj_SAT_group, table_prop, min_SAT_group_6y,
             maj_SAT_group_6y, table_6y, ncol=3,nrow=3)
dev.off()

# 2. distribution of minority and majority verbal scores 
png("LR_ENG_SATverbal_boxplot.png",width=780,height=880)
min_SAT = ggplot(df_min_SAT, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Minority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT = ggplot(df_maj_SAT, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Majority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group = ggplot(df_min_SAT, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Verbal Scores", title = "Control v Treatment\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group = ggplot(df_maj_SAT, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Verbal Scores", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT, maj_SAT, table_min_maj_num, min_SAT_group, maj_SAT_group, table_prop, 
             min_SAT_group_6y, maj_SAT_group_6y, table_6y, ncol=3,nrow=3)
dev.off()

# 3. distribution of minority and majority math scores of those who graduated on time
png("LR_grad_ENG_SATmath_boxplot.png",width=780,height=880)
min_SAT_ot = ggplot(df_min_SAT_ot, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Minority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT_ot = ggplot(df_maj_SAT_ot, aes(x = Box, y = SAT_Math_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Math Scores", title = "Math SAT (Majority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group_ot = ggplot(df_min_SAT_ot, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Math Scores", title = "4yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_ot = ggplot(df_maj_SAT_ot, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Math Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="",y = "SAT Math Scores", title = "6yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_Math_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Math Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT_ot, maj_SAT_ot, table_min_maj_num, min_SAT_group_ot, maj_SAT_group_ot, table_prop, min_SAT_group_6y, 
             maj_SAT_group_6y,table_6y, ncol=3,nrow=3)
dev.off()

# 4. distribution of minority and majority verbal scores of those who graduated on time
png("LR_grad_ENG_SATverbal_boxplot.png",width=780,height=880)
min_SAT_ot = ggplot(df_min_SAT_ot, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Minority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_SAT_ot = ggplot(df_maj_SAT_ot, aes(x = Box, y = SAT_RW_Convert, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "SAT Verbal Scores", title = "Verbal SAT (Majority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_SAT_group_ot = ggplot(df_min_SAT_ot, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "4yr grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_ot = ggplot(df_maj_SAT_ot, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_SAT_group_6y = ggplot(df_min_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT verbal Scores", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_SAT_group_6y = ggplot(df_maj_SAT_6y, aes(x = Group, y = SAT_RW_Convert, group = Group)) +
  geom_boxplot() + labs(x="", y = "SAT Verbal Scores", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(250,850))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_SAT_ot, maj_SAT_ot, table_min_maj_num, min_SAT_group_ot, maj_SAT_group_ot, table_prop,min_SAT_group_6y,
             maj_SAT_group_6y, table_6y, ncol=3,nrow=3)
dev.off()







#============================================================================================================================
# Visualize the control and treatment data for HS rank
#============================================================================================================================
# 5. distribution of minority and majority HS ranks                                  
png("LR_ENG_HS_rank.png",width=780,height=560)
min_rank = ggplot(df_min_rank, aes(x = Box, y = HS_Percentile_Rank, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "High school class rank", title = "HS rank (Minority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_rank = ggplot(df_maj_rank, aes(x = Box, y = HS_Percentile_Rank, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "High school class rank", title = "HS rank (Majority LR ENG)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_rank_group = ggplot(df_min_rank, aes(x = Group, y = HS_Percentile_Rank, group = Group)) +
  geom_boxplot() + labs(y = "High school class rank") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_rank_group = ggplot(df_maj_rank, aes(x = Group, y = HS_Percentile_Rank, group = Group)) +
  geom_boxplot() + labs(y = "High school class rank") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
grid.arrange(min_rank, maj_rank, table_min_maj_num, min_rank_group, maj_rank_group, ncol=3,nrow=2)
dev.off()

# 6. distribution of minority and majority HS ranks of those who graduated on time
png("LR_ENG_grad_HS_rank.png",width=780,height=880)
min_rank_ot = ggplot(df_min_rank_ot, aes(x = Box, y = HS_Percentile_Rank, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "High school class rank", title = "HS rank (Minority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
maj_rank_ot = ggplot(df_maj_rank_ot, aes(x = Box, y = HS_Percentile_Rank, group = Box)) +
  geom_boxplot() + labs(x = "Cohort years",y = "High school class rank", title = "HS rank (Majority LR ENG grad ot)\n") + theme_classic() +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1), limits= c(0.5,10.5), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10, angle=90),
        axis.text.y = element_text(color = "black", size=10),
        plot.title = element_text(hjust = 0.5))
min_rank_group_ot = ggplot(df_min_rank_ot, aes(x = Group, y = HS_Percentile_Rank, group = Group)) +
  geom_boxplot() + labs(y = "High school class rank", title = "4y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_rank_group_ot = ggplot(df_maj_rank_ot, aes(x = Group, y = HS_Percentile_Rank, group = Group)) +
  geom_boxplot() + labs(y = "High school class rank", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
min_rank_group_6y = ggplot(df_maj_rank_6y, aes(x = Group, y = HS_Percentile_Rank, group = Group)) +
  geom_boxplot() + labs(x="", y = "High school rank", title = "6y grad\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
maj_rank_group_6y = ggplot(df_maj_rank_6y, aes(x = Group, y = HS_Percentile_Rank, group = Group)) +
  geom_boxplot() + labs(x="", y = "High school rank", title = "\n") + theme_classic() + 
  scale_x_continuous(breaks = round(seq(1, 2, by = 1),1), limits= c(0.5,2.5), labels = c("Control","Treatment"))+
  scale_y_continuous(limits= c(0,100))+
  theme(axis.text.x = element_text(color = "black", size=10),
        axis.text.y = element_text(color = "black", size=10))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime, maj=df_visual$num_majority,
                                    maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime,maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6y = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y= df_visual$num_minority_who_graduated,
                           min_6yP= df_visual$per_minority_grad, maj_6yP= df_visual$per_majority_grad, maj_6y= df_visual$num_majority_who_graduated))
grid.arrange(min_rank_ot, maj_rank_ot, table_min_maj_num, min_rank_group_ot, maj_rank_group_ot,table_prop, min_rank_group_6y,
             maj_rank_group_6y, table_6y, ncol=3,nrow=3)
dev.off()


# 7. Proportion of Minority & Majority Low Risk students (and graduation proportions) Histogram
png("LR_min_maj_prop.png",width=780,height=880)
minority_rates <- data.frame(cbind(df_visual$year, df_visual$minority_per)) 
plot_minority <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = minority_rates[which(minority_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = minority_rates[which(minority_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of min in LR ENG") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=minority_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

majority_rates <- data.frame(cbind(df_visual$year, df_visual$majority_per)) 
plot_majority <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = majority_rates[which(majority_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = majority_rates[which(majority_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of maj in LR") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=majority_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

minority_ot_rates <- data.frame(cbind(df_visual$year, df_visual$per_minority_ontime)) 
plot_minority_ot <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = minority_ot_rates[which(minority_ot_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = minority_ot_rates[which(minority_ot_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of min in LR grad ot") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=minority_ot_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

majority_ot_rates <- data.frame(cbind(df_visual$year, df_visual$per_majority_ontime)) 
plot_majority_ot <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = majority_ot_rates[which(majority_ot_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = majority_ot_rates[which(majority_ot_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of maj in LR grad ot") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=majority_ot_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

minority_6yr_rates <- data.frame(cbind(df_visual$year, df_visual$per_minority_grad)) 
plot_minority_6yr <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = minority_6yr_rates[which(minority_6yr_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = minority_6yr_rates[which(minority_6yr_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of min in LR grad 6yr") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=minority_6yr_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

majority_6yr_rates <- data.frame(cbind(df_visual$year, df_visual$per_majority_grad)) 
plot_majority_6yr <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = majority_6yr_rates[which(majority_6yr_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = majority_6yr_rates[which(majority_6yr_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of maj in LR grad 6yr ") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=majority_6yr_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))
table_min_maj_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min=df_visual$num_minority,
                                    min_ot=df_visual$num_minority_who_graduated_ontime,maj=df_visual$num_majority, maj_ot=df_visual$num_majority_who_graduated_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_p=df_visual$minority_per ,
                             min_ot_p= df_visual$per_minority_ontime, maj_p=df_visual$majority_per, maj_ot_p= df_visual$per_majority_ontime))
table_6yr = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), min_6y =df_visual$num_minority_who_graduated,
                            min_6y_p= df_visual$per_minority_grad,  maj_6y=df_visual$num_majority_who_graduated, maj_6y_p= df_visual$per_majority_grad))


grid.arrange(plot_minority, plot_majority, table_min_maj_num, plot_minority_ot, plot_majority_ot, table_prop, plot_minority_6yr, plot_majority_6yr, table_6yr, ncol=3, nrow=3)

dev.off()


# Gender based plots
# 8. Proportion of Female & Male Low Risk students (and graduation proportions) Histogram
png("LR_f_m_prop.png",width=780,height=880)

gender_rates <- data.frame(cbind(df_visual$year, df_visual$male_per)) 
plot_gender <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = gender_rates[which(gender_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = gender_rates[which(gender_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of males in LR") +
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

f_gender_rates <- data.frame(cbind(df_visual$year, df_visual$female_per)) 
f_plot_gender <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = f_gender_rates[which(f_gender_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = f_gender_rates[which(f_gender_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of females in LR") +
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

gender_ot_rates <- data.frame(cbind(df_visual$year, df_visual$per_male_ontime)) 
plot_gender_ot <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = gender_ot_rates[which(gender_ot_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = gender_ot_rates[which(gender_ot_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of males in LR grad 4yrs") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=gender_ot_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

f_gender_ot_rates <- data.frame(cbind(df_visual$year, df_visual$per_female_ontime)) 
f_plot_gender_ot <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = f_gender_ot_rates[which(f_gender_ot_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = f_gender_ot_rates[which(f_gender_ot_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of females in LR grad 4yrs") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100))+
  geom_text(data=f_gender_ot_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

gender_grad_rates <- data.frame(cbind(df_visual$year, df_visual$per_male_grad)) 
plot_gender_grad <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = gender_grad_rates[which(gender_grad_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = gender_grad_rates[which(gender_grad_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of males in LR grad 6yrs") +
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

f_gender_grad_rates <- data.frame(cbind(df_visual$year, df_visual$per_female_grad)) 
f_plot_gender_grad <- ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = f_gender_grad_rates[which(f_gender_grad_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = f_gender_grad_rates[which(f_gender_grad_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="Prop of females in LR grad 6yrs") +
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

table_f_m_num = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), female=df_visual$num_females,
                                f_ot=df_visual$num_females_ontime,male=df_visual$num_males, m_ot=df_visual$num_males_ontime))
table_prop = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), f_p=df_visual$female_per ,
                             f_ot_p= df_visual$per_female_ontime, m_p=df_visual$male_per, m_ot_p= df_visual$per_male_ontime))
table_6yr = tableGrob(cbind(year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), f_6y =df_visual$num_females_grad,
                            f_6y_p= df_visual$per_female_grad,  m_6y=df_visual$num_males_grad, m_6y_p= df_visual$per_male_grad))

grid.arrange(f_plot_gender, plot_gender, table_f_m_num, f_plot_gender_ot, plot_gender_ot, table_prop, f_plot_gender_grad, plot_gender_grad, table_6yr, ncol=3, nrow=3) #this creates the 2x2 grid


dev.off()








