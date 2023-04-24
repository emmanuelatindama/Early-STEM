# ===================================================================
# The response variable is the duration for completion of the degree
# ===================================================================
library(plotrix)
library(ggplot2) 
library(gridExtra)
library(dplyr)

setwd("~/Desktop/EARLY STEM 2/Codes");

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
df_ENG <- df_ENG[which(df_ENG$Sex_female==1),]

#=========================Let's define all the relevant groups here!==========================
# Treatment group are the high-risk(Delayed) ENG students who took ES110 in their first semester from 2011 onward
df_treatment <- df_ENG[which(df_ENG$ES110_in_1st_semester==1 & df_ENG$PH131_in_1st_semester==0 & df_ENG$PH131_in_1st_year==1 &  df_ENG$Primary_Cohort>=2011 & df_ENG$Clarkson_School==0),]

# The early control group is the group of high-risk ENG students before 2011 who did not take ES110 in sem 1
df_control <-df_ENG[which(df_ENG$ES110_in_1st_semester==0 & df_ENG$PH131_in_1st_semester==1 & df_ENG$Primary_Cohort<=2010 & df_ENG$Clarkson_School==0),]


# All Q3 ENG students who did not take ES110. # Needed for proportion test
df_control_all <- df_control
total_treatment_sum<- nrow(df_treatment)# number of treatment students
total_control_sum<- nrow(df_control_all)# number of early control students


#===============================================================================================================================
# combine the control group in 2006-2010 with treatment group (2011-2015)
df <- rbind(df_control_all, df_treatment)

Primary_Cohort<-first_year:last_year # list of years 2006 to 2015
df_visual <- data.frame(matrix(ncol = 1, nrow = total_years))# create a dataframe to store data

colnames(df_visual)<-c("year") # name the first column as year
df_visual$year<-Primary_Cohort # enter years 2006 to 2011 in the 1st column, year
options(digits = 5) # displays 3 digits after the decimal point


for (i in 1:total_years){
  year<-2005+i
  # We use df, not df_ENG because df binds the control and treatment groups. This df is not the same as the original df
  df_year<-df[df$Primary_Cohort==year, ] # this is important step
  
  df_visual$num_students[i]<-nrow(df_year)
  
  #Graduated_within_4_years
  df_visual$four_yr_graduation_num[i]<-sum(df_year$Graduated_within_4_years)
  df_visual$four_yr_graduation_percent[i]<-signif(100*sum(df_year$Graduated_within_4_years)/df_visual$num_students[i], 3)
  
  #Graduated_within_6_years
  df_visual$six_yr_graduation_num[i]<-sum(df_year$Graduated_within_6_years)
  df_visual$six_yr_graduation_percent[i]<-signif(100*sum(df_year$Graduated_within_6_years)/df_visual$num_students[i], 3)
  
  #Persistence_at_least_4_years
  df_visual$four_year_persistence_num[i]<-sum(df_year$Persistence_at_least_4_years)
  df_visual$four_year_persistence_percent[i]<-signif(100*sum(df_year$Persistence_at_least_4_years)/df_visual$num_students[i], 3)
  
  #Persistence_at_least_6_years
  df_visual$six_year_persistence_num[i]<-sum(df_year$Persistence_at_least_6_years)
  df_visual$six_year_persistence_percent[i]<-signif(100*sum(df_year$Persistence_at_least_6_years)/df_visual$num_students[i], 3)
  
  #Finished_program_at_CU
  df_visual$finished_in_CU_num[i]<-sum(df_year$Finished_program_at_CU)
  df_visual$finished_in_CU_percent[i]<-signif(100*sum(df_year$Finished_program_at_CU)/df_visual$num_students[i], 3)
  
  #Never_finished_program_at_CU
  df_visual$never_finished_in_CU_after6yr_num[i]<-sum(df_year$Persistence_at_least_6_years-df_year$Finished_program_at_CU)
  df_visual$never_finished_in_CU_after6yr_percent[i]<-signif(100*sum(df_year$Persistence_at_least_6_years-df_year$Finished_program_at_CU)/df_visual$num_students[i], 3)
  
  #Entered_in_STEM
  df_visual$STEM_entry_num[i]<-sum(df_year$Entered_in_STEM)
  df_visual$STEM_entry_percent[i]<-signif(100*sum(df_year$Entered_in_STEM)/df_visual$num_students[i], 3)
  
  #Retained_STEM_in_year2
  df_visual$STEM_retention_in_2yr_num[i]<-sum(df_year$Retained_STEM_in_year2)
  df_visual$STEM_retention_in_2yr_percent[i]<-signif(100*sum(df_year$Retained_STEM_in_year2)/df_visual$STEM_entry_num[i], 3)
  
  #Retained_STEM_in_year3
  df_visual$STEM_retention_in_3yr_num[i]<-sum(df_year$Retained_STEM_in_year3)
  df_visual$STEM_retention_in_3yr_percent[i]<-signif(100*sum(df_year$Retained_STEM_in_year3)/df_visual$STEM_entry_num[i], 3)
  
  #Retained_STEM_in_year4
  df_visual$STEM_retention_in_4yr_num[i]<-sum(df_year$Retained_STEM_in_year4)
  df_visual$STEM_retention_in_4yr_percent[i]<-signif(100*sum(df_year$Retained_STEM_in_year4)/df_visual$STEM_entry_num[i], 3)
  
  #Graduated_in_STEM
  df_visual$STEM_graduates_num[i]<-sum(df_year$Graduated_in_STEM)
  df_visual$STEM_graduates_percent[i]<-signif(100*sum(df_year$Graduated_in_STEM)/df_visual$num_students[i], 3)
  
  #Entered_in_STEM_Grad_in_STEM
  df_visual$STEM_in_STEM_out_num[i]<-sum(df_year$Entered_in_STEM_Grad_in_STEM)
  df_visual$STEM_in_STEM_out_percent[i]<-signif(100*sum(df_year$Entered_in_STEM_Grad_in_STEM)/df_visual$STEM_entry_num[i], 3)
  
  #Graduated GPA
  df_visual$GPA[i]<-signif(mean(df_year$Degree_GPA,na.rm=TRUE),3)
  
  #Satisfactory grades in PH131 on 1st attempt
  df_visual$PH131_satisfactory_grade_num[i]<-nrow(df_year[which(df_year$PH131_grade=="S" ),])
  df_visual$num_students_in_PH131[i] <- nrow(df_year[which(df_year$PH131_1>=0),])
  df_visual$PH131_satisfactory_grade_percentage[i]<-signif(100*nrow(df_year[which(df_year$PH131_grade=="S"),])/df_visual$num_students_in_PH131[i],3)
  
  #Satisfactory grades in MA131 on 1st attempt
  df_visual$MA131_satisfactory_grade_num[i]<-nrow(df_year[which(df_year$MA131_grade=="S"),])
  df_visual$num_students_in_MA131[i] <- nrow(df_year[which(df_year$MA131_1>=0),])
  df_visual$MA131_satisfactory_grade_percentage[i]<-signif(100*nrow(df_year[which(df_year$MA131_grade=="S"),])/df_visual$num_students_in_MA131[i],3)
  
  #Satisfactory grades in CM131 on 1st attempt
  df_visual$CM131_satisfactory_grade_num[i]<-nrow(df_year[which(df_year$CM131_grade=="S"),])
  df_visual$num_students_in_CM131[i] <- nrow(df_year[which(df_year$CM131_1>=0),])
  df_visual$CM131_satisfactory_grade_percentage[i]<-signif(100*nrow(df_year[which(df_year$CM131_grade=="S"),])/df_visual$num_students_in_CM131[i],3)
  
  #Satisfactory grades in PH132 on 1st attempt
  df_visual$PH132_satisfactory_grade_num[i]<-nrow(df_year[which(df_year$PH132_grade=="S"),])
  df_visual$num_students_in_PH132[i] <- nrow(df_year[which(df_year$PH132_1>=0),])
  df_visual$PH132_satisfactory_grade_percentage[i]<- signif(100*nrow(df_year[which(df_year$PH132_grade=="S"),])/df_visual$num_students_in_PH132[i],3)
  
  #Satisfactory grades in MA132 on 1st attempt
  df_visual$MA132_satisfactory_grade_num[i]<-nrow(df_year[which(df_year$MA132_grade=="S"),])
  df_visual$num_students_in_MA132[i] <- nrow(df_year[which(df_year$MA132_1>=0),])
  df_visual$MA132_satisfactory_grade_percentage[i]<-signif(100*nrow(df_year[which(df_year$MA132_grade=="S"),])/df_visual$num_students_in_MA132[i],3)
  
  #Satisfactory grades in CM132 on 1st attempt
  df_visual$CM132_satisfactory_grade_num[i]<-nrow(df_year[which(df_year$CM132_grade=="S"),])
  df_visual$num_students_in_CM132[i] <- nrow(df_year[which(df_year$CM132_1>=0),])
  df_visual$CM132_satisfactory_grade_percentage[i]<-signif(100*nrow(df_year[which(df_year$CM132_grade=="S"),])/df_visual$num_students_in_CM132[i],3)
}

#windowsFonts(Times = windowsFont("Times New Roman"))
jpeg("high risk female GPA.jpeg",width=500,height=550)
deg_GPA <- data.frame(cbind(df_visual$year, df_visual$GPA)) # a dataframe for STEM retention after 1yr
ggplot(mapping = aes(x = X1, y = X2)) +
  geom_bar(data = deg_GPA[which(deg_GPA$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = deg_GPA[which(deg_GPA$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "Percentage %", title = "GPA of Female High-Risk ENG students\n") +
  annotate(geom = "text", x = 2009.7, y = 4, label = "p", hjust = "left", family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 4, label = "= .754", hjust = "left", family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  geom_text(data=deg_GPA, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3.5, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )
dev.off()




#NOTE: we subtract a number in order to zoom in on the graph
#=========================================Beginning of Plots========================================================
#windowsFonts(Times = windowsFont("Times New Roman"))
jpeg("female retention_plots.jpeg",width=700,height=900)
yr2_retention <- data.frame(cbind(df_visual$year, df_visual$STEM_retention_in_2yr_percent)) # a dataframe for STEM retention after 1yr
yr2_plot <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = yr2_retention[which(yr2_retention$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = yr2_retention[which(yr2_retention$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "Percentage %", title = "First year STEM retention\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .068", hjust = "left", family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=yr2_retention, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3.5, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )



# range: 40-95
yr3_retention <- data.frame(cbind(df_visual$year, df_visual$STEM_retention_in_3yr_percent)) # a dataframe for STEM retention after 2yrs
yr3_plot <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = yr3_retention[which(yr3_retention$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = yr3_retention[which(yr3_retention$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "", title="Second year STEM retention\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .237", hjust = "left", family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=yr3_retention, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3.5, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour = "black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )


# range: 40-95
four_yr_grad_rates <- data.frame(cbind(df_visual$year, df_visual$four_yr_graduation_percent)) # a dataframe for the 4yr graduation rates
plot_4yr <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = four_yr_grad_rates[which(four_yr_grad_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = four_yr_grad_rates[which(four_yr_grad_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "Percentage %", title = "4-year graduation rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .374", hjust = "left", family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=four_yr_grad_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3.5, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour = "black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

# range: 40-95
#Par(mar=c(1,2,3,3))
six_yr_grad_rates <- data.frame(cbind(df_visual$year, df_visual$six_yr_graduation_percent)) # a dataframe for the 6yr graduation rates
plot_6yr <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = six_yr_grad_rates[which(six_yr_grad_rates$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = six_yr_grad_rates[which(six_yr_grad_rates$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "", y = "", title ="6-year graduation rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .083", hjust = "left", family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=six_yr_grad_rates, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=3.5, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=13.5, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )
grid.arrange(yr2_plot, yr3_plot, plot_4yr, plot_6yr, ncol=2, nrow=2) #this creates the 2x2 grid

dev.off()

#======================================End of Graduation Rate Plots========================================================================



jpeg("female success_rates.jpeg",width=1000,height=1400)

# range: 35-85
PH131_success <- data.frame(cbind(df_visual$year, df_visual$PH131_satisfactory_grade_percentage)) # a dataframe of PH131 success rate
plot_PH131 <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = PH131_success[which(PH131_success$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = PH131_success[which(PH131_success$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "Percentage %", title = "Physics I success rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", family="Times", size=5.0, fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .002**", hjust = "left", size=5.0, family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=PH131_success, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=5.0, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=17, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour = "black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

# range: 35-85
PH132_success <- data.frame(cbind(df_visual$year, df_visual$PH132_satisfactory_grade_percentage)) # a dataframe of PH132 success rate
plot_PH132 <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = PH132_success[which(PH132_success$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = PH132_success[which(PH132_success$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "", title = "Physics II success rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", size=5.0, family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .653", hjust = "left", size=5.0, family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=PH132_success, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=5.0, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=17, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour = "black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

# range: 35-85
MA131_success <- data.frame(cbind(df_visual$year, df_visual$MA131_satisfactory_grade_percentage)) # a dataframe of MA131 success rate
plot_MA131 <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = MA131_success[which(MA131_success$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = MA131_success[which(MA131_success$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "Percentage %", title  = "Calculus I success rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", size=5.0, family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .030*", hjust = "left", size=5.0, family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=MA131_success, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=5.0, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=17, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

# range: 35-85
MA132_success <- data.frame(cbind(df_visual$year, df_visual$MA132_satisfactory_grade_percentage)) # a dataframe of MA132 success rate
plot_MA132 <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = MA132_success[which(MA132_success$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = MA132_success[which(MA132_success$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "", title = "Calculus II success rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", size=5.0, family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .863", hjust = "left", size=5.0, family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=MA132_success, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=5.0, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=17, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

# range: 35-85
CM131_success <- data.frame(cbind(df_visual$year, df_visual$CM131_satisfactory_grade_percentage)) # a dataframe of CM131 success rate
plot_CM131 <- ggplot(mapping = aes(x = X1, y = X2-10)) +
  geom_bar(data = CM131_success[which(CM131_success$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = CM131_success[which(CM131_success$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "Percentage %", title = "Chemistry I success rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", size=5.0, family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .249", hjust = "left", size=5.0, family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=CM131_success, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=5.0, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=17, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

# range: 35-85
CM132_success <- data.frame(cbind(df_visual$year, df_visual$CM132_satisfactory_grade_percentage)) # a dataframe of CM132 success rate
plot_CM132 <- ggplot(mapping = aes(x = X1, y = X2-9.5)) +
  geom_bar(data = CM132_success[which(CM132_success$X1<=2010),], stat = "identity", width = 0.8,fill = "darkblue") +
  geom_bar(data = CM132_success[which(CM132_success$X1>2010),], stat = "identity", width = 0.8,fill = "darkgreen") +
  labs(x = "\n", y = "", title = "Chemistry II success rate\n") +
  annotate(geom = "text", x = 2009.7, y = 100, label = "p", hjust = "left", size=5.0, family="Times", fontface="italic") +
  annotate(geom = "text", x = 2010.0, y = 100, label = "= .880", hjust = "left", size=5.0, family="Times") +
  scale_x_discrete(limits=first_year:last_year) +
  scale_y_continuous(limits= c(0,100), labels = c("10", "35","60","85", "110"))+
  geom_text(data=CM132_success, aes(label = X2), position = position_dodge(width=0.5), vjust=-0.5, size=5.0, family="Times")+
  theme(panel.background = element_rect(fill = "white"),
        text=element_text(size=17, family="Times"),
        panel.border = element_rect(color="black", fill="NA", size=.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', color = "lightgray"),
        axis.text.x = element_text(angle = 90, colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5)
  )

grid.arrange(plot_PH131, plot_PH132, plot_MA131, plot_MA132, plot_CM131, plot_CM132, ncol=2,nrow=3) #this creates the 2 col grid

dev.off()
#======================================End of Success Plots========================================================================
























#PROPORTION Z TESTS#
#=============================================================================================================================================================
# Two proportions z test of treatment vs control_early for 4 year 
grad_4yr_treatment_sum<- sum(df_treatment$Graduated_in_4_Yrs)# number of treatment students who graduated in 4 years
grad_4yr_control_sum<- sum(df_control_all$Graduated_in_4_Yrs)# number of early control students who graduated in 4 years

x_4yr<-c(grad_4yr_treatment_sum, grad_4yr_control_sum) # this changes with quantity
n<-c(total_treatment_sum, total_control_sum) # this is the same for all proportions

# H_null: treatment_proportion = control_proportion
# H_a   : treatment_proportion > control_proportion
result_4yr_grad<-prop.test(x_4yr, n, alternative = "greater", correct = TRUE)
treatment_4yr_proportion <- grad_4yr_treatment_sum/total_treatment_sum
control_4yr_proportion <- grad_4yr_control_sum/total_control_sum

if(result_4yr_grad$p.value < 0.05){
  sprintf("Treatment group has a better 4yr graduation rate than the control group with p value %1.3f.", result_4yr_grad$p.value)
}else{
  sprintf("Treatment group DOES NOT have better 4yr graduation rate than the control group with p value %1.3f.", result_4yr_grad$p.value)
}
#=============================================================================================================================================================
# Two proportions z test of treatment vs control_early for 6 year 
grad_6yr_treatment_sum<- sum(df_treatment$Graduated_in_6_Yrs)# number of Q3 treatment students who graduated in 6 years
grad_6yr_control_sum<- sum(df_control_all$Graduated_in_6_Yrs)# number of Q3 early control students who graduated in 6 years

x_6yr<-c(grad_6yr_treatment_sum, grad_6yr_control_sum)

result_6yr_grad<-prop.test(x_6yr, n, alternative = "greater", correct = TRUE)
treatment_6yr_proportion <- grad_6yr_treatment_sum/total_treatment_sum
control_6yr_proportion <- grad_6yr_control_sum/total_control_sum

if(result_6yr_grad$p.value < 0.05){
  sprintf("Treatment group has a better 4yr graduation rate than the control group with p value %1.3f.", result_6yr_grad$p.value)
}else{
  sprintf("Treatment group DOES NOT have better 4yr graduation rate than the control group with p value %1.3f.", result_6yr_grad$p.value)
}
#=============================================================================================================================================================
# Two proportions z test of treatment vs control_early for first year STEM retention 
yr2_retention_treatment_sum<- sum(df_treatment$Retained_Clarkson_Yr2)# number of Q3 treatment students retained in yr2
yr2_retention_control_sum<- sum(df_control_all$Retained_Clarkson_Yr2)# number of Q3 early control students retained in yr2

x_yr2<-c(yr2_retention_treatment_sum, yr2_retention_control_sum)

result_yr2_retention<-prop.test(x_yr2, n, alternative = "greater", correct = TRUE)
treatment_yr2_retention_proportion <- yr2_retention_treatment_sum/total_treatment_sum
control_yr2_retention_proportion <- yr2_retention_control_sum/total_control_sum

if(result_yr2_retention$p.value < 0.05){
  sprintf("Treatment group has a better 2nd year STEM retention rate than the control group with p value %1.3f.", result_yr2_retention$p.value)
}else{
  sprintf("Treatment group DOES NOT have better 2nd year STEM retention rate than the control group with p value %1.3f.", result_yr2_retention$p.value)
}
#=============================================================================================================================================================
# Two proportions z test of treatment vs control_early for second year STEM retention 
yr3_retention_treatment_sum<- sum(df_treatment$Retained_Clarkson_Yr3)# number of Q3 treatment students retained in yr3
yr3_retention_control_sum<- sum(df_control_all$Retained_Clarkson_Yr3)# number of Q3 early control students retained in yr3

x_yr3<-c(yr3_retention_treatment_sum, yr3_retention_control_sum)

result_yr3_retention<-prop.test(x_yr3, n, alternative = "greater", correct = TRUE)
treatment_yr3_retention_proportion <- yr3_retention_treatment_sum/total_treatment_sum
control_yr3_retention_proportion <- yr3_retention_control_sum/total_control_sum

if(result_yr3_retention$p.value < 0.05){
  sprintf("Treatment group has a better 3rd year STEM retention rate than the control group with p value %1.3f.", result_yr3_retention$p.value)
}else{
  sprintf("Treatment group DOES NOT have better 3rd year STEM retention rate than the control group with p value %1.3f.", result_yr3_retention$p.value)
}
#=============================================================================================================================================================

#=============================================================================================================================================================
# Two proportions z test of treatment vs control_early for STEM graduation rate 
grad_in_stem_treatment_sum<- sum(df_treatment$Graduated_in_STEM)# number of treatment students who graduated in 4 years
grad_in_stem_control_sum<- sum(df_control_all$Graduated_in_STEM)# number of early control students who graduated in 4 years

x_stem_in_out<-c(grad_in_stem_treatment_sum, grad_in_stem_control_sum)

result_STEM_grad<-prop.test(x_stem_in_out, n, alternative = "greater", correct = TRUE)
treatment_stem_in_out_proportion <- grad_in_stem_treatment_sum/total_treatment_sum
control_stem_in_out_proportion <- grad_in_stem_control_sum/total_control_sum

if(result_STEM_grad$p.value < 0.05){
  sprintf("Treatment group has a better STEM in STEM out retention rate than the control group with p value %1.3f.", result_STEM_grad$p.value)
}else{
  sprintf("Treatment group DOES NOT have better STEM in STEM out retention rate than the control group with p value %1.3f.", result_STEM_grad$p.value)
}
#===========================================================================================================================================================










# Two proportions z test of treatment vs control_early for satisfactory PH131 grade 
n1 = c(sum(df_visual$num_students_in_PH131[6:10]), sum(df_visual$num_students_in_PH131[1:5]))
satisfactory_PH131_control_sum <- sum(df_visual$PH131_satisfactory_grade_num[1:5]) #num of control students with satisfactory PH131 grade
satisfactory_PH131_treatment_sum <- sum(df_visual$PH131_satisfactory_grade_num[6:10]) #num of treatment students with satisfactory PH131 grade

x_satisfactory_PH131_grade<-c(satisfactory_PH131_treatment_sum, satisfactory_PH131_control_sum)

result_PH131_grade<-prop.test(x_satisfactory_PH131_grade, n1, alternative = "greater", correct = TRUE)
treatment_PH131_grade_proportion <- satisfactory_PH131_treatment_sum/sum(df_visual$num_students_in_PH131[6:10])
control_PH131_grade_proportion <- satisfactory_PH131_control_sum/sum(df_visual$num_students_in_PH131[1:5])

if(result_PH131_grade$p.value < 0.05){
  sprintf("Treatment group has a more satisfactory PH131 grades than the control group with p value %1.3f.", result_PH131_grade$p.value)
}else{
  sprintf("Treatment group DOES NOT have more satisfactory PH131 grades than the control group with p value %1.3f.", result_PH131_grade$p.value)
}
#===========================================================================================================================================================

# Two proportions z test of treatment vs control_early for satisfactory MA131 grade 
n2 = c(sum(df_visual$num_students_in_MA131[6:10]), sum(df_visual$num_students_in_MA131[1:5]))
satisfactory_MA131_control_sum <- sum(df_visual$MA131_satisfactory_grade_num[1:5]) #num of control students with satisfactory MA131 grade
satisfactory_MA131_treatment_sum <- sum(df_visual$MA131_satisfactory_grade_num[6:10]) #num of treatment students with satisfactory MA131 grade

x_satisfactory_MA131_grade<-c(satisfactory_MA131_treatment_sum, satisfactory_MA131_control_sum)

result_MA131_grade<-prop.test(x_satisfactory_MA131_grade, n2, alternative = "greater", correct = TRUE)
treatment_MA131_grade_proportion <- satisfactory_MA131_treatment_sum/sum(df_visual$num_students_in_MA131[6:10])
control_MA131_grade_proportion <- satisfactory_MA131_control_sum/sum(df_visual$num_students_in_MA131[1:5])

if(result_MA131_grade$p.value < 0.05){
  sprintf("Treatment group has a more satisfactory MA131 grades than the control group with p value %1.3f.", result_MA131_grade$p.value)
}else{
  sprintf("Treatment group DOES NOT have more satisfactory MA131 grades than the control group with p value %1.3f.", result_MA131_grade$p.value)
}
#===========================================================================================================================================================

# Two proportions z test of treatment vs control_early for satisfactory CM131 grade 
n3 = c(sum(df_visual$num_students_in_CM131[6:10]), sum(df_visual$num_students_in_CM131[1:5]))
satisfactory_CM131_control_sum <- sum(df_visual$CM131_satisfactory_grade_num[1:5]) #num of control students with satisfactory CM131 grade
satisfactory_CM131_treatment_sum <- sum(df_visual$CM131_satisfactory_grade_num[6:10]) #num of treatment students with satisfactory CM131 grade

x_satisfactory_CM131_grade<-c(satisfactory_CM131_treatment_sum, satisfactory_CM131_control_sum)

result_CM131_grade<-prop.test(x_satisfactory_CM131_grade, n3, alternative = "greater", correct = TRUE)
treatment_CM131_grade_proportion <- satisfactory_CM131_treatment_sum/sum(df_visual$num_students_in_CM131[6:10])
control_CM131_grade_proportion <- satisfactory_CM131_control_sum/sum(df_visual$num_students_in_CM131[1:5])

if(result_CM131_grade$p.value < 0.05){
  sprintf("Treatment group has a more satisfactory CM131 grades than the control group with p value %1.3f.", result_CM131_grade$p.value)
}else{
  sprintf("Treatment group DOES NOT have more satisfactory CM131 grades than the control group with p value %1.3f.", result_CM131_grade$p.value)
}
#===========================================================================================================================================================

# Two proportions z test of treatment vs control_early for satisfactory PH132 grade 
n4 = c(sum(df_visual$num_students_in_PH132[6:10]), sum(df_visual$num_students_in_PH132[1:5]))
satisfactory_PH132_control_sum <- sum(df_visual$PH132_satisfactory_grade_num[1:5]) #num of control students with satisfactory PH132 grade
satisfactory_PH132_treatment_sum <- sum(df_visual$PH132_satisfactory_grade_num[6:10]) #num of treatment students with satisfactory PH132 grade

x_satisfactory_PH132_grade<-c(satisfactory_PH132_treatment_sum, satisfactory_PH132_control_sum)

result_PH132_grade<-prop.test(x_satisfactory_PH132_grade, n4, alternative = "greater", correct = TRUE)
treatment_PH132_grade_proportion <- satisfactory_PH132_treatment_sum/sum(df_visual$num_students_in_PH132[6:10])
control_PH132_grade_proportion <- satisfactory_PH132_control_sum/sum(df_visual$num_students_in_PH132[1:5])

if(result_PH132_grade$p.value < 0.05){
  sprintf("Treatment group has a more satisfactory PH132 grades than the control group with p value %1.3f.", result_PH132_grade$p.value)
}else{
  sprintf("Treatment group DOES NOT have more satisfactory PH132 grades than the control group with p value %1.3f.", result_PH132_grade$p.value)
}
#===========================================================================================================================================================

# Two proportions z test of treatment vs control_early for satisfactory MA132 grade 
n5 = c(sum(df_visual$num_students_in_MA132[6:10]), sum(df_visual$num_students_in_MA132[1:5]))
satisfactory_MA132_control_sum <- sum(df_visual$MA132_satisfactory_grade_num[1:5]) #num of control students with satisfactory MA132 grade
satisfactory_MA132_treatment_sum <- sum(df_visual$MA132_satisfactory_grade_num[6:10]) #num of treatment students with satisfactory MA132 grade

x_satisfactory_MA132_grade<-c(satisfactory_MA132_treatment_sum, satisfactory_MA132_control_sum)

result_MA132_grade<-prop.test(x_satisfactory_MA132_grade, n5, alternative = "greater", correct = TRUE)
treatment_MA132_grade_proportion <- satisfactory_MA132_treatment_sum/sum(df_visual$num_students_in_MA132[6:10])
control_MA132_grade_proportion <- satisfactory_MA132_control_sum/sum(df_visual$num_students_in_MA132[1:5])

if(result_MA132_grade$p.value < 0.05){
  sprintf("Treatment group has a more satisfactory MA132 grades than the control group with p value %1.3f.", result_MA132_grade$p.value)
}else{
  sprintf("Treatment group DOES NOT have more satisfactory MA132 grades than the control group with p value %1.3f.", result_MA132_grade$p.value)
}
#===========================================================================================================================================================

# Two proportions z test of treatment vs control_early for satisfactory CM132 grade 
n6 = c(sum(df_visual$num_students_in_CM132[6:10]), sum(df_visual$num_students_in_CM132[1:5]))
satisfactory_CM132_control_sum <- sum(df_visual$CM132_satisfactory_grade_num[1:5]) #num of control students with satisfactory CM132 grade
satisfactory_CM132_treatment_sum <- sum(df_visual$CM132_satisfactory_grade_num[6:10]) #num of treatment students with satisfactory CM132 grade

x_satisfactory_CM132_grade<-c(satisfactory_CM132_treatment_sum, satisfactory_CM132_control_sum)

result_CM132_grade<-prop.test(x_satisfactory_CM132_grade, n6, alternative = "greater", correct = TRUE)
treatment_CM132_grade_proportion <- satisfactory_CM132_treatment_sum/sum(df_visual$num_students_in_CM132[6:10])
control_CM132_grade_proportion <- satisfactory_CM132_control_sum/sum(df_visual$num_students_in_CM132[1:5])

if(result_CM132_grade$p.value < 0.05){
  sprintf("Treatment group has a more satisfactory CM132 grades than the control group with p value %1.3f.", result_CM132_grade$p.value)
}else{
  sprintf("Treatment group DOES NOT have more satisfactory CM132 grades than the control group with p value %1.3f.", result_CM132_grade$p.value)
}







#============================================================================================================================
# Visualize the control and treatment data
#============================================================================================================================
#windowsFonts(Times = windowsFont("Times New Roman"))
#pdf("boxplots",width=10,height=10)

boxplot(df_control_all$SATMath, df_treatment$SATMath, names=c("2006-2010","2011-2015"),
        ylab="SAT Math score", family="Times",cex.lab = 1.5,cex.axis=1.3, col=c("white", "white"))
boxplot(df_control_all$SATVerb, df_treatment$SATVerb, names=c("2006-2010","2011-2015"),
        ylab="SAT Verbal score", family="Times",cex.lab = 1.5,cex.axis=1.3, col=c("white", "white"))
boxplot(df_control_all$Math_Score, df_treatment$Math_Score, names=c("2006-2010","2011-2015"),
        ylab="Math diagnostic score", family="Times",cex.lab = 1.5,cex.axis=1.3, col=c("white", "white"))
boxplot(df_control_all$Physics_Score, df_treatment$Physics_Score, names=c("2006-2010","2011-2015"),
        ylab="Physics diagnostic score", family="Times",cex.lab = 1.5,cex.axis=1.3, col=c("white", "white"))
