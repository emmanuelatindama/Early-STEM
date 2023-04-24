### This is a modified form of Rob's semesterGrades code to generate our own sunflower plot

setwd("~/Desktop/Early STEM submission_data/studies");
rm(list=ls(all=TRUE)) 
library(descr)
#library(doBy)
library(plotrix)
library(ggplot2)
library(mltools)


#************************************************************************************
#change the class variable to reflect the class, i.e. PH131, CM132 etc.
#to find a list of the possible classes, run the script once, then type
#into the buffer: levels(grades$class). It will output the possible classes
class <- paste("PH131")
#change the cohort group (2011, 2012 or Historical)
cogroup <- paste("Historical")
#change the semester
semes <- 1
#end changable variables
xaxis<- 0.5
yaxis<- 0.65
wedge_xcoord<- 0
wedge_ycoord<- 0.7


#************************************************************************************
#read in files
grades <- read.csv(file="files/CU_IRQ_RAMSDELL_Grades_Sept_2021_EA_PVA_MR.csv", header=TRUE, sep=",")      # read grades
groupss <- read.csv(file="files/surveyListAll_EA_PVA_MR.csv", header=TRUE, sep=",")              # read surveylistAll
#groupss <- groupss[which(groupss$math!="-"),]                 # drop students with no math score in surveyList all
#groupss <- groupss[which(groupss$physics!="-"),]              # drop students with no physics score in surveyList all
write.table(groupss,row.names=FALSE,file="tmp.csv")           # this new group is saved into a csv file called tmp
groups <- read.csv(file="tmp.csv",header=TRUE,sep=" ")        # read tmp as groups  ## not in use at the moment


#separate the cohort group, then the class
clas <- grades[ which(grades$cohortGroup == cogroup),]    # select the students in grades.csv under cohortGroup='Historical'
cla <- clas[ which(clas$class == class),]                 # then select only students whose class = 'PH131' 


#separate the semester
cla <- merge(cla,groups, by="studentNum")        # merging survey_list_all and cohort data
#cla <- cla[which(cla$prog=="ENG"),]
cla <- cla[ which(cla$Student_with_MP_score == 1),]     # select students with MP scores
cla <- cla[ which(cla$PH131_fall_cohort_year == 1),]    # select students who took physics1 in fall of cohort year
#cla <- cla[ which(cla$CM131_fall_cohort_year == 1),]    # select students who took chem1 in fall of cohort year
#cla <- cla[ which(cla$MA131_fall_cohort_year == 1),]    # select students who took math1 in fall of cohort year

sem1 <- cla[ which(cla$semester == semes),] #select Ph131 students from grades.csv who are in their 1st semester
#separate the semester into pass and fail, and grades
s1sg <- sem1[ which(sem1$passFail == 'S'),]          #select Ph131 students from grades.csv who are in their 1st semester with satisfactory grade
s1ug <- sem1[ which(sem1$passFail == "U"),]          #select Ph131 students from grades.csv who are in their 1st semester with unsatisfactory grade

#math
s1sg$math <- s1sg$math+.01
s1ug$math <- s1ug$math-.01
#physics
s1sg$physics <- s1sg$physics+.01
s1ug$physics <- s1ug$physics-.01


#naming things for titles and such
title <- paste(cogroup,"Cohort",class,"Grades by MP Score,Semester",semes)
rocfilename <- paste("semesterGrades/",cogroup,"/Semester",semes,cogroup,class,"Grades_roc.pdf",sep="")
sunfilename <- paste("semesterGrades/",cogroup,"/Semester",semes,cogroup,class,"Grades_sun.pdf",sep="")
### Sunflower plot at the bottom of the page









#################################################### ROC Curve ################################################
combined <- merge(sem1, groups, by="studentNum")

num_Ugrade <- nrow(combined[which(combined$passFail=="U"),]) #number of students with unsatisfactory grade
num_Sgrade <- nrow(combined[which(combined$passFail=="S"),]) #number of students with satisfactory grade


False_Positive_Rate<- list()
True_Positive_Rate<- list()
i<-1
step = 0.02
for (m_cutoff in seq(0.02, 1, by=step)) {
  for (p_cutoff in seq(0.02, 1, by=step)){
    
    wedge_set <- combined[which(combined$math.x<m_cutoff),]
    wedge_set <- wedge_set[which(wedge_set$physics.x>=p_cutoff),]
    wedge_set[35] <- (p_cutoff - wedge_set$physics.x)/(m_cutoff - wedge_set$math.x)
    wedge_slope <- (p_cutoff - wedge_ycoord)/(m_cutoff - wedge_xcoord)
    lower_wedge_set <- wedge_set[which(wedge_set$V35>wedge_slope),]
    num_lower_wedge_set <- nrow(lower_wedge_set)
    num_lower_wedge_set_and_Sgrade = nrow(lower_wedge_set[which(lower_wedge_set$passFail=="S"),])
    num_lower_wedge_set_and_Ugrade = nrow(lower_wedge_set[which(lower_wedge_set$passFail=="U"),])
    
    
    quarter3 <- combined[which(combined$physics.x<p_cutoff),]
    quarter3 <- quarter3[which(quarter3$math.x<m_cutoff),]
    quarter3[35] <- (p_cutoff - quarter3$physics.x)/(m_cutoff - quarter3$math.x) #defining useless column
    
    high_risk_set <- rbind(quarter3,lower_wedge_set)
    high_risk_and_Sgrade <- high_risk_set[which(high_risk_set$passFail=="S"),]
    high_risk_and_Ugrade <- high_risk_set[which(high_risk_set$passFail=="U"),]
    
    num_high_risk <- nrow(high_risk_set)
    num_high_risk_and_Sgrade = nrow(high_risk_and_Sgrade)
    num_high_risk_and_Ugrade = nrow(high_risk_and_Ugrade)
    
    
    True_Positive_Rate[i] = (num_high_risk_and_Ugrade)/num_Ugrade
    False_Positive_Rate[i] = (num_high_risk_and_Sgrade)/num_Sgrade
    i <- i+1
  }
}
#### Plotting ####

#windowsFonts(Times = windowsFont("Times New Roman"))
pdf(rocfilename,width=10,height=10)
plot(False_Positive_Rate, True_Positive_Rate, xlab=" ", ylab=" ",cex.axis=1.3)


par(new = TRUE)

plot(False_Positive_Rate,True_Positive_Rate,pch=20,family="Times", xlab="False positive rate",ylab="\nTrue positive rate",cex.lab = 1.5,cex.axis=1.3, axes = FALSE)
segments(x0 = 0 , y0 = 0, x1 = 1, y1 = 1)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
legend(.68,.0, legend = c("Math cutoff=.65, Physics cutoff=.5"), pch=19, col = c("red"))
legend(.4,.0, legend = c("Random classifier line"),lwd = 1, col = c("black"))



par(new = TRUE)
math_cutoff<-yaxis #.65
physics_cutoff <-xaxis #.5

num_combined <- nrow(combined)

# slope of the wedge line = (0.5-0.7)/(0.65-0). So we identify points below the line
wedge_set <- combined[which(combined$math.x<math_cutoff),]
wedge_set <- wedge_set[which(wedge_set$physics.x>=physics_cutoff),]
wedge_set[35] <- (physics_cutoff - wedge_set$physics.x)/(math_cutoff - wedge_set$math.x) 
wedge_slope <- (physics_cutoff - wedge_ycoord)/(math_cutoff - wedge_xcoord)
lower_wedge_set <- wedge_set[which(wedge_set$V35>wedge_slope),]
num_lower_wedge_set <- nrow(lower_wedge_set)
num_lower_wedge_set_and_Sgrade = nrow(lower_wedge_set[which(lower_wedge_set$passFail=="S"),])
num_lower_wedge_set_and_Ugrade = nrow(lower_wedge_set[which(lower_wedge_set$passFail=="U"),])


quarter3 <- combined[which(combined$math.x<math_cutoff),]
quarter3 <- quarter3[which(quarter3$physics.x<physics_cutoff),]
quarter3[35] <- (physics_cutoff - quarter3$physics.x)/(math_cutoff - quarter3$math.x) #defining useless column
num_quarter3 <- nrow(quarter3)

high_risk_set <- rbind(quarter3,lower_wedge_set)
high_risk_and_Sgrade <- high_risk_set[which(high_risk_set$passFail=="S"),]
high_risk_and_Ugrade <- high_risk_set[which(high_risk_set$passFail=="U"),]

num_high_risk <- nrow(high_risk_set)
num_high_risk_and_Sgrade = nrow(high_risk_and_Sgrade)
num_high_risk_and_Ugrade = nrow(high_risk_and_Ugrade)

quarter1and4 <- combined[which(combined$math.x>=math_cutoff),]
upper_wedge_set <- wedge_set[which(wedge_set$V35<=wedge_slope),]
quarter1and4[35] <- (physics_cutoff - quarter1and4$physics.x)/(math_cutoff - quarter1and4$math.x) #defining useless column
med_low_set <- rbind(quarter1and4,upper_wedge_set)
med_low_and_Sgrade <- med_low_set[which(med_low_set$passFail=="S"),]
med_low_and_Ugrade <- med_low_set[which(med_low_set$passFail=="U"),]

num_med_low_risk <- nrow(med_low_set)
num_med_low_Ugrade <- nrow(med_low_and_Ugrade)
num_med_low_Sgrade <- nrow(med_low_and_Sgrade)

low_risk <- combined[which(combined$math.x>=math_cutoff),]
low_risk <- low_risk[which(low_risk$physics.x>=physics_cutoff),]
low_risk_and_Sgrade <- low_risk[which(low_risk$passFail=="S"),]
low_risk_and_Ugrade <- low_risk[which(low_risk$passFail=="U"),]
num_low_Sgrade <- nrow(low_risk_and_Sgrade)
num_low_Ugrade <- nrow(low_risk_and_Ugrade)
num_med_Sgrade <- num_med_low_Sgrade - num_low_Sgrade
num_med_Ugrade <- num_med_low_Ugrade - num_low_Ugrade


TPR = num_high_risk_and_Ugrade/num_Ugrade
FNR = num_med_low_Ugrade/num_Ugrade
TNR = num_med_low_Sgrade/num_Sgrade
FPR = (num_high_risk_and_Sgrade)/num_Sgrade
ACC = (num_med_low_Sgrade+num_high_risk_and_Ugrade)/(num_combined)
PPV = num_high_risk_and_Ugrade/num_high_risk
NPV = num_med_low_Sgrade/num_med_low_risk
MCC = mcc( preds = NULL, actuals = NULL, TP = num_high_risk_and_Ugrade,
           FP = num_high_risk_and_Sgrade,
           TN = num_med_low_Sgrade, FN = num_med_low_Ugrade)

paste("the class is", class)
paste("high U=", num_high_risk_and_Ugrade, "high S=", num_high_risk_and_Sgrade)
paste("med+low U=",num_med_Ugrade,"+",num_low_Ugrade,"=",num_med_low_Ugrade)
paste( "med+low S=",num_med_Sgrade,"+",num_low_Sgrade,"=",num_med_low_Sgrade)
paste("TPR=",round(TPR,3)," TNR=",round(TNR,3), " FPR =",round(FPR,3), " FNR =",round(FNR,3))
paste("PPV =",round(PPV,3), "NPV =",round(NPV,3), "ACC =",round(ACC,3), "MCC =",round(MCC,3))



plot(FPR, TPR, pch=20,
     bg = "red",   # Fill color
     col = "red", # Border color
     cex = 3,      # Symbol size
     family="Times",
     lwd = 3, axes=FALSE, ylim=c(0, 1),xlim=c(0, 1),xlab="", ylab="",cex.axis=1.3)

quad1 = combined[which(combined$math.x>=math_cutoff),]
quad4 = nrow(quad1[which(quad1$physics.x<physics_cutoff),]) #computing for the 4th quadrant

quad1 = nrow(quad1[which(quad1$physics.x>=physics_cutoff),])
quad1_percent = quad1*100/nrow(combined)

quad3 = nrow(quarter3)
quad3_percent = quad3*100/nrow(combined)

quad4_percent = quad4*100/nrow(combined)

quad_wedge = num_lower_wedge_set
quad_wedge_percent = quad_wedge*100/nrow(combined)

quad2 = nrow(upper_wedge_set)
quad2_percent = quad2*100/nrow(combined)


dev.off()





##### plotting #####
pdf(sunfilename,width=10,height=10)
par(mar=c(5,6,4,1)+.1)
sunflowerplot(s1sg$math,s1sg$physics,family="Times", xlab="Mathematics diagnostic score",ylab="Physics diagnostic score",
              col="blue4",seg.col="blue4",xlim=c(0,1),ylim=c(0,1),size=1/16,seg.lwd=.5,cex=.5,cex.fact=1,cex.lab=1.8)
sunflowerplot(s1ug$math,s1ug$physics,col="red3",seg.col="red3",add=TRUE,size=1/16,seg.lwd=.5,cex=.5,cex.fact=1)

# Lines 98 - 100 draw the 3 line segments for dividing the plots and lines 101 - 107 give all the labels
abline(h=xaxis)
abline(v=yaxis)
segments(yaxis,xaxis,wedge_xcoord,wedge_ycoord)



total = paste("N=",nrow(combined),sep="")
mtext(total,side=3,adj=.5,col="black",cex=1.5)

text(0.02,0.01,"M-P-",cex=1.5,col="black")
q3 <- paste("n=",round(quad3,1),", ",round(quad3_percent,2),"%", sep="")
text(0.08,-0.02,q3,col="black",cex=1.5)

text(0.03,.55,"M-P-+",cex=1.5,col="black")
q_wedge <- paste("n=",round(quad_wedge,1),", ",round(quad_wedge_percent,2),"%", sep="")
text(0.08,.52,q_wedge,col="black",cex=1.5)

text(0.02,1.02,"M-P+",cex=1.5,col="black")
q2 <- paste("n=",round(quad2,1),", ",round(quad2_percent,2),"%", sep="")
text(0.07,0.99,q2,col="black",cex=1.5)

text(0.87,0.01,"M+P-",cex=1.5,col="black")
q4 <- paste("n=",round(quad4,1),", ",round(quad4_percent,2),"%", sep="")
text(0.93,-0.02,q4,col="black",cex=1.5)

text(0.87,1.02,"M+P+",cex=1.5,col="black")
q1 <- paste("n=",round(quad1,1),", ",round(quad1_percent,2),"%", sep="")
text(0.93,0.99,q1,col="black",cex=1.5)


dev.off()
