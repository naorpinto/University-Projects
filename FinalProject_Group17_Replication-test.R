##-----------------------------------------  Part B------------------------------------------------

#---------------------------------------
n0 <- 15
n <- n0
gamma <- 0.12
alpha_total <- 0.1
alpha_i <- alpha_total/3
t <- qt(1-(alpha_i)/2,n0-1)
gamma_tag <-  (gamma)/(1+gamma)

calc_relative_accuracy <-  function(mean,sd){ 
  (t*sd/sqrt(n))/mean
}

number_of_replications <- function(relative_accuracy){
  n*(relative_accuracy/gamma_tag)^2
}
#-----------------------------------------------------------------------------------------------------------------
#read files
## For current 
filePath1<-choose.files()
data1 <- read.csv(filePath1,header=TRUE)

## For Alter 1
filePath2<-choose.files()
data2 <- read.csv(filePath2,header=TRUE)

## For Alter 2
filePath3<-choose.files()
data3 <- read.csv(filePath3,header=TRUE)

#mean and sd for kayam measures
Mean1_kayam_MDA_Q <- mean(data1$kayam_measure1)
sd1_kayam_MDA_Q  <- sd(data1$kayam_measure1)
Mean2_kayam_WT <- mean(data1$kayam_measure2)
sd2_kayam_wt <- sd(data1$kayam_measure2)
Mean3_kayam_LabC_Q  <- mean(data1$kayam_measure3)
sd3_kayam_LabC_Q <- sd(data1$kayam_measure3)

#mean and sd for alt1 measures
Mean1_alternative1_MDA_Q <- mean(data2$alt1_measure1)
sd1_alternative1_MDA_Q <- sd(data2$alt1_measure1)
Mean2_alternative1_WT <- mean(data2$alt1_measure2)
sd2_alternative1_WT <- sd(data2$alt1_measure2)
Mean3_alternative1_LabC_Q <- mean(data2$alt1_measure3)
sd3_alternative1_LabC_Q  <- sd(data2$alt1_measure3)

#mean and sd for alt2 measures
Mean1_alternative2_MDA_Q <- mean(data3$alt2_measure1)
sd1_alternative2_MDA_Q <- sd(data3$alt2_measure1)
Mean2_alternative2_WT <- mean(data3$alt2_measure2)
sd2_alternative2_WT<- sd(data3$alt2_measure2)
Mean3_alternative2_LabC_Q <- mean(data3$alt2_measure3)
sd3_alternative2_LabC_Q<- sd(data3$alt2_measure3)
#----------------------------------------------------------------------------------------------------------------------------------------
#Calculating the various measures and examining them according to relative accuracy

##first measure mda
c1 <- calc_relative_accuracy(Mean1_kayam_MDA_Q,sd1_kayam_MDA_Q)%>%print()
c2 <- calc_relative_accuracy(Mean1_alternative1_MDA_Q,sd1_alternative1_MDA_Q)%>%print()
c3 <- calc_relative_accuracy(Mean1_alternative2_MDA_Q,sd1_alternative2_MDA_Q)%>%print()

##second measure- avg waiting time HMO
c4 <- calc_relative_accuracy(Mean2_kayam_WT,sd2_kayam_wt)%>%print()
c5 <- calc_relative_accuracy(Mean2_alternative1_WT,sd2_alternative1_WT)%>%print()
c6 <- calc_relative_accuracy(Mean2_alternative2_WT,sd2_alternative2_WT)%>%print()

##thired measure- avg Queue at Lab C
c7 <- calc_relative_accuracy(Mean3_kayam_LabC_Q,sd3_kayam_LabC_Q)%>%print()
c8 <- calc_relative_accuracy(Mean3_alternative1_LabC_Q,sd3_alternative1_LabC_Q)%>%print()
c9 <- calc_relative_accuracy(Mean3_alternative2_LabC_Q,sd3_alternative2_LabC_Q)%>%print()


#Calculate n according to the coarser approximation method->When the relative accuracy is not valid
number_of_replications(c1)%>%print()
number_of_replications(c2)%>%print()
number_of_replications(c3)%>%print()
number_of_replications(c4)%>%print()
number_of_replications(c5)%>%print()
number_of_replications(c6)%>%print()
number_of_replications(c7)%>%print()
number_of_replications(c8)%>%print()
number_of_replications(c9)%>%print()

