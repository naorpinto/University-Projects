#-----------------------------------------Alternative test--------------------------------------------------------------
## For current 
filePath1<-choose.files()
data1 <- read.csv(filePath1,header=TRUE)

## For Alter 1
filePath2<-choose.files()
data2 <- read.csv(filePath2,header=TRUE)

## For Alter 2
filePath3<-choose.files()
data3 <- read.csv(filePath3,header=TRUE)


conf.lvl <- 1-(0.1)/9
#paired t-test

#Comparing alternatives by performing a paired t test

# We will choose the best alternative depending on the test results

#------------------------------------------measure 1--------------------------------------------------------------------
Q_MDA_Kayam_VS_alt1<- t.test(x= data1$kayam_measure1,y=data2$alt1_measure1, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(Q_MDA_Kayam_VS_alt1)

Q_MDA_Kayam_VS_alt2<- t.test(x= data1$kayam_measure1,y= data3$alt2_measure1, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(Q_MDA_Kayam_VS_alt2)

Q_MDA_alt1_VS_alt2<- t.test(x= data2$alt1_measure1,y=data3$alt2_measure1, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(Q_MDA_alt1_VS_alt2)
#------------------------------------------measure 2--------------------------------------------------------------------

wt_HMO_Kayam_VS_alt1<- t.test(x= data1$kayam_measure2,y=data2$alt1_measure2, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(wt_HMO_Kayam_VS_alt1)

wt_HMO_Kayam_VS_alt2<- t.test(x= data1$kayam_measure2,y=data3$alt2_measure2, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(wt_HMO_Kayam_VS_alt2)

wt_HMO_alt1_VS_alt2<- t.test(x= data2$alt1_measure2,y=data3$alt2_measure2, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(wt_HMO_alt1_VS_alt2)
#------------------------------------------measure 3--------------------------------------------------------------------
Q_LabC_Kayam_VS_alt1<- t.test(x= data1$kayam_measure3,y=data2$alt1_measure3, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(Q_LabC_Kayam_VS_alt1)

Q_LabC_Kayam_VS_alt2<- t.test(x= data1$kayam_measure3,y=data3$alt2_measure3, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(Q_LabC_Kayam_VS_alt2)

Q_LabC_alt1_VS_alt2<- t.test(x= data2$alt1_measure3,y=data3$alt2_measure3, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=conf.lvl)
print(Q_LabC_alt1_VS_alt2)
#-------------------------------------------------------------------------------------------------------------------------
