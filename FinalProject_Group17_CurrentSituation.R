
##----------------------------------------- 1.  all functions ------------------------------------------------


addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

#avg queue per resource
avgQueue <- function(time, queueLength, simTime){
  Lavg = 0;
  L = queueLength[1];
  Tnow = time[1];
  Llast = time[1];
  TL = 0;
  Tmax = simTime;
  if (length(time) == length(queueLength)){
    for (i in 2:length(time)){
      if(queueLength[i] != queueLength[i-1]){
        Tnow = time[i];
        TL = TL+L*(Tnow-Llast);
        L = queueLength[i];
        Llast = Tnow;
      }#if
    }#for
  }#end if
  TL=TL+L*(Tmax-Llast);
  Lavg = TL/Tmax;
  return (Lavg);
}#end func



trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

  

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------
simulationTime <- 48*60  # for the current situation and for the alt1, alt2 the simulation time is 120960 (84 days) with 30240 minutes of warming time(21 days)
warming_time <- 0        #30240 minutes of warming time(21 days) when the simulation Time is big the 35 days (on the report)
arriveVec <- c(seq(18*60,simulationTime,24*60))# Time to close the drive-in station

Full_time_Drivein<-schedule( c(0 ,6*60,18*60,21*60) , values = c(0,3,2,0), period = 24*60) ##Schedule for full time stations in mega drive in


HMO_Sch_N_S <-schedule(c(0,9*60,13*60,14*60), values = c(0,4,0,4),period = 24*60) ## HMO schedule for north and south

HMO_Sch_C <-schedule(c(0,9*60,13*60,14*60), values = c(0,7,0,7),period = 24*60) ##HMO schedule for center
  
##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

Covid19<- simmer("Covid19")%>%
  add_resource("N1_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% ## Stations for north drive ins
  add_resource("N2_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% 
  add_resource("N3_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% 
  
  add_resource("C1_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% ## Stations for center drive ins
  add_resource("C2_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% 
  add_resource("C3_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% 
  add_resource("C4_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% 
  
  add_resource("S1_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% ## Stations for south drive ins
  add_resource("S2_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>% 
  add_resource("S3_mini_drivein",capacity=Full_time_Drivein,queue_size=Inf)%>%
 
  add_resource("MDA",capacity=20,queue_size=Inf,preemptive = FALSE)%>% 
  
  add_resource("HMO_N",capacity=HMO_Sch_N_S,queue_size=Inf)%>% ## Stations for each HMO divided by areas
  add_resource("HMO_C",capacity=HMO_Sch_C,queue_size=Inf)%>%
  add_resource("HMO_S",capacity=HMO_Sch_N_S,queue_size=Inf)%>%
  
  add_resource("Lab_N",capacity=20,queue_size=Inf,preemptive = FALSE)%>% ## Stations for Labs divided by areas 
  add_resource("Lab_C",capacity=20,queue_size=Inf,preemptive = FALSE)%>%
  add_resource("Lab_S",capacity=20,queue_size=Inf,preemptive = FALSE)
  

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

  goHome <- trajectory("goHome")%>%  ##Leave trajectory
    set_attribute("positive", 0)  
  
  quarantine <- trajectory("quarantine")%>%
    set_attribute("positive", 1)%>%  
    deactivate(sources = "DriveIn_quarantine")%>%
    activate(sources = "DriveIn_quarantine")
    
  Lab_North_Traj <- trajectory("Lab_North_Traj")%>%
    addService("Lab_N",5)
  
  Lab_Center_Traj <- trajectory("Lab_Center_Traj")%>%
    addService("Lab_C",5)
  
  Lab_South_Traj <- trajectory("Lab_South_Traj")%>%
    addService("Lab_S",5)
  
  MDA_Break <- trajectory("MDA_Break")%>% ## trajectory that takes out all the ambulances to their break
  seize(resource = "MDA",amount=1)%>%  
  timeout(function () trimmedNorm(15,5))%>%  #the time it takes to drive to the lab
  timeout(20)%>%  #the time it takes to transfer the test to the lab and refill the medical equipment
  release("MDA", amount=1)%>%
  leave(1)
  

  MDA <- trajectory("MDA")%>%
    set_attribute("Symptoms",1)%>%
    seize(resource = "MDA",amount=1)%>%
    set_attribute("onTheWay",1)%>%
    timeout(function() rtriangle(1,10,20,15) + 1.5)%>% ##the time it takes to drive to the client and test him for corona
    release("MDA", amount =1)%>%
    batch( 10000, timeout = 4*60, permanent = FALSE)%>%
    deactivate(sources = "MDA_Supervisor")%>%
    activate(sources = "MDA_Supervisor")%>% #activate supervisor generator
    seize(resource = "MDA",amount = 20)%>% #the batch wait for the ambulances to reach the lab
    timeout(0)%>%
    release("MDA", amount = 20)%>%
    separate()%>%
    branch(option= function() rdiscrete(1,c(0.35,0.42,0.23),c(1,2,3)), continue=c(TRUE,TRUE,TRUE),Lab_North_Traj,Lab_Center_Traj,Lab_South_Traj)%>%
    branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
    rollback(15,1)%>% ##Go back and repeat the Corona test.
    leave(1,quarantine)
 
  
  
  N_mega_drivein <- trajectory("N_mega_drivein")%>%  ##North drive-in traj
    set_attribute("Area",1)%>%  
    simmer::select(resources=c("N1_mini_drivein","N2_mini_drivein","N3_mini_drivein"),policy=c("shortest-queue"))%>% ##sending clients to mini drive in based on the shortest queue
    seize_selected(amount = 1) %>% 
    timeout(1.5) %>%    ## Corona test time
    release_selected(amount = 1)%>%
    batch(100, timeout = 4*60, permanent = FALSE) %>% ## waiting batch, either 100 tests pending for result or 4 hours.
    timeout(function () trimmedNorm(15,5))%>%  ##time from drive-in to labs
    separate()%>%
    addService("Lab_N",5)%>% ##Lab test
    branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
    rollback(11,1)%>% ##Go back and repeat the Corona test.
    leave(1,quarantine)
    
    
  ## 2nd time for uncertain result, we send directly to quarantine
    
    

 C_mega_drivein <- trajectory("C_mega_drivein")%>%
  set_attribute("Area",2)%>%
  simmer::select(resources=c("C1_mini_drivein","C2_mini_drivein","C3_mini_drivein","C4_mini_drivein"),policy=c("shortest-queue"))%>%
  seize_selected(amount = 1)%>%
  timeout(1.5)%>%
  release_selected(amount = 1)%>%
  batch(100, timeout = 4*60, permanent = FALSE) %>%
  timeout(function () trimmedNorm(15,5))%>%  ##time from drive in to labs
  separate()%>% 
  addService("Lab_C",5)%>%
   branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
   rollback(11,1)%>% ##Go back and repeat the Corona test.
   leave(1,quarantine)
   

 S_mega_drivein <- trajectory("S_mega_drivein")%>%
  set_attribute("Area",3)%>%
  simmer::select(resources=c("S1_mini_drivein","S2_mini_drivein","S3_mini_drivein"),policy=c("shortest-queue"))%>%
  seize_selected(amount = 1) %>%
  timeout(1.5) %>%
  release_selected(amount = 1)%>%
  batch(100, timeout = 4*60, permanent = FALSE) %>%
  timeout(function () trimmedNorm(15,5))%>%  ##time from drive in to labs
  separate()%>% 
  addService("Lab_S",5)%>%
  branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
  rollback(11,1)%>% ##Go back and repeat the Corona test.
  leave(1,quarantine)
  


##Area attribute north=1, center=2 , south=3


 S_HMO <- trajectory("S_HMO")%>% 
  set_attribute("Area",3)%>%
  seize("HMO_S",1)%>%
  timeout(1.5)%>%
  release("HMO_S",1)%>%
   set_attribute(key= "finished_hmo",1)%>%
  timeout(function () trimmedNorm(15,5))%>%
  addService("Lab_S",5)%>%
   branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
   rollback(9,1)%>% ##Go back and repeat the Corona test.
   leave(1,quarantine)
  
  

 C_HMO <- trajectory("C_HMO")%>%
  set_attribute("Area",2)%>%  
  seize("HMO_C",1)%>%
  timeout(1.5)%>%
  release("HMO_C",1)%>%
   set_attribute(key= "finished_hmo",1)%>%
  timeout(function () trimmedNorm(15,5))%>%
  addService("Lab_C",5)%>%
   branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
   rollback(9,1)%>% ##Go back and repeat the Corona test.
   leave(1,quarantine)
  

  N_HMO <- trajectory("N_HMO")%>%
  set_attribute("Area",1)%>%  
  seize("HMO_N",1)%>%
  timeout(1.5)%>%
  release("HMO_N",1)%>%
  set_attribute(key= "finished_hmo",1)%>%
  timeout(function () trimmedNorm(15,5))%>% ##time to transfer the test to the labs
  addService("Lab_N",5)%>% ##Lab test
  branch(option= function() rdiscrete(1,c(0.15,0.75,0.1),c(0,1,2)), continue=c(FALSE,FALSE),goHome,quarantine)%>% ##Based on the probability we send the entities as instructed
  rollback(9,1)%>% ##Go back and repeat the Corona test.
  leave(1,quarantine)
  

  HMO <- trajectory("HMO")%>%
  set_attribute("VIP",1)%>%
  branch(option= function() rdiscrete(1,c(0.35,0.42,0.23),c(1,2,3)), continue=c(FALSE,FALSE,FALSE), N_HMO,  C_HMO, S_HMO) ##dividing clients according to the given probabilities
  

  CovidTest <- trajectory("CovidTest")%>%
  set_attribute("VIP",0)%>%
  branch(option= function() rdiscrete(1,c(0.35,0.42,0.23),c(1,2,3)), continue=c(FALSE,FALSE,FALSE), N_mega_drivein,  C_mega_drivein, S_mega_drivein) ##dividing clients according to the given probabilities
  
  

##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
  
  Covid19%>%
    add_generator("Drivein_Clients",CovidTest,distribution = from_to(start_time = 6*60,stop_time = 21*60,function () rexp (1,0.557),FALSE,every = 24*60),mon=2,priority = 0,preemptible = 1,restart = TRUE)%>% ##generating clients between 6am and 9pm
    add_generator("MDA_Clients", MDA, function () rexp (1,0.463), mon=2,priority = 0,preemptible = 1 ,restart = TRUE)%>% 
    add_generator("HMO_Morning_Clients",HMO, distribution= from_to(start_time = 9*60 ,stop_time=13*60 , function () rexp (1,8),TRUE, every = 24*60), mon = 2,priority = 1,preemptible = 2 ,restart = TRUE)%>%  ##generating clients between 9am and 1pm
    add_generator("HMO_Noon_Clients",HMO, distribution= from_to(start_time = 14*60 ,stop_time=18*60 , function () rexp (1,8),TRUE, every = 24*60), mon = 2,priority = 1,preemptible = 2 ,restart = TRUE)%>%  ##generating clients between 2am and 6pm
    add_generator("DriveIn_quarantine", CovidTest, when_activated(2), mon=2,priority = 0,preemptible = 1 ,restart = TRUE)%>%  ##generates 2 more people that need to get covid test
    add_generator("MDA_Supervisor", MDA_Break, when_activated(20), mon=2,priority = 2,preemptible = 3 ,restart = TRUE) ##generate 20 supervisors that take the ambulance to his break #####

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
 #the number of replication=n=15 (on the for loop below)
  
  mm1envs <- mclapply(1:1, function(i) {                   #for one run with simulation Time=48*60
  set.seed(248+i)
    reset(Covid19)%>%run(until=simulationTime) %>%
    wrap()
 })                                    
  
  arrivalData <- get_mon_arrivals(mm1envs)
  resourceData <- get_mon_resources(mm1envs)
  attributeData <- get_mon_attributes(mm1envs)
#----------------------------------------------------------- calc warming time------------------------------------------------------------------------------------------ 
   # vec <- c(seq(1,(50400)/(24*60),1))
   # 
#LabC Queue----------------------------------------------------------------------
# 
#   lab_table <- sqldf("select * from resourceData as re
#                      where re.resource in ('Lab_C')")
# 
#    averageLabqueue <- c()
#    for(i in 1:(50400)/(24*60)){
#      z<- subset(lab_table,lab_table$time<i*(60*24))
#      averageLabqueue[i] <- mean(c(z$queue),na.rm=T)
#    }
# 
#    plot(vec,averageLabqueue,xlab = "time", ylab = "Avg lab c queue" )
#    lines(vec, averageLabqueue, col="green",lty=2)

#wating time HMO-------------------------------------------------------------------
# 
#   fullData<-get_mon_arrivals(Covid19)
# 
#   new <- sqldf("select fd.start_time , (time-start_time-1.5) as waiting_time from fullData as fd join attributeData as ad
#   on fd.name=ad.name
#   where ad.key='finished_hmo'")
# 
#    averageTotalWaiting <- c()
#    for(i in 1:(50400)/(24*60)){
#      x <- subset(new,new$start_time<i*(24*60))
#      averageTotalWaiting[i] <- mean(c(x$waiting_time),na.rm=T)
#    }
# 
#   plot(vec,averageTotalWaiting,xlab = "time", ylab = "Avg Waiting time" )
#   lines(vec, averageTotalWaiting, col="red",lty=2)

#MDA Queue------------------------------------------------------------------------

 # fullData5 <- get_mon_resources(Covid19)
 # 
 #  mda_table2 <- sqldf("select * from fullData5 as re
 #  where re.resource in ('MDA')
 #                     " ) #avg queue for mda
 # 
 # averageMDAqueue <- c()
 #  for(i in 1:50400/(24*60)){
 #    z<- subset(mda_table2,mda_table2$time<i*(24*60))
 #    averageMDAqueue[i] <- mean(c(z$queue),na.rm=T)
 #  }
 # 
 # plot(vec,averageMDAqueue,xlab = "time", ylab = "Avg MDA queue" )
 # lines(vec, averageMDAqueue, col="blue",lty=2)
 
  
  ##----------------------------------------- 7.  Part B ------------------------------------------------
  #avg queue for MDA 
  
  mda_table <- sqldf("select * from resourceData as re
                     where re.resource in ('MDA') and time>=warming_time ")        #Taking the relevant data after the warming time
 
   #Calculate the average queue length in MDA for each run
  
  mesuresMDAQ_kayam<-c ()
    for (i in 1:15) {                                                  #when we run the  simulation 15 replication
      time <- as.matrix(subset(mda_table,replication==i,select=c(time)))
      queueLength <- as.matrix(subset(mda_table,replication==i,select=c(queue)))
      mesuresMDAQ_kayam[i] <- avgQueue(time, queueLength, simulationTime-warming_time) 
    }
 
  Mean1_kayam_MDA_Q <- mean(mesuresMDAQ_kayam)
  sd1_kayam_MDA_Q  <- sd(mesuresMDAQ_kayam)
  
##--------------------------------------------------------------------------------------------------------------  
  fullData<-get_mon_arrivals(mm1envs) #avg waiting time at HMO
  
  #Calculation of the average waiting time in the HMO for each run
  
  wt_hmo_kayam <- sqldf("select replication, avg(waiting_time) as wt from (
  select fd.replication , (time-start_time-1.5) as waiting_time from fullData as fd join attributeData as ad 
                on fd.name=ad.name and fd.replication=ad.replication
                where ad.key='finished_hmo' and fd.start_time>=warming_time)
                group by replication")
  
  Mean2_kayam_WT <- mean(wt_hmo_kayam$wt)
  sd2_kayam_wt <- sd(wt_hmo_kayam$wt)
  
##----------------------------------------------------------------------------------------------------------------------------  
  #avg queue on LabC
  
  lab_table2 <- sqldf("select * from resourceData as re
                       where re.resource in ('Lab_C') and time>=warming_time")    #Taking the relevant data after the warming time
  
  #Calculate the average queue length in a center lab for each run
  
  mesuresLabQ_kayam<-c ()
  for (i in 1:15) {                                                       #when we run the  simulation 15 replication
    time <- as.matrix(subset(lab_table2,replication==i,select=c(time)))
    queueLength <- as.matrix(subset(lab_table2,replication==i,select=c(queue)))
    mesuresLabQ_kayam[i] <- avgQueue(time, queueLength, simulationTime-warming_time)
  }
  
  Mean3_kayam_LabC_Q  <- mean(mesuresLabQ_kayam)
  sd3_kayam_LabC_Q <- sd(mesuresLabQ_kayam)
    
##-----------------------------------------------------------------save data-----------------------------------------------------------------------------------------
  
  kayam_measure1 <-mesuresMDAQ_kayam
  kayam_measure2 <-wt_hmo_kayam$wt
  kayam_measure3 <- mesuresLabQ_kayam
  kayam_table <- as.matrix(data.frame(kayam_measure1,kayam_measure2,kayam_measure3))
  write.csv(kayam_table,"C:/Users/naorp/Desktop/סימולציה/פרוייקט/KayamTable_Measures.csv")