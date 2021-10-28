setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/TimeFrequency")


library(brms)
library(bayestestR)
library(tidyr)

#Remove # from the desired frequency to analyse

modeltheta <- readRDS('modeltheta_lead_v2.rds')
#modelalpha <- readRDS('modelalpha_lead_v2.rds')
#modelbeta <- readRDS('modelbeta_lead_v2.rds')


#Remove # from the desired frequency to analyse

posterior_data <- posterior_samples(modeltheta)
#posterior_data <- posterior_samples(modelalpha)
#posterior_data <- posterior_samples(modelbeta)

elec <- as.data.frame(c('Fz','Cz','Pz'))
colnames(elec) <- "a"

output <- data.frame()

for (i in 1:nrow(elec)){

  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo[%s,Intercept]', elec$a[i])]]
  b1 <- posterior_data[['b_Lead2']] + posterior_data[[sprintf('r_electrodo[%s,Lead2]', elec$a[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo[%s,TrialRep1]', elec$a[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo[%s,TrialRep2]', elec$a[i])]]
  b4 <- posterior_data[['b_Lead2:TrialRep1']]+posterior_data[[sprintf('r_electrodo[%s,Lead2:TrialRep1]', elec$a[i])]]
  b5 <- posterior_data[['b_Lead2:TrialRep2']]+posterior_data[[sprintf('r_electrodo[%s,Lead2:TrialRep2]', elec$a[i])]]
  
  var_hyp0 <- b1 #"Leader-Follower: Differences in Intercept"
  var_hyp1 <- -b2 #"Leaders: TrialRep 1 over Intercept"
  var_hyp2 <- -b3 #"Leaders: TrialRep 2 over Intercept"
  var_hyp3 <- b2 - b3 #"Leaders: Difference 2 over 1"
  var_hyp4 <- -b2 -b4 #"Followers: TrialRep 1 over Intercept"
  var_hyp5 <- -b3-b5 #"Followers: TrialRep 2 over Intercept"
  var_hyp6 <- b2+b4-b3-b5 #"Followers: Difference 2 over 1"
  var_hyp7 <- b4 #"Leader-Follower: TrialRep 1 over Intercept"
  
  
  hyp0test <- equivalence_test(var_hyp0, ci=(.95), range=c(-.01,.01))
  hyp1test <- equivalence_test(var_hyp1, ci=(.95), range=c(-.01,.01))
  hyp2test <- equivalence_test(var_hyp2, ci=(.95), range=c(-.01,.01))
  hyp3test <- equivalence_test(var_hyp3, ci=(.95), range=c(-.01,.01))
  hyp4test <- equivalence_test(var_hyp4, ci=(.95), range=c(-.01,.01))
  hyp5test <- equivalence_test(var_hyp5, ci=(.95), range=c(-.01,.01))
  hyp6test <- equivalence_test(var_hyp6, ci=(.95), range=c(-.01,.01))
  hyp7test <- equivalence_test(var_hyp7, ci=(.95), range=c(-.01,.01))

  hyp0test$Hypothesys <- "H0:Leader-Follower: Differences in Intercept"
  hyp0test$Elec <- sprintf('%s',elec[i,])
  hyp1test$Hypothesys <- "H1:Leaders: TrialRep 1 over Intercept"
  hyp1test$Elec <- sprintf('%s',elec[i,])
  hyp2test$Hypothesys <- "H2:Leaders: TrialRep 2 over Intercept"
  hyp2test$Elec <- sprintf('%s',elec[i,])
  hyp3test$Hypothesys <- "H3:Leaders: Difference 2 over 1"
  hyp3test$Elec <- sprintf('%s',elec[i,])
  hyp4test$Hypothesys <- "H4:Followers: TrialRep 1 over Intercept"
  hyp4test$Elec <- sprintf('%s',elec[i,])
  hyp5test$Hypothesys <- "H5:Followers: TrialRep 2 over Intercept"
  hyp5test$Elec <- sprintf('%s',elec[i,])
  hyp6test$Hypothesys <- "H6:Followers: Difference 2 over 1"
  hyp6test$Elec <- sprintf('%s',elec[i,])
  hyp7test$Hypothesys <- "H7:Leader-Follower: TrialRep 1 over Intercept"
  hyp7test$Elec <- sprintf('%s',elec[i,])

  
  
  dat <- rbind(hyp0test, hyp1test, hyp2test, hyp3test, hyp4test, hyp5test, hyp6test, hyp7test)
  output <- rbind(dat, output)
}

