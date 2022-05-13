#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP/")
setwd("~/Desktop/Analysis/")


library(brms)
library(bayestestR)
library(tidyr)


lead <- readRDS('lead_v1.rds') #lead as intercept

posterior_data <- as_draws(lead)
posterior_data <- do.call(rbind.data.frame, posterior_data)

time <- c('225', '275','350','500')
elec <- c('Fz','Cz','Pz')

df = expand.grid(a = time, b = elec)
output <- data.frame()

for (i in 1:nrow(df)){

  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s,Intercept.',df$a[i],df$b[i])]]
  b1 <- posterior_data[['b_Lead2']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.Lead2.',df$a[i],df$b[i])]]
  b2 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.TrialRep1.',df$a[i],df$b[i])]]
  b3 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2.C_%s_%s.TrialRep2.',df$a[i],df$b[i])]]
  b4 <- posterior_data[['b_Lead2.TrialRep1']]+posterior_data[[sprintf('r_electrodo2.C_%s_%s.Lead2.TrialRep1.',df$a[i],df$b[i])]]
  b5 <- posterior_data[['b_Lead2.TrialRep2']]+posterior_data[[sprintf('r_electrodo2.C_%s_%s.Lead2.TrialRep2.',df$a[i],df$b[i])]]

  var_hyp0 <- b1 #"Leader-Follower: Differences in Intercept"
  var_hyp1 <- -b2 #"Leaders: TrialRep 1 over Intercept"
  var_hyp2 <- -b3 #"Leaders: TrialRep 2 over Intercept"
  var_hyp3 <- b2-b3 #"Leaders: Difference 2 over 1"
  var_hyp4 <- -b2-b4 #"Followers: TrialRep 1 over Intercept"
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
  hyp0test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp1test$Hypothesys <- "H1:Leaders: TrialRep 1 over Intercept"
  hyp1test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp2test$Hypothesys <- "H2:Leaders: TrialRep 2 over Intercept"
  hyp2test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp3test$Hypothesys <- "H3:Leaders: Difference 2 over 1"
  hyp3test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp4test$Hypothesys <- "H4:Followers: TrialRep 1 over Intercept"
  hyp4test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp5test$Hypothesys <- "H5:Followers: TrialRep 2 over Intercept"
  hyp5test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp6test$Hypothesys <- "H6:Followers: Difference 2 over 1"
  hyp6test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  hyp7test$Hypothesys <- "H7:Leader-Follower: TrialRep 1 over Intercept"
  hyp7test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
  
  
  dat <- rbind(hyp0test, hyp1test, hyp2test, hyp3test,hyp4test, hyp5test, hyp6test, hyp7test)
  output <- rbind(dat, output)

}

