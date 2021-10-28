setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/TimeFrequency")


library(brms)
library(bayestestR)
library(tidyr)

#Remove # from the desired frequency to analyse
#modeltheta <- readRDS('modeltheta_v11.rds')
#modelalpha <- readRDS('modelalpha_v11.rds')
#modelbeta <- readRDS('modelbeta_v11.rds')

#Remove # from the desired frequency to analyse
#posterior_data <- posterior_samples(modeltheta)
#posterior_data <- posterior_samples(modelalpha)
#posterior_data <- posterior_samples(modelbeta)

elec <- as.data.frame(c('Fz','Cz','Pz'))
colnames(elec) <- "a"

output <- data.frame()

for (i in 1:nrow(elec)){
  
  b0 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo[%s,Intercept]',elec$a[i])]]
  b1 <- posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo[%s,TrialRep1]',elec$a[i])]]
  b2 <- posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo[%s,TrialRep2]',elec$a[i])]]
  
  var_hyp1 <- -b1
  var_hyp2 <- -b2
  var_hyp3 <- b1 - b2
  
  hyp1test <- equivalence_test(var_hyp1, ci=(.95), range=c(-.01,.01))
  hyp2test <- equivalence_test(var_hyp2, ci=(.95), range=c(-.01,.01))
  hyp3test <- equivalence_test(var_hyp3, ci=(.95), range=c(-.01,.01))
  

  hyp1test$Hypothesys <- "H1:TrialRep 1"
  hyp1test$Elec <- sprintf('%s',elec[i,])
  hyp2test$Hypothesys <- "H2:TrialRep 2"
  hyp2test$Elec <- sprintf('%s',elec[i,])
  hyp3test$Hypothesys <- "H3:Difference"
  hyp3test$Elec <- sprintf('%s',elec[i,])

  dat <- rbind(hyp1test, hyp2test, hyp3test)
  output <- rbind(dat, output)
}

