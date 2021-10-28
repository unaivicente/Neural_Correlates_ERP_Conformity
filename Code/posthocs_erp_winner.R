setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/ERP Método Alberto")


library(brms)
library(bayestestR)
library(tidyr)


lead <- readRDS('winner_v3.rds') #lead as intercept
posterior_data <- posterior_samples(lead)

time <- c('230', '300', '500')
elec <- c('Fz','Cz','Pz')

df = expand.grid(a = time, b = elec)

output <- data.frame()

for (i in 1:nrow(df)){

intp <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]
winner <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]+posterior_data[['b_winnerWinner']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,winnerWinner]',df$a[i],df$b[i])]]
trep1 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]+posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep1]',df$a[i],df$b[i])]]
trep2 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]+posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep2]',df$a[i],df$b[i])]]
intx_tr1 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]+posterior_data[['b_winnerWinner']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,winnerWinner]',df$a[i],df$b[i])]]+posterior_data[['b_TrialRep1']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep1]',df$a[i],df$b[i])]]+posterior_data[['b_winnerWinner:TrialRep1']]+posterior_data[[sprintf('r_electrodo2[C_%s_%s,winnerWinner:TrialRep1]',df$a[i],df$b[i])]]
intx_tr2 <- posterior_data[['b_Intercept']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,Intercept]',df$a[i],df$b[i])]]+posterior_data[['b_winnerWinner']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,winnerWinner]',df$a[i],df$b[i])]]+posterior_data[['b_TrialRep2']] + posterior_data[[sprintf('r_electrodo2[C_%s_%s,TrialRep2]',df$a[i],df$b[i])]]+posterior_data[['b_winnerWinner:TrialRep2']]+posterior_data[[sprintf('r_electrodo2[C_%s_%s,winnerWinner:TrialRep2]',df$a[i],df$b[i])]]

var_hyp1 <- winner - intx_tr1
var_hyp2 <- winner - intx_tr2
var_hyp3 <- var_hyp1 - var_hyp2
var_hyp4 <- intp - trep1
var_hyp5 <- intp - trep2
var_hyp6 <- var_hyp4 - var_hyp3
var_hyp7 <- var_hyp4 - var_hyp1

hyp1test <- equivalence_test(var_hyp1, ci=(.95), range=c(-.01,.01))
hyp2test <- equivalence_test(var_hyp2, ci=(.95), range=c(-.01,.01))
hyp3test <- equivalence_test(var_hyp3, ci=(.95), range=c(-.01,.01))
hyp4test <- equivalence_test(var_hyp4, ci=(.95), range=c(-.01,.01))
hyp5test <- equivalence_test(var_hyp5, ci=(.95), range=c(-.01,.01))
hyp6test <- equivalence_test(var_hyp6, ci=(.95), range=c(-.01,.01))
hyp7test <- equivalence_test(var_hyp7, ci=(.95), range=c(-.01,.01))

hyp1test$Hypothesys <- "Losers: TrialRep 1 over Intercept"
hyp1test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
hyp2test$Hypothesys <- "Losers: TrialRep 2 over Intercept"
hyp2test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
hyp3test$Hypothesys <- "Losers: Difference 2 over 1"
hyp3test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
hyp4test$Hypothesys <- "Winners: TrialRep 1 over Intercept"
hyp4test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
hyp5test$Hypothesys <- "Winners: TrialRep 2 over Intercept"
hyp5test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
hyp6test$Hypothesys <- "Winners: Difference 2 over 1"
hyp6test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])
hyp7test$Hypothesys <- "Losers over Winners: Trial 1 over Intercept"
hyp7test$ElecTime <- sprintf('%s_%s',df$a[i],df$b[i])


dat <- rbind(hyp1test, hyp2test, hyp3test,hyp4test, hyp5test, hyp6test, hyp7test)
output <- rbind(dat, output)

}


