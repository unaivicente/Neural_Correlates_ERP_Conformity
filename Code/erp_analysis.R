setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/New Julio2020/ERP Método Alberto")


library(brms)
library(bayestestR)
library(tidyr)


trialrep <- readRDS('trialrep_v2.rds')

posterior_trialrep <- posterior_samples(trialrep)

#
C_230_Fz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_230_Fz,TrialRep]`
C_230_Fz_hyp <- equivalence_test(C_230_Fz, ci=(.95), range=c(-.01,.01))
C_230_Fz_hyp$electrode <- "230ms Fz"

C_230_Cz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_230_Cz,TrialRep]`
C_230_Cz_hyp <- equivalence_test(C_230_Cz, ci=(.95), range=c(-.01,.01))
C_230_Cz_hyp$electrode <- "230ms Cz"

C_230_Pz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_230_Pz,TrialRep]`
C_230_Pz_hyp <- equivalence_test(C_230_Pz, ci=(.95), range=c(-.01,.01))
C_230_Pz_hyp$electrode <- "230ms Pz"
###
C_300_Fz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_300_Fz,TrialRep]`
C_300_Fz_hyp <- equivalence_test(C_300_Fz, ci=(.95), range=c(-.01,.01))
C_300_Fz_hyp$electrode <- "300ms Fz"

C_300_Cz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_300_Cz,TrialRep]`
C_300_Cz_hyp <- equivalence_test(C_300_Cz, ci=(.95), range=c(-.01,.01))
C_300_Cz_hyp$electrode <- "300ms Cz"

C_300_Pz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_300_Pz,TrialRep]`
C_300_Pz_hyp <- equivalence_test(C_300_Pz, ci=(.95), range=c(-.01,.01))
C_300_Pz_hyp$electrode <- "300ms Pz"
###
C_500_Fz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_500_Fz,TrialRep]`
C_500_Fz_hyp <- equivalence_test(C_500_Fz, ci=(.95), range=c(-.01,.01))
C_500_Fz_hyp$electrode <- "500ms Fz"

C_500_Cz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_500_Cz,TrialRep]`
C_500_Cz_hyp <- equivalence_test(C_500_Cz, ci=(.95), range=c(-.01,.01))
C_500_Cz_hyp$electrode <- "500ms Cz"

C_500_Pz <- posterior_trialrep$b_TrialRep + posterior_trialrep$`r_electrodo2[C_500_Pz,TrialRep]`
C_500_Pz_hyp <- equivalence_test(C_500_Pz, ci=(.95), range=c(-.01,.01))
C_500_Pz_hyp$electrode <- "500ms Pz"


results_tr <- rbind(C_230_Fz_hyp, C_230_Cz_hyp, C_230_Pz_hyp,
                    C_300_Fz_hyp, C_300_Cz_hyp, C_300_Pz_hyp,
                    C_500_Fz_hyp, C_500_Cz_hyp, C_500_Pz_hyp)

results_tr$model <- "TrialRep"

#######



lead <- readRDS('lead_v3.rds') #lead as intercept
posterior_lead <- posterior_samples(lead)

#
C_230_Fz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_230_Fz,Lead2:TrialRep1]`
C_230_Fz_rep1_hyp <- equivalence_test(C_230_Fz_rep1, ci=(.95), range=c(-.01,.01))
C_230_Fz_rep1_hyp$electrode <- "230ms Fz Rep 1"

C_230_Fz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_230_Fz,Lead2:TrialRep2]`
C_230_Fz_rep2_hyp <- equivalence_test(C_230_Fz_rep2, ci=(.95), range=c(-.01,.01))
C_230_Fz_rep2_hyp$electrode <- "230ms Fz Rep 2"
#
C_230_Cz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_230_Cz,Lead2:TrialRep1]`
C_230_Cz_rep1_hyp <- equivalence_test(C_230_Cz_rep1, ci=(.95), range=c(-.01,.01))
C_230_Cz_rep1_hyp$electrode <- "230ms Cz Rep 1"

C_230_Cz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_230_Cz,Lead2:TrialRep2]`
C_230_Cz_rep2_hyp <- equivalence_test(C_230_Cz_rep2, ci=(.95), range=c(-.01,.01))
C_230_Cz_rep2_hyp$electrode <- "230ms Cz Rep 2"
#
C_230_Pz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_230_Pz,Lead2:TrialRep1]`
C_230_Pz_rep1_hyp <- equivalence_test(C_230_Pz_rep1, ci=(.95), range=c(-.01,.01))
C_230_Pz_rep1_hyp$electrode <- "230ms Pz Rep 1"

C_230_Pz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_230_Pz,Lead2:TrialRep2]`
C_230_Pz_rep2_hyp <- equivalence_test(C_230_Pz_rep2, ci=(.95), range=c(-.01,.01))
C_230_Pz_rep2_hyp$electrode <- "230ms Pz Rep 2"
########
C_300_Fz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_300_Fz,Lead2:TrialRep1]`
C_300_Fz_rep1_hyp <- equivalence_test(C_300_Fz_rep1, ci=(.95), range=c(-.01,.01))
C_300_Fz_rep1_hyp$electrode <- "300ms Fz Rep 1"

C_300_Fz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_300_Fz,Lead2:TrialRep2]`
C_300_Fz_rep2_hyp <- equivalence_test(C_300_Fz_rep2, ci=(.95), range=c(-.01,.01))
C_300_Fz_rep2_hyp$electrode <- "300ms Fz Rep 2"
#
C_300_Cz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_300_Cz,Lead2:TrialRep1]`
C_300_Cz_rep1_hyp <- equivalence_test(C_300_Cz_rep1, ci=(.95), range=c(-.01,.01))
C_300_Cz_rep1_hyp$electrode <- "300ms Cz Rep 1"

C_300_Cz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_300_Cz,Lead2:TrialRep2]`
C_300_Cz_rep2_hyp <- equivalence_test(C_300_Cz_rep2, ci=(.95), range=c(-.01,.01))
C_300_Cz_rep2_hyp$electrode <- "300ms Cz Rep 2"
#
C_300_Pz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_300_Pz,Lead2:TrialRep1]`
C_300_Pz_rep1_hyp <- equivalence_test(C_300_Pz_rep1, ci=(.95), range=c(-.01,.01))
C_300_Pz_rep1_hyp$electrode <- "300ms Pz Rep 1"

C_300_Pz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_300_Pz,Lead2:TrialRep2]`
C_300_Pz_rep2_hyp <- equivalence_test(C_300_Pz_rep2, ci=(.95), range=c(-.01,.01))
C_300_Pz_rep2_hyp$electrode <- "300ms Pz Rep 2"
########
C_500_Fz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_500_Fz,Lead2:TrialRep1]`
C_500_Fz_rep1_hyp <- equivalence_test(C_500_Fz_rep1, ci=(.95), range=c(-.01,.01))
C_500_Fz_rep1_hyp$electrode <- "500ms Fz Rep 1"

C_500_Fz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_500_Fz,Lead2:TrialRep2]`
C_500_Fz_rep2_hyp <- equivalence_test(C_500_Fz_rep2, ci=(.95), range=c(-.01,.01))
C_500_Fz_rep2_hyp$electrode <- "500ms Fz Rep 2"
#
C_500_Cz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_500_Cz,Lead2:TrialRep1]`
C_500_Cz_rep1_hyp <- equivalence_test(C_500_Cz_rep1, ci=(.95), range=c(-.01,.01))
C_500_Cz_rep1_hyp$electrode <- "500ms Cz Rep 1"

C_500_Cz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_500_Cz,Lead2:TrialRep2]`
C_500_Cz_rep2_hyp <- equivalence_test(C_500_Cz_rep2, ci=(.95), range=c(-.01,.01))
C_500_Cz_rep2_hyp$electrode <- "500ms Cz Rep 2"
#
C_500_Pz_rep1 <- posterior_lead$`b_Lead2:TrialRep1` + posterior_lead$`r_electrodo2[C_500_Pz,Lead2:TrialRep1]`
C_500_Pz_rep1_hyp <- equivalence_test(C_500_Pz_rep1, ci=(.95), range=c(-.01,.01))
C_500_Pz_rep1_hyp$electrode <- "500ms Pz Rep 1"

C_500_Pz_rep2 <- posterior_lead$`b_Lead2:TrialRep2` + posterior_lead$`r_electrodo2[C_500_Pz,Lead2:TrialRep2]`
C_500_Pz_rep2_hyp <- equivalence_test(C_500_Pz_rep2, ci=(.95), range=c(-.01,.01))
C_500_Pz_rep2_hyp$electrode <- "500ms Pz Rep 2"



results_lead <- rbind(C_230_Fz_rep1_hyp, C_230_Cz_rep1_hyp, C_230_Pz_rep1_hyp,
                      C_300_Fz_rep1_hyp, C_300_Cz_rep1_hyp, C_300_Pz_rep1_hyp,
                      C_500_Fz_rep1_hyp, C_500_Cz_rep1_hyp, C_500_Pz_rep1_hyp,
                      C_230_Fz_rep2_hyp, C_230_Cz_rep2_hyp, C_230_Pz_rep2_hyp,
                      C_300_Fz_rep2_hyp, C_300_Cz_rep2_hyp, C_300_Pz_rep2_hyp,
                      C_500_Fz_rep2_hyp, C_500_Cz_rep2_hyp, C_500_Pz_rep2_hyp)


results_lead$model <- "Leader"


#####



winner <- readRDS('winner_v3.rds')

posterior_winner <- posterior_samples(winner)

#
C_230_Fz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_230_Fz,TrialRep]`
C_230_Fz_hyp <- equivalence_test(C_230_Fz, ci=(.95), range=c(-.01,.01))
C_230_Fz_hyp$electrode <- "230ms Fz"

C_230_Cz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_230_Cz,TrialRep]`
C_230_Cz_hyp <- equivalence_test(C_230_Cz, ci=(.95), range=c(-.01,.01))
C_230_Cz_hyp$electrode <- "230ms Cz"

C_230_Pz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_230_Pz,TrialRep]`
C_230_Pz_hyp <- equivalence_test(C_230_Pz, ci=(.95), range=c(-.01,.01))
C_230_Pz_hyp$electrode <- "230ms Pz"
###
C_300_Fz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_300_Fz,TrialRep]`
C_300_Fz_hyp <- equivalence_test(C_300_Fz, ci=(.95), range=c(-.01,.01))
C_300_Fz_hyp$electrode <- "300ms Fz"

C_300_Cz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_300_Cz,TrialRep]`
C_300_Cz_hyp <- equivalence_test(C_300_Cz, ci=(.95), range=c(-.01,.01))
C_300_Cz_hyp$electrode <- "300ms Cz"

C_300_Pz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_300_Pz,TrialRep]`
C_300_Pz_hyp <- equivalence_test(C_300_Pz, ci=(.95), range=c(-.01,.01))
C_300_Pz_hyp$electrode <- "300ms Pz"
###
C_500_Fz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_500_Fz,TrialRep]`
C_500_Fz_hyp <- equivalence_test(C_500_Fz, ci=(.95), range=c(-.01,.01))
C_500_Fz_hyp$electrode <- "500ms Fz"

C_500_Cz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_500_Cz,TrialRep]`
C_500_Cz_hyp <- equivalence_test(C_500_Cz, ci=(.95), range=c(-.01,.01))
C_500_Cz_hyp$electrode <- "500ms Cz"

C_500_Pz <- posterior_winner$b_winnerWinner + posterior_winner$`r_electrodo2[C_500_Pz,TrialRep]`
C_500_Pz_hyp <- equivalence_test(C_500_Pz, ci=(.95), range=c(-.01,.01))
C_500_Pz_hyp$electrode <- "500ms Pz"


results_winner <- rbind(C_230_Fz_hyp, C_230_Cz_hyp, C_230_Pz_hyp,
                        C_300_Fz_hyp, C_300_Cz_hyp, C_300_Pz_hyp,
                        C_500_Fz_hyp, C_500_Cz_hyp, C_500_Pz_hyp)

results_winner$model <- "Winner"
####


Results <- rbind(results_tr,results_winner,results_lead)




