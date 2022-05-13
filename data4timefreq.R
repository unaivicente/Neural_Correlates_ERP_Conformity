#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/EEG Data/ERP2022/Bayesian ERP/")
setwd("~/Desktop/Analysis/timefreq/")

#THETA

#add frequency values at 4-8freq 180-230ms and the difference between 3 and 1 repetition

frequency1 <- read.csv("freqP4_8_180_v1.csv", sep=';', header=T)
names(frequency1)[names(frequency1) == "valor"] <- "timefreq4_8_180"
names(frequency1)[names(frequency1) == "sujeto"] <- "AbsSuj"
names(frequency1)[names(frequency1) == "condicion"] <- "TrialRep"
names(frequency1)[names(frequency1) == "trial"] <- "Trial"

frequency1$TrialRep[frequency1$TrialRep == 2] <- 0
frequency1$TrialRep[frequency1$TrialRep == 4] <- 1
frequency1$TrialRep[frequency1$TrialRep == 6] <- 2
frequency1$electrodo[frequency1$electrodo == 1] <- "Fz"
frequency1$electrodo[frequency1$electrodo == 2] <- "Cz"
frequency1$electrodo[frequency1$electrodo == 3] <- "Pz"

#add frequency values at 4-8freq 230-500ms and the difference between 3 and 1 repetition

frequency2 <- read.csv("freqP4_8_230_v1.csv", sep=';', header=T)
names(frequency2)[names(frequency2) == "valor"] <- "timefreq4_8_230"
names(frequency2)[names(frequency2) == "sujeto"] <- "AbsSuj"
names(frequency2)[names(frequency2) == "condicion"] <- "TrialRep"
names(frequency2)[names(frequency2) == "trial"] <- "Trial"

frequency2$TrialRep[frequency2$TrialRep == 2] <- 0
frequency2$TrialRep[frequency2$TrialRep == 4] <- 1
frequency2$TrialRep[frequency2$TrialRep == 6] <- 2
frequency2$electrodo[frequency2$electrodo == 1] <- "Fz"
frequency2$electrodo[frequency2$electrodo == 2] <- "Cz"
frequency2$electrodo[frequency2$electrodo == 3] <- "Pz"


########

#ALPHA

#add frequency values at 8-12freq 180-230ms and the difference between 3 and 1 repetition

frequency4 <- read.csv("freqP8_12_180_v1.csv", sep=';', header=T)
names(frequency4)[names(frequency4) == "valor"] <- "timefreq8_12_180"
names(frequency4)[names(frequency4) == "sujeto"] <- "AbsSuj"
names(frequency4)[names(frequency4) == "condicion"] <- "TrialRep"
names(frequency4)[names(frequency4) == "trial"] <- "Trial"

frequency4$TrialRep[frequency4$TrialRep == 2] <- 0
frequency4$TrialRep[frequency4$TrialRep == 4] <- 1
frequency4$TrialRep[frequency4$TrialRep == 6] <- 2
frequency4$electrodo[frequency4$electrodo == 1] <- "Fz"
frequency4$electrodo[frequency4$electrodo == 2] <- "Cz"
frequency4$electrodo[frequency4$electrodo == 3] <- "Pz"

#add frequency values at 8-12freq 230-500ms and the difference between 3 and 1 repetition

frequency5 <- read.csv("freqP8_12_230_v1.csv", sep=';', header=T)
names(frequency5)[names(frequency5) == "valor"] <- "timefreq8_12_230"
names(frequency5)[names(frequency5) == "sujeto"] <- "AbsSuj"
names(frequency5)[names(frequency5) == "condicion"] <- "TrialRep"
names(frequency5)[names(frequency5) == "trial"] <- "Trial"

frequency5$TrialRep[frequency5$TrialRep == 2] <- 0
frequency5$TrialRep[frequency5$TrialRep == 4] <- 1
frequency5$TrialRep[frequency5$TrialRep == 6] <- 2
frequency5$electrodo[frequency5$electrodo == 1] <- "Fz"
frequency5$electrodo[frequency5$electrodo == 2] <- "Cz"
frequency5$electrodo[frequency5$electrodo == 3] <- "Pz"


########

#BETA

#add frequency values at 12-30freq 180-230ms and the difference between 3 and 1 repetition

frequency7 <- read.csv("freqP12_30_180_v1.csv", sep=';', header=T)
names(frequency7)[names(frequency7) == "valor"] <- "timefreq12_30_180"
names(frequency7)[names(frequency7) == "sujeto"] <- "AbsSuj"
names(frequency7)[names(frequency7) == "condicion"] <- "TrialRep"
names(frequency7)[names(frequency7) == "trial"] <- "Trial"

frequency7$TrialRep[frequency7$TrialRep == 2] <- 0
frequency7$TrialRep[frequency7$TrialRep == 4] <- 1
frequency7$TrialRep[frequency7$TrialRep == 6] <- 2
frequency7$electrodo[frequency7$electrodo == 1] <- "Fz"
frequency7$electrodo[frequency7$electrodo == 2] <- "Cz"
frequency7$electrodo[frequency7$electrodo == 3] <- "Pz"

#add frequency values at 12-30freq 230-500ms and the difference between 3 and 1 repetition

frequency8 <- read.csv("freqP12_30_230_v1.csv", sep=';', header=T)
names(frequency8)[names(frequency8) == "valor"] <- "timefreq12_30_230"
names(frequency8)[names(frequency8) == "sujeto"] <- "AbsSuj"
names(frequency8)[names(frequency8) == "condicion"] <- "TrialRep"
names(frequency8)[names(frequency8) == "trial"] <- "Trial"

frequency8$TrialRep[frequency8$TrialRep == 2] <- 0
frequency8$TrialRep[frequency8$TrialRep == 4] <- 1
frequency8$TrialRep[frequency8$TrialRep == 6] <- 2
frequency8$electrodo[frequency8$electrodo == 1] <- "Fz"
frequency8$electrodo[frequency8$electrodo == 2] <- "Cz"
frequency8$electrodo[frequency8$electrodo == 3] <- "Pz"


########

#GAMMA

#add frequency values at 30-42freq 180-230ms and the difference between 3 and 1 repetition

frequency10 <- read.csv("freqP30_42_180_v1.csv", sep=';', header=T)
names(frequency10)[names(frequency10) == "valor"] <- "timefreq30_42_180"
names(frequency10)[names(frequency10) == "sujeto"] <- "AbsSuj"
names(frequency10)[names(frequency10) == "condicion"] <- "TrialRep"
names(frequency10)[names(frequency10) == "trial"] <- "Trial"

frequency10$TrialRep[frequency10$TrialRep == 2] <- 0
frequency10$TrialRep[frequency10$TrialRep == 4] <- 1
frequency10$TrialRep[frequency10$TrialRep == 6] <- 2
frequency10$electrodo[frequency10$electrodo == 1] <- "Fz"
frequency10$electrodo[frequency10$electrodo == 2] <- "Cz"
frequency10$electrodo[frequency10$electrodo == 3] <- "Pz"

#add frequency values at 30-42freq 230-500ms and the difference between 3 and 1 repetition

frequency11 <- read.csv("freqP30_42_230_v1.csv", sep=';', header=T)
names(frequency11)[names(frequency11) == "valor"] <- "timefreq30_42_230"
names(frequency11)[names(frequency11) == "sujeto"] <- "AbsSuj"
names(frequency11)[names(frequency11) == "condicion"] <- "TrialRep"
names(frequency11)[names(frequency11) == "trial"] <- "Trial"

frequency11$TrialRep[frequency11$TrialRep == 2] <- 0
frequency11$TrialRep[frequency11$TrialRep == 4] <- 1
frequency11$TrialRep[frequency11$TrialRep == 6] <- 2
frequency11$electrodo[frequency11$electrodo == 1] <- "Fz"
frequency11$electrodo[frequency11$electrodo == 2] <- "Cz"
frequency11$electrodo[frequency11$electrodo == 3] <- "Pz"


frequencies <- cbind(frequency1,frequency2,
                     frequency4,frequency5,
                     frequency7,frequency8,
                     frequency10,frequency11)

frequencies <- frequencies[order(frequencies$AbsSuj,frequencies$TrialRep),]
frequencies <- frequencies[, !duplicated(colnames(frequencies))]


freq <- with(frequencies,frequencies[order(AbsSuj, TrialRep),])

freq$ID <-  vector(length = dim(freq)[1])

for (i in 1:nrow(freq)) {
  if (0<freq$AbsSuj[i]&&freq$AbsSuj[i]<=2){freq$ID[i] <- print("E01")}
  else if (2<freq$AbsSuj[i]&&freq$AbsSuj[i]<=4){freq$ID[i] <- print("E02")} 
  else if (4<freq$AbsSuj[i]&&freq$AbsSuj[i]<=6){freq$ID[i] <- print("E03")} 
  else if (6<freq$AbsSuj[i]&&freq$AbsSuj[i]<=8){freq$ID[i] <- print("E04")} 
  else if (8<freq$AbsSuj[i]&&freq$AbsSuj[i]<=10){freq$ID[i] <- print("E05")} 
  else if (10<freq$AbsSuj[i]&&freq$AbsSuj[i]<=12){freq$ID[i] <- print("E06")} 
  else if (12<freq$AbsSuj[i]&&freq$AbsSuj[i]<=14){freq$ID[i] <- print("E07")} 
  else if (14<freq$AbsSuj[i]&&freq$AbsSuj[i]<=16){freq$ID[i] <- print("E08")} 
  else if (16<freq$AbsSuj[i]&&freq$AbsSuj[i]<=18){freq$ID[i] <- print("E09")} 
  else if (18<freq$AbsSuj[i]&&freq$AbsSuj[i]<=20){freq$ID[i] <- print("E10")} 
  else if (20<freq$AbsSuj[i]&&freq$AbsSuj[i]<=22){freq$ID[i] <- print("E11")} 
  else if (22<freq$AbsSuj[i]&&freq$AbsSuj[i]<=24){freq$ID[i] <- print("E12")} 
  else if (24<freq$AbsSuj[i]&&freq$AbsSuj[i]<=26){freq$ID[i] <- print("E13")} 
  else if (26<freq$AbsSuj[i]&&freq$AbsSuj[i]<=28){freq$ID[i] <- print("E14")} 
  else if (28<freq$AbsSuj[i]&&freq$AbsSuj[i]<=30){freq$ID[i] <- print("E15")} 
  else if (30<freq$AbsSuj[i]&&freq$AbsSuj[i]<=32){freq$ID[i] <- print("E16")} 
  else if (32<freq$AbsSuj[i]&&freq$AbsSuj[i]<=34){freq$ID[i] <- print("E17")} 
  else {freq$ID[i] <- print("E18")} 
}

freq$AbsSuj <- as.factor(freq$AbsSuj)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(freq, is.nan))

freq[is.nan(freq)] <- NA
freq <- freq[, c("AbsSuj","electrodo","TrialRep","Trial","ID",
                 "timefreq4_8_180","timefreq4_8_230",
                 "timefreq8_12_180","timefreq8_12_230",
                 "timefreq12_30_180","timefreq12_30_230",
                 "timefreq30_42_180","timefreq30_42_230")]

write.csv(freq, file = "finaldb_tf.csv")
