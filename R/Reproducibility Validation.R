##################################################### Energy minimization #####################################################
library(tidyr)
library(dplyr)
library(ggplot2)
setwd("D:/CMML3/ICA/R_result/Energy/")
# Energy minimization
# Receives potential.xvg
T300 <- read.table("T300.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                        stringsAsFactors = FALSE)
T300_1 <- read.table("T300_1.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                   stringsAsFactors = FALSE)
T300_2 <- read.table("T300_2.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                   stringsAsFactors = FALSE)
max_len <- max(length(T300$V1), length(T300_1$V2), length(T300_2$V2))
T300_new <- c(T300$V2, rep(NA, max_len - length(T300$V1)))
T300_1_new <- c(T300_1$V2, rep(NA, max_len - length(T300_1$V2)))
T300_2_new <- c(T300_2$V2, rep(NA, max_len - length(T300_2$V2)))

#Find the model that with the most length
energy <- as.data.frame(cbind(T300_2$V1,T300_new, T300_1_new, T300_2_new))
colnames(energy)<-c('Time','T300','T300_1',"T300_2")
long_df <- pivot_longer(energy, 
                        cols = -Time, 
                        names_to = "Replicate", 
                        values_to = "Value")
ggplot(data = long_df, aes(x = Time, y = Value, color = Replicate)) +
  geom_line() +
  geom_point() +
  ylim(min(T300$V2,T300_1$V2,T300_2$V2), 0) +
  labs(x = "Energy Minimization Step", y = bquote("Potential Energy (kJ "*~mol^-1*')')) +
  ggtitle("T300, Energy Minimization, Steepest Descent") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"))
#################### Statistical analysis
anova_result <- aov(Value ~ Replicate, data = long_df)
summary(anova_result) # P value:   280K: 0.0936   300K: 0.324   320K: 0.105





##################################################### Temperature Equilibration #####################################################
setwd("D:/CMML3/ICA/R_result/Temperature/")
# Temperature equilibration
# Receives temperature.xvg
T320 <- read.table("T320.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                          stringsAsFactors = FALSE)
T320_1 <- read.table("T320_1.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                   stringsAsFactors = FALSE)
T320_2 <- read.table("T320_2.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                   stringsAsFactors = FALSE)
temperature <- cbind(T320,T320_1$V2,T320_2$V2)
colnames(temperature) <- c("Time","T320","T320_1","T320_2")
temperature$T320average10ps <- NA
temperature$T320_1average10ps <- NA
temperature$T320_2average10ps <- NA
temperature$T320average10ps[10:nrow(temperature)] <- sapply(10:nrow(temperature), function(x){mean(temperature$T320[(x-9):x])})
temperature$T320_1average10ps[10:nrow(temperature)] <- sapply(10:nrow(temperature), function(x){mean(temperature$T320_1[(x-9):x])})
temperature$T320_2average10ps[10:nrow(temperature)] <- sapply(10:nrow(temperature), function(x){mean(temperature$T320_2[(x-9):x])})

temperature2 <- temperature[,c(1,5:7)]
colnames(temperature2) <- c("Time","T320","T320_1","T320_2")
temperature <- temperature[,1:4]

long_df <- pivot_longer(temperature, 
                        cols = -Time, 
                        names_to = "Replicate", 
                        values_to = "Value") 
temperature2 <- pivot_longer(temperature2, 
                             cols = -Time, 
                             names_to = "Replicate", 
                             values_to = "Value") 
ggplot(data = long_df, aes(x = Time, y = Value, color = Replicate)) +
  geom_line() +
  geom_point() +
  geom_line(data = temperature2, aes(y = Value, col = Replicate)) +
  labs(x = "Time (ps)", y = "Temperature (K)") +
  ggtitle("T320K, Temperature, NVT equilibration") +
  theme_bw() +
  theme(#legend.position = c(0.80, 0.3),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), face = "bold"))
#################### Statistical analysis
anova_result <- aov(Value ~ Replicate, data = long_df)
summary(anova_result) # P value:   280K: 0.451   300K: 0.962   320K: 0.912 



##################################################### Pressure Equilibration #####################################################
setwd("D:/CMML3/ICA/R_result/Pressure/")
# Pressure equilibration
# Receives Pressure.xvg
T300 <- read.table("T300.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                   stringsAsFactors = FALSE)
T300_1 <- read.table("T300_1.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                     stringsAsFactors = FALSE)
T300_2 <- read.table("T300_2.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                     stringsAsFactors = FALSE)
Pressure <- cbind(T300,T300_1$V2,T300_2$V2)
colnames(Pressure) <- c("Time","T300","T300_1","T300_2")
Pressure$T300average10ps <- NA
Pressure$T300_1average10ps <- NA
Pressure$T300_2average10ps <- NA
Pressure$T300average10ps[10:nrow(Pressure)] <- sapply(10:nrow(Pressure), function(x){mean(Pressure$T300[(x-9):x])})
Pressure$T300_1average10ps[10:nrow(Pressure)] <- sapply(10:nrow(Pressure), function(x){mean(Pressure$T300_1[(x-9):x])})
Pressure$T300_2average10ps[10:nrow(Pressure)] <- sapply(10:nrow(Pressure), function(x){mean(Pressure$T300_2[(x-9):x])})

Pressure2 <- Pressure[,c(1,5:7)]
colnames(Pressure2) <- c("Time","T300","T300_1","T300_2")
Pressure <- Pressure[,1:4]

long_df <- pivot_longer(Pressure, 
                        cols = -Time, 
                        names_to = "Replicate", 
                        values_to = "Value") 
Pressure2 <- pivot_longer(Pressure2, 
                             cols = -Time, 
                             names_to = "Replicate", 
                             values_to = "Value") 
ggplot(data = long_df, aes(x = Time, y = Value, color = Replicate)) +
  geom_line() +
  geom_point() +
  geom_line(data = Pressure2, aes(y = Value, col = Replicate)) +
  labs(x = "Time (ps)", y = "Pressure (K)") +
  ggtitle("T300K, Pressure, NVT equilibration") +
  theme_bw() +
  theme(#legend.position = c(1, 0.9),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), face = "bold"))
#################### Statistical analysis
anova_result <- aov(Value ~ Replicate, data = long_df)
summary(anova_result) # P value:   280K: 0.338   300K: 0.421   320K: 0.595 




##################################################### Density Equilibration #####################################################
setwd("D:/CMML3/ICA/R_result/Density/")
# Density equilibration
# Receives Density.xvg
T320 <- read.table("T320.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                   stringsAsFactors = FALSE)
T320_1 <- read.table("T320_1.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                     stringsAsFactors = FALSE)
T320_2 <- read.table("T320_2.xvg", sep = "" , header = FALSE , skip = 24, na.strings = "",
                     stringsAsFactors = FALSE)
Density <- cbind(T320,T320_1$V2,T320_2$V2)
colnames(Density) <- c("Time","T320","T320_1","T320_2")
Density$T320average10ps <- NA
Density$T320_1average10ps <- NA
Density$T320_2average10ps <- NA
Density$T320average10ps[10:nrow(Density)] <- sapply(10:nrow(Density), function(x){mean(Density$T320[(x-9):x])})
Density$T320_1average10ps[10:nrow(Density)] <- sapply(10:nrow(Density), function(x){mean(Density$T320_1[(x-9):x])})
Density$T320_2average10ps[10:nrow(Density)] <- sapply(10:nrow(Density), function(x){mean(Density$T320_2[(x-9):x])})

Density2 <- Density[,c(1,5:7)]
colnames(Density2) <- c("Time","T320","T320_1","T320_2")
Density <- Density[,1:4]

long_df <- pivot_longer(Density, 
                        cols = -Time, 
                        names_to = "Replicate", 
                        values_to = "Value") 
Density2 <- pivot_longer(Density2, 
                             cols = -Time, 
                             names_to = "Replicate", 
                             values_to = "Value") 
ggplot(data = long_df, aes(x = Time, y = Value, color = Replicate)) +
  geom_line() +
  geom_point() +
  geom_line(data = Density2, aes(y = Value, col = Replicate)) +
  labs(x = "Time (ps)", y = "Density (K)") +
  ggtitle("T320K, Density, NVT equilibration") +
  theme_bw() +
  theme(#legend.position = c(0.80, 0.3),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(1.5), face = "bold"))
#################### Statistical analysis
anova_result <- aov(Value ~ Replicate, data = long_df)
summary(anova_result) # P value:   280K: 0.854   300K: 0.962   320K: 0.418 
