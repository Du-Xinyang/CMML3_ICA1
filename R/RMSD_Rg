##################################################### RMSD #####################################################
setwd("D:/CMML3/ICA/R_result/RMSD/")
library(ggplot2)
# RMSD, backbone
# Receives rmsd.xvg and rmsd_xtal.xvg
###########
# For Fig.1C
rmsd_T280_2 <- read.table("T280_2.xvg", sep = "" , header = FALSE , skip = 18, na.strings = "",
                                stringsAsFactors = FALSE)
rmsd_T300_2 <- read.table("T300_2.xvg", sep = "" , header = FALSE , skip = 18, na.strings = "",
                        stringsAsFactors = FALSE)
rmsd_T320_2 <- read.table("T320_2.xvg", sep = "" , header = FALSE , skip = 18, na.strings = "",
                          stringsAsFactors = FALSE)
rmsd <- rmsd_T280_2
names(rmsd) <- c("time", "T320")
rmsd$T300_2 <- rmsd_T300_2$V2
rmsd$T320_2 <- rmsd_T320_2$V2
rmsd_filtered <- rmsd[rmsd$time >= 20 & rmsd$time <= 25, ] 
long_df <- pivot_longer(rmsd_filtered,
                        cols = -time,
                        names_to = "Replicate",
                        values_to = "Value")
ggplot(long_df, aes(x = time)) +
  geom_line(aes(y = T280_2, col = "Ref: T280")) +
  geom_line(aes(y = T300_2, col = "Ref: T300")) +
  geom_line(aes(y = T320_2, col = "Ref: T320") ) +
  labs(x = "Time (ns)", y = "RMSD (nm)") +
  ggtitle("RMSD Comparison, backbone") +
  theme_bw() +
  theme(legend.position = c(0.80, 0.2),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), face = "bold"))

###########
# For Fig1.A, change the input file to T280.xvg, T280_1.xvg, T280_2.xvg
rmsd <- rmsd_T280
names(rmsd) <- c("time", "T280")
rmsd$T280_1 <- rmsd_T280_1$V2
rmsd$T280_2 <- rmsd_T280_2$V2
red_palette <- c("#8B0000", "#B22222", "#FF6347")  # for 280K
green_palette <- c("#006400", "#228B22", "#32CD32")  # for 300K
blue_palette <- c("#00008B", "#1E90FF", "#87CEFA")  # for 320K
ggplot(rmsd, aes(x = time)) +
  geom_line(aes(y = T320, color = "Ref: T320"), linewidth = 1) +
  geom_line(aes(y = T320_1, color = "Ref: T320_1"), linewidth = 1) +
  geom_line(aes(y = T320_2, color = "Ref: T320_2"), linewidth = 1) +
  scale_color_manual(
    values = blue_palette,  
    labels = c("T320", "T320_1", "T320_2")  
  ) +
  labs(
    x = "Time (ns)", 
    y = "RMSD (nm)", 
    title = "320K RMSD Comparison, backbone",
    color = "Temperature"  
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.80, 0.2),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    panel.grid.major = element_line(color = "grey90") 
  )




##################################################### Rg #####################################################
setwd("D:/CMML3/ICA/R_result/gyrate/")
library(tidyr)
# Radius of gyration
# Receives gyrate.xvg

###########
# For Fig.1D
T300 <- read.table("T300.xvg", sep = "" , header = FALSE , skip = 27, na.strings = "",
                       stringsAsFactors = FALSE)
T300_1 <- read.table("T300_1.xvg", sep = "" , header = FALSE , skip = 27, na.strings = "",
                    stringsAsFactors = FALSE)
T300_2 <- read.table("T300_2.xvg", sep = "" , header = FALSE , skip = 27, na.strings = "",
                    stringsAsFactors = FALSE)
time <- T300$V1/1000
gyration <- as.data.frame(cbind(time,T300$V2,T300_1$V2,T300_2$V2))
colnames(gyration) <- c("Time","T300","T300_1","T300_2")
long_df <- pivot_longer(gyration, 
                        cols = -Time, 
                        names_to = "Replicate", 
                        values_to = "Value") 

# For Fig.1E
gyration_filtered <- gyration[gyration$Time >= 15 & gyration$Time <= 25, ]
long_df <- pivot_longer(gyration_filtered, 
                        cols = -Time, 
                        names_to = "Replicate", 
                        values_to = "Value") 
anova_result <- aov(Value ~ Replicate, data = long_df)
summary(anova_result) # P = 0.137

ggplot(data = long_df, aes(x = Time, y = Value,color = Replicate)) +
  geom_line() +
  labs(x = "Time (ns)", y = bquote(~R[g]*" (nm)")) +
  ggtitle("50ns, Radius of gyration") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"))

###########
#For Fig.1B
red_palette <- c("#8B0000", "#B22222", "#FF6347")  # for 280K
green_palette <- c("#006400", "#228B22", "#32CD32")  # for 300K
blue_palette <- c("#00008B", "#1E90FF", "#87CEFA")  # for 320K
ggplot(long_df, aes(x = Time, y = Value, color = Replicate)) + # long_df represents the complete 50ns simulation rather than gyration_filtered
  geom_line(linewidth = 1.2) +  
  scale_color_manual(
    name = "Replicates",  
    values = blue_palette,
    labels = c("T280", "T300_1", "T300_2")  
  ) +
  labs(
    x = "Time (ns)", 
    y = bquote(~R[g]~"(nm)"), 
    title = "Radius of Gyration (50 ns Simulation)"
  ) +
  theme_bw(base_size = 14) +  
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
    #legend.position = c(1, 0.5),  
    legend.background = element_rect(fill = "white", color = "grey70"), 
    panel.grid.minor = element_blank() 
  )

