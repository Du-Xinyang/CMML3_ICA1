setwd("D:/CMML3/ICA/R_result/DSSP/")
library(tidyr)
# Receives DSSP.xvg (here renamed to T320.svg)
#### Change the input when changing temperature. 
T320 <- read.table("T320.xvg", sep = "" , header = FALSE , skip = 33, na.strings = "",
                   stringsAsFactors = FALSE)
colnames(T320)<-c("Time","α-helix","β-bridge","β-sheet","3_10-helix","Π-helix","PPII helix","bend","turn","break","loop")
data_long <- pivot_longer(T320, cols = -Time, names_to = "Structure", values_to = "Count")

# Draw stacked bar chart of secondary structure
ggplot(data_long, aes(x = Time, y = Count, fill = Structure)) +
  geom_bar(stat = "identity") +
  labs(title = "320K Number of Secondary Structures Over Time",
       x = "Time (ps)",
       y = "Number of Structures",
       fill = "Secondary Structures") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
