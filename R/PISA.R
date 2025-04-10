library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyr)
library(ggrepel)
library(scales)
############################# ΔG comparison #############################
# create dataframe
data <- data.frame(
  Group = c("T280", "T300", "T320", "T280", "T300", "T320"),
  Value = c(-5.9, -8.3, -8.5, 0.432, 0.303, 0.188),
  Type = c("ΔG kcal/mol", "ΔG kcal/mol", "ΔG kcal/mol", "P value", "P value", "P value")
)
# devide the data into ΔG and P value
deltaG_data <- data %>% filter(Type == "ΔG kcal/mol")
pvalue_data <- data %>% filter(Type == "P value")

# turn P value into significant signs
pvalue_data <- pvalue_data %>%
  mutate(Significance = case_when(
    Value < 0.01 ~ "***",
    Value < 0.1 ~ "**",
    Value < 0.5 ~ "*",
    TRUE ~ "ns"
  ))
# draw graph
ggplot() +
  # Draw a Δ G bar chart
  geom_col(data = deltaG_data, 
           aes(x = Group, y = Value, fill = Group),
           width = 0.7) +
  # Add P-value significance marker
  geom_text(data = pvalue_data,
            aes(x = Group, y = max(deltaG_data$Value) + 0.5, 
                label = Significance),
            size = 6) +
  # Set colors and labels
  scale_fill_brewer(palette = "Set2") +
  labs(title = "ΔG Values with P-value Significance",
       y = "ΔG (kcal/mol)",
       x = "Temperature") +
  # Adjust the theme
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none")


############################# Hbond comparison #############################
# Read the data
H<- read.xlsx("Hbond.xlsx")
colnames(H)<-c('Temperature',"Hbond_num")
# Draw the graph
ggplot(H,aes(x= Temperature,y=Hbond_num,color = Temperature)) + 
  geom_bar(stat = "identity",aes(fill= Temperature)) +
  theme_minimal() +  
  labs(title = "Number of Hbond", x = "Temperature", y = "Value")  +
  geom_text(aes(label = Hbond_num), vjust = -0.2,size=3,color ='black') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


############################# Buried Surface Area Comparison #############################
# Read the data
data <- read.xlsx("Buried_areas.xlsx")
# Transfer data format to long format
data_long <- data %>%
  pivot_longer(
    cols = T280:T320,  
    names_to = "Temperature",  
    values_to = "Value"  
  )
# Data preprocessing
data_long <- data %>%
  #Convert temperature values to numerical values and create unique residue identifiers
  pivot_longer(
    cols = T280:T320,
    names_to = "Temperature",
    values_to = "Value"
  ) %>%
  mutate(
    Temperature = parse_number(Temperature), #Extract temperature values
    Residue_ID = paste(Chain, Residue, sep = ": ") #Create a unique identifier
  ) 

# Draw the graph
ggplot(data_long, aes(x = Temperature, y = Value)) +
  geom_line(aes(color = Chain, group = Residue_ID), 
            linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(color = Chain, shape = Chain), 
             size = 2.5, alpha = 0.9) +
  geom_text_repel(
    aes(label = ifelse(Temperature == 300, Residue, "")),
    size = 3,
    color = "black",
    box.padding = 0.3,
    min.segment.length = 0,
    segment.color = "gray50",
    max.overlaps = Inf,
    direction = "both"
  ) +
  facet_wrap(~Chain, ncol = 1, scales = "free_y") +
  scale_x_continuous(
    breaks = unique(data_long$Temperature),
    labels = label_number(suffix = "K")
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(color = "gray30"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  labs(
    title = "Temperature-dependent Buried Surface Area Variations",
    x = "Temperature",
    y = expression(Buried~Surface~Area~(Å^2))
  )
