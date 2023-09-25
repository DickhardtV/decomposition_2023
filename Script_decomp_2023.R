
library(ggplot2)

# Set working directory to the projekt
setwd("D:/Uni/0_Master_Pys_Geogr/3_Semester/Arctic Ecosystems/0_Projektwork")
getwd()

# Reading in the table
table <- read.table(file = "./data/decomp_2023_long.csv", 
                  header = TRUE, dec=",", sep=";")

table<- subset(table, !is.na(relative.mass.remaining))

# Does a Subset just for the data of the year 2023
# When you put an hashtag before the next command, you get an overview over all of the years
table<- subset(table, YR_retrieved=="2023")

# Calculates the mass loss of buried tea bags in %
# Formular: ((Start weight tea only - litter dry weight) / Start weight tea only)*100
massloss_perc <- ((table$startW_tea.only-table$litter_DW)/table$startW_tea.only)*100

# puts the mass loss in % to the table and creates a new table
new_table <- cbind(table, massloss_perc)

# calculates the years, how long the teabags were buried
years_undergroud <- as.character(new_table$YR_retrieved-new_table$YR_buried)
new_table <- cbind(new_table, years_undergroud)


# throws out all masslosses under 0 %
negativ_massloss <- subset(new_table, massloss_perc < 0 )
new_table <- new_table[new_table$massloss_perc >= 0, ]

# Create a boxplot for the two sites for every teatype seperated; good overview
ggplot(new_table, aes(x = as.factor(years_undergroud), y = massloss_perc, fill = TEATYPE)) +
  geom_boxplot() +
  facet_wrap(~ location, scales = "free_x") +
  labs(
    title = "Mass loss by years buried and teatype",
    x = "Years buried",
    y = "Mass loss in %",
    fill = "Teatype"
  ) + 
  scale_fill_manual(values=c("limegreen", "yellow2", "sandybrown"))+
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),# Adjust axis label text size
    legend.text = element_text(size = 14),        # Adjust legend text size
    legend.title = element_text(size = 16),       # Adjust legend title text size
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),  # Change the grid color# Change the background color
    text = element_text(family = "Arial")  # Change the font family to Arial
    # Adjust title text size and style
  )

# This plot shows the Mass loss for all teatypes for the two locations by years buried
ggplot(new_table, aes(x = as.factor(years_undergroud), y = massloss_perc, fill = location)) +
  geom_boxplot() +
  facet_wrap(~ location, scales = "free_x") +
  labs(
    title = "Mass loss by years buried for all teatypes for both Locations",
    x = "Years buried",
    y = "Mass loss in %",
    fill = "Location"
  ) + 
  scale_fill_manual(values=c("sandybrown", "yellow2"))+
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),# Adjust axis label text size
    legend.text = element_text(size = 14),        # Adjust legend text size
    legend.title = element_text(size = 16),       # Adjust legend title text size
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),  # Change the grid color# Change the background color
    text = element_text(family = "Arial")  # Change the font family to Arial
    # Adjust title text size and style
  )+
  stat_summary(
    fun = "median",   # Use the median function
    geom = "line",    # Draw a line
    aes(group = 1),   # Connect all medians
    color = "grey20", # Line color
    size = 1.1 ,      # Line size
    alpha = 0.9       # Transperancy of Line
  ) 
  
# Massloss for every location
ggplot(new_table, aes(x =location, y = massloss_perc, fill=location)) +           
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none")+
  labs(
    title = "Mass loss of geen tea by years burried in Nissanjok",
    x = "Years buried",
    y = "Mass loss [%]",
    fill ="Location"
  )+
  scale_fill_manual(values=c("sandybrown", "yellow2"))+
  geom_jitter(
    width = 0.2,    # Adjust the width of jitter
    alpha = 0.7,    # Transparency of data points
    color = "black" # Color of data points
  ) +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),# Adjust axis label text size
    legend.text = element_text(size = 14),        # Adjust legend text size
    legend.title = element_text(size = 16),       # Adjust legend title text size
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),  # Change the grid color# Change the background color
    text = element_text(family = "Arial")  # Change the font family to Arial
    # Adjust title text size and style
  )
#########################################################################
################# Statistical Tests #####################################
#########################################################################

# Subsample of Nissanjokk
sub_nis   <- subset(new_table, new_table$location=="Nissanjok")
sub_nis_1 <- subset(sub_nis, sub_nis$Years.of.burial==1)
sub_nis_2 <- subset(sub_nis, sub_nis$Years.of.burial==2)
sub_nis_3 <- subset(sub_nis, sub_nis$Years.of.burial==3)
# Subsample of Katterjokk
sub_kat   <- subset(new_table, new_table$location=="Katterjok")
sub_kat_1 <- subset(sub_kat, sub_kat$Years.of.burial==1)
sub_kat_2 <- subset(sub_kat, sub_kat$Years.of.burial==2)
sub_kat_3 <- subset(sub_kat, sub_kat$Years.of.burial==3)

sub_green   <- subset(new_table, new_table$TEATYPE=="GREEN")
sub_green_1 <- subset(sub_green, sub_green$Years.of.burial==1)
sub_green_2 <- subset(sub_green, sub_green$Years.of.burial==2)
sub_green_3 <- subset(sub_green, sub_green$Years.of.burial==3)

sub_roi   <- subset(new_table, new_table$TEATYPE=="ROOIBOS")
sub_roi_1 <- subset(sub_roi, sub_roi$Years.of.burial==1)
sub_roi_2 <- subset(sub_roi, sub_roi$Years.of.burial==2)
sub_roi_3 <- subset(sub_roi, sub_roi$Years.of.burial==3)

sub_lit   <- subset(new_table, new_table$TEATYPE=="LOCAL")
sub_lit_1 <- subset(sub_lit, sub_lit$Years.of.burial==1)
sub_lit_2 <- subset(sub_lit, sub_lit$Years.of.burial==2)
sub_lit_3 <- subset(sub_lit, sub_lit$Years.of.burial==3)

sub_1 <-     subset(new_table, new_table$Years.of.burial==1)
sub_2 <-     subset(new_table, new_table$Years.of.burial==2)
sub_3 <-     subset(new_table, new_table$Years.of.burial==3)

distr_list <- list(nis1=sub_nis_1, nis2=sub_nis_2, nis3=sub_nis_3,
                   kat1=sub_kat_1, kat2=sub_kat_2, kat3=sub_kat_3,
                   gre1=sub_green_1, gre2=sub_green_2, gre3=sub_green_3,
                   roi1=sub_roi_1, roi2=sub_roi_2, roi3=sub_roi_3,
                   lit1=sub_lit_1, lit2=sub_lit_2, lit3=sub_lit_3)

results_df <- data.frame(
  Distribution = character(),
  KS_p_value = double(),
  stringsAsFactors = FALSE
)


library(purrr)
library(dplyr)
library(car)

for (i in 1:length(distr_list)) {
 a <- distr_list[[i]]$relative.mass.remaining
 
 # Perform KS test for normality
 ks_result <- ks.test(a, "pnorm",mean=mean(a), sd=sd(a)) 

 name <- names(distr_list)[i]
 
 results_df <- bind_rows(results_df, data.frame(
   Distribution = name,
   KS_p_value = ks_result$p.value
 ))
 
}
# Data is normal distributed, when p-value > 0.05

levene.test(subset_data$MassLossPercent, group = subset_data$Location)

# ANOVAS for the years buried

# One year buried Teabags
anova_model_1 <- aov(massloss_perc ~ location*TEATYPE, data = sub_1)
summary(anova_model_1)

# Two years 
anova_model_2 <- aov(massloss_perc ~ location*TEATYPE, data = sub_2)
summary(anova_model_2)

# Three years
anova_model_3 <- aov(massloss_perc ~ location*TEATYPE, data = sub_3)
summary(anova_model_3)

# There is an significant difference, when p-value is under 0.05

model_1 <- lm(sub_green$massloss_perc~sub_green$Years.of.burial)
model_1
plot(sub_green$Years.of.burial, sub_green$massloss_perc)
abline(model_1)
summary(model_1)

model_2 <- lm(sub_roi$massloss_perc~sub_roi$Years.of.burial)
plot(sub_roi$Years.of.burial, sub_roi$massloss_perc)
abline(model_2)
summary(model_2)


model_3 <- lm(sub_lit$massloss_perc~sub_lit$Years.of.burial)
plot(sub_lit$Years.of.burial, sub_lit$massloss_perc)
abline(model_3)
summary(model_3)


plot1 <- ggplot(sub_roi, aes(x = Years.of.burial, y = massloss_perc)) +
  geom_point() +   # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear model line
  xlab("X") +
  ylab("Y") +
  ggtitle("Scatterplot with Linear Model Line")

plot2 <- ggplot(sub_green, aes(x = Years.of.burial, y = massloss_perc)) +
  geom_point() +   # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Linear model line
  xlab("X") +
  ylab("Y") +
  ggtitle("Scatterplot with Linear Model Line")

plot3 <- ggplot(sub_lit, aes(x = Years.of.burial, y = massloss_perc)) +
  geom_point() +   # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +  # Linear model line
  xlab("X") +
  ylab("Y") +
  ggtitle("Scatterplot with Linear Model Line")

library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol = 2)


# Create a boxplot for the two sites for every teatype seperated; good overview
ggplot(new_table, aes(x = as.factor(years_undergroud), y = massloss_perc, fill = TEATYPE)) +
  geom_boxplot() +
  labs(
    title = "Mass loss by years buried and teatype",
    x = "Years buried",
    y = "Mass loss in %",
    fill = "Teatype"
  ) + 
  scale_fill_manual(values=c("limegreen", "yellow2", "sandybrown"))+
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),# Adjust axis label text size
    legend.text = element_text(size = 14),        # Adjust legend text size
    legend.title = element_text(size = 16),       # Adjust legend title text size
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 14),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey90"),  # Change the grid color# Change the background color
    text = element_text(family = "Arial")  # Change the font family to Arial
    # Adjust title text size and style
  )+
  geom_abline(intercept = 30.36, slope = 8.29,  color = "yellow2", linetype = "dotted", size = 1.1)+
  geom_abline(intercept = 23.946, slope = 6.048,  color = "sandybrown", linetype = "dotted", size = 1.1)+
  geom_abline(intercept = 48.548, slope = 8.184,  color = "limegreen", linetype = "dotted", size = 1.1)


