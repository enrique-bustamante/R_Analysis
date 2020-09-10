# Import libraries
library(tidyverse)

# Import MechaCar_mpg data
MechaCar_mpg <- read.csv("MechaCar_mpg.csv", check.names = T)

# Review the first few rows of data to get an idea of the data set
head(MechaCar_mpg)

# Use linear model to run multiple lineear regressions to analyze each factor on mpg
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = MechaCar_mpg)

# Use linear model within summary to get p-value
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = MechaCar_mpg)
)

# Import Suspension coil data
Suspension_Coil <- read.csv('Suspension_Coil.csv')

# Create a summary table for suspension data
suspension_summary <- Suspension_Coil %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance_PSI = var(PSI), SD_PSI = sd(PSI))
suspension_summary

# Create a summary table by lot for suspension data
suspension_summary_by_lot <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance_PSI = var(PSI), SD_PSI = sd(PSI))
suspension_summary_by_lot

# Run T-test to see is there is a significant difference between this set and the population
t.test(Suspension_Coil$PSI, mu=1500)

