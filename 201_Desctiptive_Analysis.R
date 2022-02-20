# Read the final dataset
analytic <- read.csv("C:/Users/azimz/Desktop/R_Linkedin/Data/analytic.csv")

# Create frequency tables for ASTHMA4 -> categorical response/outcome variable 
AsthmaFreq <- table(analytic$ASTHMA4)
AsthmaFreq
write.csv(AsthmaFreq, "C:/Users/azimz/Desktop/R_Linkedin/Data/AsthmaFreq.csv")

# What proportion of our dataset has Asthma?
PropAsthma <- 5343/52788
PropAsthma

# Look at the categorical outcome ASTHMA4 by exposure variable ALCGRP 
AsthmaAlcFreq <- table(analytic$ASTHMA4, analytic$ALCGRP)
AsthmaAlcFreq
write.csv(AsthmaAlcFreq, "C:/Users/azimz/Desktop/R_Linkedin/Data/AsthmaAlcFreq.csv")

# Visualize the frequencies 
library(ggplot2)

df <- read.csv("AsthmaFreq.csv")
as.data.frame(df)

ggplot(df, aes(x = "", y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  coord_polar("y", start = 0)

# Review continuous outcome variable Sleep duration 
summary(analytic$SLEPTIM2)

hist(analytic$SLEPTIM2, 
     col="Yellow", 
     xlim = c(0, 15),
     ylim = c(0, 20000))

boxplot(analytic$SLEPTIM2, 
        col = "Cyan")

boxplot(SLEPTIM2 ~ ALCGRP, data = analytic,
        col = "green")


# Macros
library(gtools)

# Define a Macro 
# Takes in a Output table name, variable name and csv file name 
# Outputs a csv file for frequencies of that variable 
FreqTable <- defmacro(OutputTable, InputVar, CSVTable, 
                      expr = {
                        OutputTable <- as.data.frame(table(InputVar));
                        write.csv(OutputTable, file = paste0(CSVTable, ".csv"))
                      })

# Use the macro 
FreqTable(AgeTable, analytic$X_AGE_G, "Age")

# We can do the same with R FUNCTION 
FreqTable2 <- function(OutputTable, InputVar, CSVTable){
                        OutputTable <- as.data.frame(table(InputVar));
                        write.csv(OutputTable, file = paste0(CSVTable, ".csv"))}

FreqTable2(AgeTable, analytic$X_AGE_G, "Age2")


# Calculate means and SDs

mean(analytic$SLEPTIM2)
sd(analytic$SLEPTIM2)

# Calculate mean and SD of Sleep duration for each level in each categorical variable
library(plyr)
ddply(analytic, ~ALCGRP, summarise, mean = mean(SLEPTIM2), sd = sd(SLEPTIM2))

# Create a Macro to do it for all variables 
library(gtools)

SumTbl <- defmacro(OutputTable, GroupVar, CSVTable,
                   expr={
                     OutputTable <- ddply(analytic,~GroupVar,summarise,mean=mean(SLEPTIM2),sd=sd(SLEPTIM2));
                     write.csv(OutputTable, file = paste0(CSVTable, ".csv"))
                   })

SumTbl(ALCTable, analytic$ALCGRP, "ALCOHOL")
























