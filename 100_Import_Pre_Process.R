# Call required libraries 
library(gtools)
library(foreign)
library(dplyr)
library(questionr)
library(MASS)

# Obtain BRFSS-2014 dataset along with corresponding data dictionary and codebook
# Read XPT dataset into R

BRFSS_a <- read.xport("LLCP2014.XPT")

# Check out the column names in the dataset
colnames(BRFSS_a)

# To keep necessary native variables 
# First create a list of variables 

varlist <- c(
  "VETERAN3",
  "ALCDAY5",
  "SLEPTIM1",
  "ASTHMA3",
  "X_AGE_G",
  "SMOKE100",
  "SMOKDAY2",
  "SEX",
  "MARITAL",
  "X_HISPANC",
  "X_MRACE1",
  "GENHLTH",
  "HLTHPLN1",
  "EDUCA",
  "INCOME2",
  "X_BMI5CAT",
  "EXERANY2"
)

# Create a new df with only 16 selected columns 
BRFSS_b <- BRFSS_a[varlist]

# Check  out the dimensions of the new df -> check the row numbers with codebook
dim(BRFSS_b)
colnames(BRFSS_b)


# Inclusion and Exclusion 
# Decide which rows to remove 
nrow(BRFSS_b)

# First exclusion
# Everyone who is not in our Subpopulation
# Remove all non-veterans 

BRFSS_c <- subset(BRFSS_b, VETERAN3==1)

table(BRFSS_b$VETERAN3)
table(BRFSS_c$VETERAN3)

nrow(BRFSS_c)


# Now focus on Exposure variables 
# Remove rows without a VALID answers 

BRFSS_d <- subset(BRFSS_c, ALCDAY5<777|ALCDAY5==888)

nrow(BRFSS_d)
table(BRFSS_d$ALCDAY5)

# Asthma3 
BRFSS_e <- subset(BRFSS_d, ASTHMA3 <7)

nrow(BRFSS_e)
table(BRFSS_e$ASTHMA3)

BRFSS_f <- subset(BRFSS_e, SLEPTIM1 <77)
nrow(BRFSS_f)








