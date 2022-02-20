# Create new variables 

# Exposure Variable <- alcohol consumption 

# A grouping variable for Alcohol consumption 
BRFSS_g <- BRFSS_f

BRFSS_g$ALCGRP <- 9

BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 < 200] <- 3
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 >= 200 & BRFSS_g$ALCDAY5 < 777] <- 2
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 == 888] <- 1

table(BRFSS_g$ALCGRP, BRFSS_g$ALCDAY5)

# A set of indicator variables for pattern of drinking 
# Monthly or Daily 

BRFSS_g$DRINKMONTHLY <- 0
BRFSS_g$DRINKMONTHLY[BRFSS_g$ALCGRP == 2] <- 1

BRFSS_g$DRINKWEEKLY <- 0
BRFSS_g$DRINKWEEKLY[BRFSS_g$ALCGRP == 3] <- 1

table(BRFSS_g$DRINKMONTHLY, BRFSS_g$ALCGRP)
table(BRFSS_g$DRINKWEEKLY, BRFSS_g$ALCGRP)

# Outcome Variables <- Sleep duration, Asthma 
BRFSS_h <- BRFSS_g

BRFSS_h$SLEPTIM2 <- na.rm(BRFSS_h$SLEPTIM1)

table(BRFSS_h$SLEPTIM2, BRFSS_h$SLEPTIM1)
dim(BRFSS_h)


BRFSS_h$ASTHMA4 <- 9
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 1] <- 1 
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 2] <- 0

table(BRFSS_h$ASTHMA4, BRFSS_h$ASTHMA3)





