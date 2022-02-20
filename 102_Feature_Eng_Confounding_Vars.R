# Create indicator variables for _AGE_G
library(fastDummies)
library(data.table)

BRFSS_i <- dummy_cols(BRFSS_h, select_columns = 'X_AGE_G', remove_first_dummy = TRUE)

table(BRFSS_i$X_AGE_G_2, BRFSS_i$X_AGE_G)
table(BRFSS_i$X_AGE_G_3, BRFSS_i$X_AGE_G)
table(BRFSS_i$X_AGE_G_4, BRFSS_i$X_AGE_G)
table(BRFSS_i$X_AGE_G_5, BRFSS_i$X_AGE_G)
table(BRFSS_i$X_AGE_G_6, BRFSS_i$X_AGE_G)


# Rename newly made AGE indicator variables for simplicity 
setnames(BRFSS_i, old = c("X_AGE_G_2", "X_AGE_G_3", "X_AGE_G_4", "X_AGE_G_5", "X_AGE_G_6"),
         new = c("AGE2", "AGE3", "AGE4", "AGE5", "AGE6"))

table(BRFSS_i$AGE2, BRFSS_i$X_AGE_G)
table(BRFSS_i$AGE3, BRFSS_i$X_AGE_G)
table(BRFSS_i$AGE4, BRFSS_i$X_AGE_G)
table(BRFSS_i$AGE5, BRFSS_i$X_AGE_G)
table(BRFSS_i$AGE6, BRFSS_i$X_AGE_G)


# Create grouping variables and indicator variables for SMOKING 
# First an indicator variable out of SMOK100 variable <- never smokers flagged

BRFSS_j <- BRFSS_i
BRFSS_j$NEVERSMK <- 0
BRFSS_j$NEVERSMK[BRFSS_j$SMOKE100 == 2] <- 1

table(BRFSS_j$NEVERSMK, BRFSS_j$SMOKE100)


# Use NEVERSMK and SMOKDAY2 to create a Grouping variable <- based on Data Dictionary

BRFSS_j$SMOKGRP <- 9
BRFSS_j$SMOKGRP[BRFSS_j$SMOKDAY2 == 1 | BRFSS_j$SMOKDAY2 == 2] <- 1
BRFSS_j$SMOKGRP[BRFSS_j$SMOKDAY2 == 3] <- 2

table(BRFSS_j$SMOKGRP, BRFSS_j$SMOKDAY2)


# Create an indicator variable for SMOKING 
BRFSS_j <- dummy_cols(BRFSS_j, select_columns = 'SMOKGRP')
BRFSS_j$SMOKGRP_9 <- NULL
BRFSS_j$SMOKGRP_2 <- NULL
names(BRFSS_j)

setnames(BRFSS_j, old = "SMOKGRP_1", new = "SMOKER")

table(BRFSS_j$SMOKER, BRFSS_j$SMOKGRP)

# Create an indicator variable for SEX 
BRFSS_j <- dummy_cols(BRFSS_j, select_columns = 'SEX')
table(BRFSS_j$SEX_1)


# remove unwanted indicator variable
BRFSS_j$SEX_2 <- NULL
setnames(BRFSS_j, old = 'SEX_1', new = 'MALE')

# Create an indicator variable for Hispanic (_HISPANC)
BRFSS_j<- dummy_cols(BRFSS_j, select_columns = 'X_HISPANC')

BRFSS_j$X_HISPANC_2 <- NULL
BRFSS_j$X_HISPANC_9 <- NULL
setnames(BRFSS_j, old = 'X_HISPANC_1', new = 'HISPANIC')

table(BRFSS_j$X_HISPANC, BRFSS_j$HISPANIC)

# Create a new grouping variable for Race (_MRACE)
BRFSS_j$RACEGRP <- 9
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 1] <- 1
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 2] <- 2
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 3] <- 3
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 4] <- 4
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 5] <- 5
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 6] <- 6
BRFSS_j$RACEGRP[BRFSS_j$X_MRACE1 == 7] <- 6

table(BRFSS_j$RACEGRP)

# Create 3 indicator variables from RACEGRP <- BLACK, ASIAN, OTHRACE 
BRFSS_j <- dummy_cols(BRFSS_j, select_columns = 'RACEGRP')
BRFSS_j <- BRFSS_j[!(colnames(BRFSS_j) %in% c("RACEGRP_1", "RACEGRP_3", "RACEGRP_5", "RACEGRP_6", "RACEGRP_9"))]

# The 3rd indicator variable will use multiple groups so will be made separately
BRFSS_j$OTHRACE <- 0
BRFSS_j$OTHRACE[BRFSS_j$RACEGRP == 3] <- 1
BRFSS_j$OTHRACE[BRFSS_j$RACEGRP == 5] <- 1
BRFSS_j$OTHRACE[BRFSS_j$RACEGRP == 6] <- 1

table(BRFSS_j$OTHRACE, BRFSS_j$RACEGRP)


# Create a grouping variable for MARITAL status
BRFSS_j$MARGRP <- 9
BRFSS_j$MARGRP[BRFSS_j$MARITAL == 1] <- 1
BRFSS_j$MARGRP[BRFSS_j$MARITAL == 2] <- 2
BRFSS_j$MARGRP[BRFSS_j$MARITAL == 3] <- 2
BRFSS_j$MARGRP[BRFSS_j$MARITAL == 5] <- 1
BRFSS_j$MARGRP[BRFSS_j$MARITAL == 4] <- 3
BRFSS_j$MARGRP[BRFSS_j$MARITAL == 9] <- 9

table(BRFSS_j$MARGRP)

# Create two indicator groups
# Never married -> NEVERMAR
BRFSS_j$NEVERMAR <- 0
BRFSS_j$NEVERMAR[BRFSS_j$MARGRP == 3] <- 1

table(BRFSS_j$NEVERMAR, BRFSS_j$MARGRP)

# Formerly married -> FORMERMAR 
BRFSS_j$FORMERMAR <- 0
BRFSS_j$FORMERMAR[BRFSS_j$MARGRP == 2] <- 1

table(BRFSS_j$FORMERMAR, BRFSS_j$MARGRP)

# Create a new grouping variable for <- GENHLTH
BRFSS_j$GENHLTH2 <- BRFSS_j$GENHLTH
BRFSS_j$GENHLTH2[BRFSS_j$GENHLTH == 7] <- 9

table(BRFSS_j$GENHLTH2, BRFSS_j$GENHLTH)

# Create two indicator variables 
# FAIR HEALTH -> FAIRHLTH 
BRFSS_j <- dummy_cols(BRFSS_j, select_columns = 'GENHLTH2')
BRFSS_j <- BRFSS_j[!(colnames(BRFSS_j) %in% c("GENHLTH2_1", "GENHLTH2_2", "GENHLTH2_3", "GENHLTH2_9"))]
setnames(BRFSS_j, old = c( "GENHLTH2_4", "GENHLTH2_5"), new = c("FAIRHLTH", "POORHLTH"))

# Create new grouping variable for HLTHPLN 
BRFSS_j$HLTHPLN2 <- 9
BRFSS_j$HLTHPLN2[BRFSS_j$HLTHPLN1 == 1] <- 1
BRFSS_j$HLTHPLN2[BRFSS_j$HLTHPLN1 == 2] <- 2

table(BRFSS_j$HLTHPLN2, BRFSS_j$HLTHPLN1)

# Create an indicator variable for HLTPLN -> No Plan 
BRFSS_j$NOPLAN <- 0
BRFSS_j$NOPLAN[BRFSS_j$HLTHPLN2 == 2] <- 1

table(BRFSS_j$NOPLAN, BRFSS_j$HLTHPLN2)

# Create a grouping variable for EDUCA
BRFSS_j$EDGROUP <- 9
BRFSS_j$EDGROUP[BRFSS_j$EDUCA <= 3] <- 1
BRFSS_j$EDGROUP[BRFSS_j$EDUCA == 4] <- 2
BRFSS_j$EDGROUP[BRFSS_j$EDUCA == 5] <- 3
BRFSS_j$EDGROUP[BRFSS_j$EDUCA == 6] <- 4

table(BRFSS_j$EDGROUP, BRFSS_j$EDUCA)

# Create two indicator variables for LOWED and SOMECOLL 
BRFSS_j$LOWED <- 0
BRFSS_j$LOWED[BRFSS_j$EDGROUP < 3] <- 1

BRFSS_j$SOMECOLL <- 0
BRFSS_j$SOMECOLL[BRFSS_j$EDGROUP == 3] <- 1

table(BRFSS_j$LOWED, BRFSS_j$EDGROUP)
table(BRFSS_j$SOMECOLL, BRFSS_j$EDGROUP)

# Create a new grouping var for Income2 and call it income3
BRFSS_j$INCOME3 <- BRFSS_j$INCOME2
BRFSS_j$INCOME3[BRFSS_j$INCOME2 > 8] <- 9

table(BRFSS_j$INCOME3, BRFSS_j$INCOME2)

# Create indicator variables for income 
# Keep the last value as reference 
BRFSS_j <- dummy_cols(BRFSS_j, select_columns = 'INCOME3', remove_most_frequent_dummy = TRUE)

colnames(BRFSS_j)
BRFSS_j$INCOME3_9 <- NULL

setnames(BRFSS_j, old = c("INCOME3_1", "INCOME3_2", "INCOME3_3", "INCOME3_4", "INCOME3_5", "INCOME3_6", "INCOME3_7"),
         new = c("INC1", "INC2", "INC3", "INC4", "INC5", "INC6", "INC7"))

# Create a new grouping variable for OBESITY (BMI)

BRFSS_j$BMI5CAT <- BRFSS_j$X_BMI5CAT
BRFSS_j$BMI5CAT[is.na(BRFSS_j$X_BMI5CAT)] <- 9

table(BRFSS_j$BMI5CAT, BRFSS_j$X_BMI5CAT)

# Create 3 indicator variables for OBESITY (BMI)
BRFSS_j <- dummy_cols(BRFSS_j, select_columns = 'BMI5CAT')

colnames(BRFSS_j)
BRFSS_j$BMI5CAT_2 <- NULL
BRFSS_j$BMI5CAT_9 <- NULL

BRFSS_j <- setnames(BRFSS_j, old = c("BMI5CAT_1", "BMI5CAT_3", "BMI5CAT_4"), new = c("UNDWT", "OVWT", "OBESE"))

# Create a new grouping variable for EXERCISE (EXERANY2)
BRFSS_j$EXERANY3 <- BRFSS_j$EXERANY2 
BRFSS_j$EXERANY3[BRFSS_j$EXERANY2 > 2] <- 9 

table(BRFSS_j$EXERANY3, BRFSS_j$EXERANY2)

# Create an indicator variable for EXERANY3 and call it NoEXER 
BRFSS_j$NOEXER <- BRFSS_j$EXERANY3
BRFSS_j$NOEXER <- 0
BRFSS_j$NOEXER[BRFSS_j$EXERANY2 == 2] <- 1

table(BRFSS_j$NOEXER)

# All variables created 

# Export the final "Analytic" dataset 
write.csv(BRFSS_j, file = "C:/Users/azimz/Desktop/R_Linkedin/data/analytic.csv")






