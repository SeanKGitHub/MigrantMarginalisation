# Qualitative Comparative Analysis of Migrant Marginalisation across European Workforces
# Uncalibrated data in 'G_calib.csv' file

# clearing workspace
rm(list = ls())

# loading required packages
library(QCA)
library(SetMethods)
library(ggplot2)
library(dplyr)

# setting directory and loading raw data from excel file
setwd("C:/data/LFS/data/")
calibRaw <- read.csv("G_calib.csv", header = TRUE, row.names = "cn")

# Calibration of Conditions and Outcome

# Comprehensive Immigration Restrictiveness measure from Helbling (based on three pillars...see G_IMPIC.do)
  # Insepecting visually
ggplot(calibRaw, 
       aes(x = reorder(row.names(calibRaw), imCom3), y = imCom3)) +
  geom_point()

  # inspecting more closely around crossover
calibRaw[c("CH", "NO", "BE", "ES", "IT"), "imCom3"]

# calibrated condition using direct calibration method 
I <- calibrate(calibRaw$imCom3,
               type = "fuzzy",
               thresholds = c(0.24, 0.27, 0.4),
               include = TRUE,
               logistic = TRUE)

# developed welfare state. Based on Esping Anderson + Ferrera southern regime
# calibrated condition using qualitative method
W <- NA #empty variable
W[calibRaw$wr > 3.1] <-1
W[calibRaw$wr > 2.1 & calibRaw$wr < 4]<-0.67
W[calibRaw$wr > 1.1 & calibRaw$wr < 3]<-0.33
W[calibRaw$wr < 2]<-0
W

# EMPLOYMENT PROTECTION  reweighted following emmenegger for 2017. High number = high protection
# code for inspecting the distribution
ggplot(calibRaw, 
       aes(x = reorder(row.names(calibRaw), epR17), y = epR17)) +
  geom_point()

# calibrated condition using direct calibration method
ER <- calibrate(calibRaw$epR17,
                type = "fuzzy",
                thresholds = c(1.6, 2, 2.75),
                include = TRUE,
                logistic = TRUE)


# LOW SKILL. Based on isco 5 + 9 jobs over total employment, OECD 2019b and andersson 2019
ggplot(calibRaw, 
       aes(x = reorder(row.names(calibRaw), lowskill), y = lowskill)) +
  geom_point()

# thresholds relatively clear from visual inspection and fall on even numbers. crossover is simply a quarter of jobs
L <- calibrate(calibRaw$lowskill, 
               type = "fuzzy", 
               thresholds = c(22.5, 25, 27.5), 
               include = TRUE, 
               logistic = TRUE)

# OUTCOME based on odds ratios of migrant vs local working bottom quartile job (see G2_Logistic)
# controls are group-mean centred age and education, and gender
ggplot(calibRaw, 
       aes(x = reorder(row.names(calibRaw), ORc_quar), y = ORc_quar)) +
  geom_point()

O <- calibrate(calibRaw$ORc_quar, 
               type = "fuzzy", 
               thresholds = c(1.7, 2.25, 2.75),
               include = TRUE, 
               logistic = TRUE)

# combining calibrated data into data frame
calibTable <- data.frame(ER, I, W, L, O, row.names = row.names(calibRaw))
calibTable

#testing for skewness, esp. no more than 80/20% (Oana et al. 2021 2.3.1)
# Restrictive Immigration Policy
skew.check(calibTable$I,
           hist = FALSE)
# 70%

# Protected Labour Market
skew.check(calibTable$ER,
           hist = FALSE)
# 76% is a bit high but still below 20% "rule of thumb"

# Developed Welfare State
skew.check(calibTable$W,
           hist = FALSE)
# 64%

# Prominent Low-Skill jobs
skew.check(calibTable$L,
           hist = FALSE)
# 52%

# Outcome
skew.check(calibTable$O,
           hist = FALSE)
# 64%

# object containing the conditions for other qca commands 
conds <- c("ER", "I", "L", "W")


# ------------------> ANALYSIS OF NECESSITY <----------------------- 
# Testing for single necessary conditions
QCAfit(calibTable$ER, 
       calibTable$O)

QCAfit(calibTable$W, 
       calibTable$O)

QCAfit(calibTable$I, 
       calibTable$O)

QCAfit(calibTable$L, 
       calibTable$O)
# no single necessary conditions found (no consistency near 0.9)

# now for negation of outcome
QCAfit(calibTable$ER, 
       1-calibTable$O)

QCAfit(calibTable$W, 
       1-calibTable$O)

QCAfit(calibTable$I, 
       1-calibTable$O)

QCAfit(calibTable$L, 
       1-calibTable$O)
# no single necessary conditions found (no consistency near 0.9)

# Looking at combinations of conditions, following Oana ECPR summer school code
# No combinations found with Relevance of Necessity cut-off of 0.5 (recommended by Oana 2021, Schneider & Wagemann 2012)

SUIN_O2 <- superSubset(data = calibTable,
                       outcome  = "O",
                       relation = "necessity",
                       conditions = conds,
                       incl.cut = 0.9,
                       cov.cut = 0.5,
                       depth = 2)
SUIN_O2

# now for absense of high marginalisation
# no combinations found

NRLo <- superSubset(calibTable, 
                   outcome = "O", 
                  incl.cut = 0.9, 
                 cov.cut = 0.5, 
                neg.out = TRUE,
               depth = 2)
NRLo

# >-----------------------------ANALYSIS OF SUFFICIENCY --------------------- <

# building truth tables
# high marginalisation
TT <- truthTable(calibTable, 
                 outcome = "O", 
                 incl.cut1 = 0.80, 
                 show.cases = TRUE, 
                 sort.by = ("incl"), 
                 complete = TRUE)
TT
# choosing 0.8 or 0.85 as cutoff makes no difference 

# truth table for negation of outcome
TTneg <- truthTable(calibTable, 
                    outcome = "~O", 
                    incl.cut1 = 0.8, 
                    show.cases = TRUE, 
                    sort.by = ("incl"), 
                    complete = TRUE)
TTneg
# again taking 0.85 would make no difference


# logical minimisation of truth tables
# high migrant marginalisation
# conservative solution
SCc <- minimize(TT,
                details = TRUE,
                use.tilde = TRUE)
SCc

# most parsimonious solution (because of include = "?")
SCp <- minimize(TT, include = "?", 
                row.dom = TRUE, 
                details = TRUE, 
                show.cases = TRUE,
                use.tilde = TRUE)
SCp
# no further simplification

# check prime implicants
SCp$PIchart
# some model ambiguity as row 16 covered by both pathways (AT)

# negation of outcome
# conservative solution
SCnegc <- minimize(TTneg, 
                   details = TRUE, 
                   use.tilde = TRUE)
SCnegc

# check prime implicants
SCnegc$PIchart

#parsimonious solution
SCnegp <- minimize(TTneg,
                   details = TRUE,
                   use.tilde = TRUE,
                   include = "?",
                   row.dom = FALSE)
SCnegp


# plotting sufficiency

# positive outcome (high marginalisation) on XY plots
# plotting parsimonious solution using pimplot
pimplot(data = calibTable,
        outcome = "O",
        results = SCp,
        all_labels = TRUE,
        jitter = TRUE,
        fontsize = 5)

# Using XYplot with whole solution (same information as pimplot, but with ability to add titles)
XY_SOL <- XYplot(SCp$solution[[1]], O, 
                 data = calibTable, 
                 enhance = TRUE,
                 xlab = "Parsimonious Solution Formula: ER*I*L or I*W*L",
                 ylab = "Outcome: High Migrant Marginalisation",
                 clabels = rownames(calibTable),
                 cex.main=0.5, cex.lab=1.0)
title("Figure 2. XY Plot of Sufficiency", adj = 0, line = 1.5)

# graph of just I*W*L pathway
XY_IWL <- XYplot(SCp$pims[[2]], O, 
                 data = calibTable, 
                 enhance = TRUE,
                 relation = "sufficiency",
                 xlab = "I*W*L",
                 ylab = "Outcome: High Migrant Marginalisation",
                 clabels = rownames(calibTable),
                 cex.main=0.5, cex.lab=1.0)
title("Figure 3. XY Plot of Sufficiency", adj = 0, line = 1.5)

# graph of just ER*I*L pathway
XY_ERIL <- XYplot(SCp$pims[[1]], O, 
                  data = calibTable, 
                  enhance = TRUE,
                  relation = "sufficiency",
                  xlab = "ER*I*L",
                  ylab = "Outcome: High Migrant Marginalisation",
                  clabels = rownames(calibTable),
                  cex.main=0.5, cex.lab=1.0)
title("Figure 4. XY Plot of Sufficiency", adj = 0, line = 1.5)    

# negation of outcome parsimonious solution (absense of hight marginalisation)
pimplot(data = calibTable,
        outcome = "~O",
        results = SCnegp,
        all_labels = TRUE,
        jitter = TRUE,
        fontsize = 5)

# since not further interpreting, no need for graphs with titles

# Enhanced Standard Analysis (Oana et al. 2021 ch.4.4.2 p130; Schneider Wagemann 2012 Ch8.2)

# Three types of Untenable Assumptions we need to prevent the use of
# Use esa() function of SetMethods package to exclude untenable assumptions, 
# Type 1: Contradictory Simplifying Assumptions 
# Checking for contradictory simplifying assumptions used in the two parsimonious solutions ( for O and ~O) See Oana 2021 p133
CSA <- LR.intersect(SCp, SCnegp)
CSA # returns "character(0)" -  no contradictory assumptions

# Type 2: Assumptions contradicting claims of necessity
# Since no necessary causes found, not an issue

# Type 3: assumptions on impossible remainders
# No reason why any of the conditions couldn't occur in same case so no problem

