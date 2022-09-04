# Robustness Checks of Migrant Marginalisation QCA (see G4_QCA):
  # Runs analysis while including fifth causal condition, integration policy (based on MIPEX). See supplementary material for more information
  # Sensitivity ranges for calibration
  # Sensitivity ranges for raw consistency threshold
  # Create and combining test solutions using alternative calibrations for comparison with solution
  # Calculate Robustness Parameters

# After the MIPEX check, the data is cleared and a new dataset used. This is because there were issues with
# 'rob.calibrange' command whereby it wouldn't finish running using the initial small raw values of some conditions 
# and the outcome
# To facilitate the function running but preserve the proportions, the OECD EPL index (epR17), outcome (ORc_quar) and immigration policy index(imCom3) scores were all multiplied by 100, and lowskill multiplied by 10
# The data for these robustness checks featuring these scores are found in excel file 'G_Robust.csv'
  
rm(list = ls())

# setting directory and loading raw data from excel file
setwd("C:/data/LFS/data/")
calibRaw <- read.csv("G_calib.csv", header = TRUE, row.names = "cn")

# loading required packages
library(QCA)
library(SetMethods)
library(ggplot2)
library(dplyr)

# Calibration of Conditions and Outcome

# MIPEX grouped countries. Based on https://www.mipex.eu/key-findings (last accessed 21/03/2022)

M <- NA #empty variable
M[calibRaw$MIPEXg > 3.1] <-1
M[calibRaw$MIPEXg > 2.1 & calibRaw$MIPEXg < 4]<-0.67
M[calibRaw$MIPEXg > 1.1 & calibRaw$MIPEXg < 3]<-0.33
M[calibRaw$MIPEXg < 2]<-0
M

I <- calibrate(calibRaw$imCom3,
               type = "fuzzy",
               thresholds = c(0.24, 0.27, 0.4),
               include = TRUE,
               logistic = TRUE)

ER <- calibrate(calibRaw$epR17,
                type = "fuzzy",
                thresholds = c(1.6, 2, 2.75),
                include = TRUE,
                logistic = TRUE)


# LOW SKILL

L <- calibrate(calibRaw$lowskill, 
               type = "fuzzy", 
               thresholds = c(22.5, 25, 27.5), 
               include = TRUE, 
               logistic = TRUE)

# welfare state development
W <- NA #empty variable
W[calibRaw$wr > 3.1] <-1
W[calibRaw$wr > 2.1 & calibRaw$wr < 4]<-0.67
W[calibRaw$wr > 1.1 & calibRaw$wr < 3]<-0.33
W[calibRaw$wr < 2]<-0
W

# OUTCOME

O <- calibrate(calibRaw$ORc_quar, 
               type = "fuzzy", 
               thresholds = c(1.7, 2.25, 2.75),
               include = TRUE, 
               logistic = TRUE)

# combining calibrated data into data frame
calibTable <- data.frame(ER, M, I, W, L, O, row.names = row.names(calibRaw))
calibTable

conds <- c("ER", "I", "L", "W", "M")

# NECESSITY TESTING 
# Testing for single necessary conditions
QCAfit(calibTable$M, 
       calibTable$O)

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
QCAfit(calibTable$M, 
       1-calibTable$O)

QCAfit(calibTable$ER, 
       1-calibTable$O)

QCAfit(calibTable$W, 
       1-calibTable$O)

QCAfit(calibTable$I, 
       1-calibTable$O)

QCAfit(calibTable$L, 
       1-calibTable$O)

SUIN_O2 <- superSubset(data = calibTable,                    # combinations of conditions
                       outcome  = "O",
                       relation = "necessity",
                       conditions = conds,
                       incl.cut = 0.9,
                       cov.cut = 0.5,
                       depth = 2)
SUIN_O2

# now for absense of high disadvantage
NRLo <- superSubset(calibTable, 
                    outcome = "O", 
                    incl.cut = 0.9, 
                    cov.cut = 0.5, 
                    neg.out = TRUE,
                    depth = 2)
NRLo

TT <- truthTable(calibTable, 
                 outcome = "O", 
                 incl.cut1 = 0.8, 
                 show.cases = TRUE, 
                 sort.by = ("incl"), 
                 complete = TRUE)
TT

TTneg <- truthTable(calibTable, 
                    outcome = "~O", 
                    incl.cut1 = 0.8, 
                    show.cases = TRUE, 
                    sort.by = ("incl"), 
                    complete = TRUE)
TTneg

# conservative solution
SCc <- minimize(TT,
                details = TRUE,
                use.tilde = TRUE)
SCc
# more complex than main conservative solution

# most parsimonious solution
SCp <- minimize(TT, include = "?", 
                row.dom = TRUE, 
                details = TRUE, 
                show.cases = TRUE,
                use.tilde = TRUE)
SCp
# most parsimonious solution is the same as that in main analysis (with MIPEX excluded)


SCnegc <- minimize(TTneg, 
                   details = TRUE, 
                   use.tilde = TRUE)
SCnegc


#parsimonious solution
SCnegp <- minimize(TTneg,
                   details = TRUE,
                   use.tilde = TRUE,
                   include = "?",
                   row.dom = FALSE)
SCnegp

# plotting solutions
pimplot(data = calibTable,
        outcome = "O",
        results = SCp,
        all_labels = TRUE,
        jitter = TRUE,
        fontsize = 5)

pimplot(data = calibTable,
        outcome = "~O",
        results = SCnegp,
        all_labels = TRUE,
        jitter = TRUE,
        fontsize = 5)

# **********************************  END OF MIPEX CHECK ****************************************

# clearing workspace
rm(list = ls())

# loading raw data with multiplied values for robustness checks from excel file
calibRaw <- read.csv("G_Robust.csv", header = TRUE, row.names = "cn")

  # calibrating conditions and outcome as per main analysis

I <- calibrate(calibRaw$imCom3,
               type = "fuzzy",
               thresholds = c(24, 27, 40),
               include = TRUE,
               logistic = TRUE)

W <- NA #empty variable
W[calibRaw$wr > 3.1] <-1
W[calibRaw$wr > 2.1 & calibRaw$wr < 4]<-0.67
W[calibRaw$wr > 1.1 & calibRaw$wr < 3]<-0.33
W[calibRaw$wr < 2]<-0
W

ER <- calibrate(calibRaw$epR17,
                type = "fuzzy",
                thresholds = c(160, 200, 275),
                include = TRUE,
                logistic = TRUE)

L <- calibrate(calibRaw$lowskill, 
               type = "fuzzy", 
               thresholds = c(225, 250, 275), 
               include = TRUE, 
               logistic = TRUE)

O <- calibrate(calibRaw$ORc_quar, 
               type = "fuzzy", 
               thresholds = c(170, 225, 275),
               include = TRUE, 
               logistic = TRUE)

# combining calibrated data into data frame
calibTable <- data.frame(ER, I, W, L, O, row.names = row.names(calibRaw))
calibTable

# object containing the conditions 
conds <- c("ER", "I", "L", "W")

## SUFFICIENCY TESTING
# building truth tables
  # outcome
TT <- truthTable(calibTable, 
                 outcome = "O", 
                 incl.cut1 = 0.8, 
                 show.cases = TRUE, 
                 sort.by = ("incl"), 
                 complete = TRUE)
TT

  # negation of outcome
TTneg <- truthTable(calibTable, 
                    outcome = "~O", 
                    incl.cut1 = 0.8, 
                    show.cases = TRUE, 
                    sort.by = ("incl"), 
                    complete = TRUE)
TTneg

# logical minimisation
  # outcome
    # conservative solution
SCc <- minimize(TT,
                details = TRUE,
                use.tilde = TRUE)
SCc

    # parsimonious solution (include = "?")
SCp <- minimize(TT, include = "?", 
                row.dom = TRUE, 
                details = TRUE, 
                show.cases = TRUE,
                use.tilde = TRUE)
SCp

  # negation of outcome
    # conservative solution
SCnegc <- minimize(TTneg, 
                   details = TRUE, 
                   use.tilde = TRUE)
SCnegc

    #parsimonious solution
SCnegp <- minimize(TTneg,
                   details = TRUE,
                   use.tilde = TRUE,
                   include = "?",
                   row.dom = FALSE)
SCnegp




# Robustness Checks
# Create second raw data table 
RawData <- data.frame(calibRaw[, c("epR17", "imCom3", "wr", "lowskill", "ORc_quar")])

# determine sensitivity ranges for calibration----------------------------------------------------------------------------

  # employment protection (original values multiplied by 100)
rob.calibrange(raw.data = calibRaw,
               calib.data = calibTable,
               test.cond.raw = "epR17",
               test.cond.calib = "ER",
               test.thresholds = c(160, 200, 275),
               step = 5,
               max.runs = 100,
               outcome = "O",
               conditions = conds,
               incl.cut = 0.8,
               n.cut = 1,
               include = "?")
# Return:
# Exclusion:  Lower bound  NA Threshold  160 Upper bound  195
# Crossover:  Lower bound  175 Threshold  200 Upper bound  210 
# Inclusion:  Lower bound  200 Threshold  275 Upper bound  NA 

  # immigration policy. Original values multiplied by 100, comprehensive measure
rob.calibrange(raw.data = RawData,
               calib.data = calibTable,
               test.cond.raw = "imCom3",
               test.cond.calib = "I",
               test.thresholds = c(24, 27, 40),
               step = 1,
               max.runs = 50,
               outcome = "O",
               conditions = conds,
               incl.cut = 0.8,
               n.cut = 1,
               include = "?")

# Exclusion:  Lower bound  NA Threshold  24 Upper bound  26 
# Crossover:  Lower bound  25 Threshold  27 Upper bound  30 
# Inclusion:  Lower bound  32 Threshold  40 Upper bound  NA  


  # low skill employment. Data multiplied by 10
rob.calibrange(raw.data = RawData,
               calib.data = calibTable,
               test.cond.raw = "lowskill",
               test.cond.calib = "L",
               test.thresholds = c(225, 250, 275),
               step = 0.5,
               max.runs = 120,
               outcome = "O",
               conditions = conds,
               incl.cut = 0.8,
               n.cut = 1,
               include = "?")
# Exclusion:  Lower bound  NA Threshold  225 Upper bound  249.5 
# Crossover:  Lower bound  241.5 Threshold  250 Upper bound  257.5 
# Inclusion:  Lower bound  259.5 Threshold  275 Upper bound  NA


  # outcome*100
rob.calibrange(raw.data = RawData,
               calib.data = calibTable,
               test.cond.raw = "ORc_quar",
               test.cond.calib = "O",
               test.thresholds = c(170, 225, 275),
               step = 0.5,
               max.runs = 120,
               outcome = "O",
               conditions = conds,
               incl.cut = 0.8,
               n.cut = 1,
               include = "?")


# Exclusion:  Lower bound  155.5 Threshold  170 Upper bound  224.5 
# Crossover:  Lower bound  217 Threshold  225 Upper bound  236 
# Inclusion:  Lower bound  225 Threshold  275 Upper bound  NA 

# determine sensitivity ranges for raw consistency threshold-------------------------------------------------------------------
rob.inclrange(data = calibTable,
              step = 0.01,
              max.runs = 102,
              outcome = "O",
              conditions = conds,
              incl.cut = 0.8,
              n.cut = 1,
              include = "?")
# Raw Consistency T.:  Lower bound  0.77 Threshold  0.8 Upper bound  0.8 

################## CREATING TEST SOLUTIONS ##################################################################-----------


############# imCom3 with more exclusive 30-----------------------------------------------------------------------------------
It <- calibrate(calibRaw$imCom3,
                type = "fuzzy",
                thresholds = c(25, 30, 40),
                include = TRUE,
                logistic = TRUE)

calibTable_It <- data.frame(ER, It, W, L, O, row.names = row.names(calibRaw))

conds_It <- c("ER", "It", "L", "W")

TT_It <- truthTable(calibTable_It, 
                    outcome = "O", 
                    incl.cut1 = 0.8, 
                    show.cases = TRUE, 
                    sort.by = ("incl"), 
                    complete = TRUE)
TT_It


TTneg_It <- truthTable(calibTable_It, 
                       outcome = "~O", 
                       incl.cut1 = 0.8, 
                       show.cases = TRUE, 
                       sort.by = ("incl"), 
                       complete = TRUE)
TTneg_It

SCc_It <- minimize(TT_It,
                   details = TRUE,
                   use.tilde = TRUE)
SCc_It

SCp_It <- minimize(TT_It,
                   details = TRUE,
                   include = "?",
                   use.tilde = TRUE)
SCp_It

# M1: ~ER*It*W + ER*It*L -> O 
# M2: ~ER*W*L + ER*It*L -> O 
# M3: ER*It*~W + It*W*L -> O 
# M4: ER*It*L + It*W*L -> O 

# --------------------------------- 
#               inclS   PRI   covS   covU   (M1)   (M2)   (M3)   (M4)   cases 
#--------------------------------------------------------------------------- 
#  1  ~ER*It*W  0.878  0.782  0.214  0.007  0.104                       DK 
#  2   ~ER*W*L  0.869  0.776  0.201  0.031         0.091                DK 
#  3  ER*It*~W  0.922  0.882  0.236  0.005                0.085         GR 
#  4   ER*It*L  0.901  0.864  0.266  0.019  0.156  0.156         0.062  GR; AT 
#  5    It*W*L  0.898  0.855  0.263  0.000                0.112  0.060  DK; AT 
#--------------------------------------------------------------------------- 
#           M1  0.892  0.840  0.370 
#           M2  0.888  0.837  0.356 
#           M3  0.892  0.839  0.349 
#           M4  0.898  0.861  0.325 


# Has same two pathways in main anaylsis but also more model ambiguity 
# Only GR AT DK explained, IT and ES no longer explained

# negation of outcome
SCnegp_It <- minimize(TTneg_It,
                      details = TRUE,
                      use.tilde = TRUE,
                      include = "?",
                      row.dom = FALSE)
SCnegp_It
# no difference with negation of outcome


#### LOW SKILL EMPLOYMENT ALTERNATIVE CALIBRATION ----------------------------------------------------------------------

Lt <- calibrate(calibRaw$lowskill, 
                type = "fuzzy", 
                thresholds = c(225, 265, 275), 
                include = TRUE, 
                logistic = TRUE)

calibTable_Lt <- data.frame(ER, I, W, Lt, O, row.names = row.names(calibRaw))

conds_Lt <- c("ER", "I", "Lt", "W")

TT_Lt <- truthTable(calibTable_Lt, 
                    outcome = "O", 
                    incl.cut1 = 0.8, 
                    show.cases = TRUE, 
                    sort.by = ("incl"), 
                    complete = TRUE)
TT_Lt

TTneg_Lt <- truthTable(calibTable_Lt, 
                       outcome = "~O", 
                       incl.cut1 = 0.8, 
                       show.cases = TRUE, 
                       sort.by = ("incl"), 
                       complete = TRUE)
TTneg_Lt


SCc_Lt <- minimize(TT_Lt,
                   details = TRUE,
                   use.tilde = TRUE)
SCc_Lt
# Austria no longer in solution

SCp_Lt <- minimize(TT_Lt,
                   details = TRUE,
                   use.tilde = TRUE,
                   include = "?")
SCp_Lt

# more model ambiguity than main model and austria no longer explained
# Still has ER*I*L pathway, and W*I*L pathway simplified to W*L

# M1: W*Lt + ER*I*~W -> O 
# M2: W*Lt + ER*I*Lt -> O 
# M3: ~ER*I*W + ER*I*~W -> O 
# M4: ~ER*I*W + ER*I*Lt -> O 

# --------------------------------- 
#            inclS   PRI   covS   covU   (M1)   (M2)   (M3)   (M4)   cases 
# ---------------------------------------------------------------------------- 
#1     W*Lt  0.812  0.713  0.302  0.044  0.147  0.103                DK 
#2  ~ER*I*W  0.864  0.762  0.221  0.013                0.107  0.114  DK 
#3  ER*I*~W  0.938  0.915  0.302  0.014  0.147         0.188         ES,GR,IT 
#4  ER*I*Lt  0.962  0.950  0.310  0.019         0.112         0.203  ES,GR,IT 
# ---------------------------------------------------------------------------- 
  #  M1  0.833  0.754  0.449 
  #  M2  0.843  0.782  0.413 
  #  M3  0.882  0.832  0.409 
  #  M4  0.910  0.872  0.424 
  
SCnegc_Lt <- minimize(TTneg_Lt,
                        details = TRUE,
                        use.tilde = TRUE,
                        row.dom = FALSE)
SCnegc_Lt

SCnegp_Lt <- minimize(TTneg_Lt,
                      details = TRUE,
                      use.tilde = TRUE,
                      include = "?",
                      row.dom = FALSE)
SCnegp_Lt

# RECALIBRATING OUTCOME with way more inclusive cutoff --------------------------------------------------------------
Ot <- calibrate(calibRaw$ORc_quar, 
                type = "fuzzy", 
                thresholds = c(170, 200, 275),
                include = TRUE, 
                logistic = TRUE)

calibTable_Ot <- data.frame(ER, I, W, L, Ot, row.names = row.names(calibRaw))

TT_Ot <- truthTable(calibTable_Ot, 
                    outcome = "Ot", 
                    incl.cut1 = 0.8, 
                    show.cases = TRUE, 
                    sort.by = ("incl"), 
                    complete = TRUE)
TT_Ot

TTneg_Ot <- truthTable(calibTable_Ot, 
                       outcome = "~Ot", 
                       incl.cut1 = 0.8, 
                       show.cases = TRUE, 
                       sort.by = ("incl"), 
                       complete = TRUE)
TTneg_Ot
# no sufficient rows

SCc_Ot <- minimize(TT_Ot,
                   details = TRUE,
                   use.tilde = TRUE)
SCc_Ot


SCp_Ot <- minimize(TT_Ot,
                   details = TRUE,
                   use.tilde = TRUE,
                   include = "?")
SCp_Ot
# Switzerland Portugal Norway and Sweden now included in solution

# Firstly, this is quite inclusive def. of high marginalisation 
skew.check(calibTable_Ot$Ot)
# 76.47%

# I*W*L is still there
# ~I*~L, ER*~W are new

# M1: ER*~W + ~I*~L + I*W*L -> Ot

#         inclS   PRI   covS   covU   cases 
#-------------------------------------------------- 
#1  ER*~W  0.891  0.848  0.338  0.148  PT; ES,GR,IT 
#2  ~I*~L  0.787  0.685  0.317  0.201  CH; NO,SE 
#3  I*W*L  0.960  0.947  0.266  0.099  DK; AT 
#-------------------------------------------------- 
#  M1  0.870  0.826  0.661 


SCnegp_Ot <- minimize(TTneg_Ot,
                      details = TRUE,
                      use.tilde = TRUE,
                      include = "?",
                      row.dom = FALSE)
SCnegp_Ot
# no values explained

### TESTING OUTCOME WITH A HIGHER CALIBRATION. more plausible than 200 --------------------------------------------------------------------
Ot2 <- calibrate(calibRaw$ORc_quar, 
                 type = "fuzzy", 
                 thresholds = c(170, 250, 275),
                 include = TRUE, 
                 logistic = TRUE)

calibTable_Ot2 <- data.frame(ER, I, W, L, Ot2, row.names = row.names(calibRaw))

TT_Ot2 <- truthTable(calibTable_Ot2, 
                     outcome = "Ot2", 
                     incl.cut1 = 0.8, 
                     show.cases = TRUE, 
                     sort.by = ("incl"), 
                     complete = TRUE)
TT_Ot2
# denmark no longer in out group

TTneg_Ot2 <- truthTable(calibTable_Ot2, 
                        outcome = "~Ot2", 
                        incl.cut1 = 0.8, 
                        show.cases = TRUE, 
                        sort.by = ("incl"), 
                        complete = TRUE)
TTneg_Ot2

SCc_Ot2 <- minimize(TT_Ot2,
                    details = TRUE,
                    use.tilde = TRUE)
SCc_Ot2

SCp_Ot2 <- minimize(TT_Ot2,
                    details = TRUE,
                    use.tilde = TRUE,
                    include = "?")
SCp_Ot2
# so still have the ER*I*L pathway but other one not there anymore
# denmark no longer explained

#M1: ER*I*L -> Ot2

#           inclS   PRI   covS   covU   cases 
# --------------------------------------------------- 
#1  ER*I*L  0.860  0.812  0.377    -    ES,GR,IT; AT 
#--------------------------------------------------- 

skew.check(calibTable_Ot2$Ot2) # 41%

SCnegp_Ot2 <- minimize(TTneg_Ot2,
                       details = TRUE,
                       use.tilde = TRUE,
                       include = "?",
                       row.dom = FALSE)
SCnegp_Ot2

############# creating test set with consistency set to 0.85------------------------------------------------------

calibTable_cons <- data.frame(ER, I, W, L, O, row.names = row.names(calibRaw))

TT_cons <- truthTable(calibTable_cons, 
                      outcome = "O", 
                      incl.cut1 = 0.85, 
                      show.cases = TRUE, 
                      sort.by = ("incl"), 
                      complete = TRUE)
TT_cons


TTneg_cons <- truthTable(calibTable_cons, 
                         outcome = "~O", 
                         incl.cut1 = 0.85, 
                         show.cases = TRUE, 
                         sort.by = ("incl"), 
                         complete = TRUE)
TTneg_cons

SCc_cons <- minimize(TT_cons,
                     details = TRUE,
                     use.tilde = TRUE)
SCc_cons

SCp_cons <- minimize(TT_cons,
                     details = TRUE,
                     include = "?",
                     use.tilde = TRUE)
SCp_cons

# the two I found are still there along with some extras adding model ambiguity

# M1: ~ER*I*W + ER*I*L -> O 
# M2: ~ER*W*L + ER*I*L -> O 
# M3: ER*I*~W + I*W*L -> O 
# M4: ER*I*L + I*W*L -> O 

#--------------------------------- 
#              inclS   PRI   covS   covU   (M1)   (M2)   (M3)   (M4)   cases 
#-------------------------------------------------------------------------------- 
#1  ~ER*I*W  0.864  0.762  0.221  0.011  0.108                       DK 
#2  ~ER*W*L  0.869  0.776  0.201  0.028         0.088                DK 
#3  ER*I*~W  0.938  0.915  0.302  0.014                0.143         ES,GR,IT 
#4   ER*I*L  0.919  0.896  0.332  0.019  0.220  0.220         0.112  ES,GR,IT; AT 
#5    I*W*L  0.891  0.850  0.284  0.004                0.126  0.064  DK; AT 
#-------------------------------------------------------------------------------- 
#  M1  0.899  0.859  0.441 
#  M2  0.903  0.868  0.420 
#  M3  0.901  0.864  0.428 
#  M4  0.904  0.877  0.396 





# *********************** combining test sets ****************************************

# Combining test solutions for comparison with solution

# rob.fit() command unable to include solutions with alternative outcome (SCp_Ot and/or SCp_Ot2) in Test Set (TS), so excluding 
# e.g., creating test set object with the code:  TS <- list(SCp_Lt, SCp_Ot, SCp_Ot2, SCp_cons)

TS <- list(SCp_It, SCp_cons, SCp_Lt)

# Calculate Robustness Parameters
RF <- rob.fit(test_sol = TS,
              initial_sol = SCp,
              outcome = "O")
RF
# return:
#                 RF_cov RF_cons RF_SC_minTS RF_SC_maxTS
# Robustness_Fit  0.745   0.968       0.664       0.695

# for negated outcome
TSn <- list(SCnegp_It, SCnegp_Lt )

RFn <- rob.fit(test_sol = TSn,
               initial_sol = SCnegp,
               outcome = "~O")
RFn
#                  RF_cov RF_cons RF_SC_minTS RF_SC_maxTS
# Robustness_Fit      1       1       0.999       0.919

################## # Case-oriented robustness. Folllowing Oana et al. 2021 p.158 #####################
# positive outcome

rob.xyplot(test_sol = TS,
           initial_sol = SCp,
           all_labels = TRUE,
           outcome = "O",
           jitter = TRUE,
           fontsize = 3.5,
           labs = TRUE)

# Upper left quadrant is most problematic and is empty. Bottom right quadrant indicates 'shaky type' cases

# negated outcome
rob.xyplot(test_sol = TSn,
           initial_sol = SCnegp,
           all_labels = TRUE,
           outcome = "~O",
           jitter = TRUE,
           fontsize = 3.5,
           labs = TRUE)

# Given the lack of cases in the problematic upper-left quadrants, and the SSR parameters equalling 1,
# one can say the Test solution and Initial are in a subset relation with eachother with regard to the relevant case types (for both
# outcome and negation of the outcome), and that Initial Solution(IS) is a superset of Test Solution(TS) (Oana p.158)

# Examining causal chain with immigration policy and welfare state
PAK_chain <- causalChain(data = calibTable,
                         ordering = "I",
                         sol.cons = 0.8,
                         include = "?",
                         row.dom = TRUE)
PAK_chain                                 #  no causal chains in data
PAK_chain <- causalChain(data = calibTable,
                         ordering = "W",
                         sol.cons = 0.8,
                         include = "?",
                         row.dom = TRUE)
PAK_chain                                 #  no causal chains in data



