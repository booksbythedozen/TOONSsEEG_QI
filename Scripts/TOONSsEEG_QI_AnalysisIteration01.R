# TOONSsEEG_QI.R
# Quality initiative to evaluate patient experience of sEEG mapping
# The Ottawa Operative Neuropsychology System (TOONS)
# Bryce P Mulligan, PhD, CPsych
# 17 June 2026
# ------------------------------------------------------------------------------
# remove all elements for a clean start (careful, now!)

# rm(list=ls(all=TRUE))  # clears the R environment
# dev.off()            # clears all plots
# cat("\014")            # clears R terminal
# ------------------------------------------------------------------------------
# Upgrade R (run periodically)

# library (installr) # for updating R
# updateR()
# ------------------------------------------------------------------------------
# load packages from library
library(rethinking)   # McElreath (2020) Statistical Rethinking
library(dagitty)      # graphical analysis of structural causal models
library(ggplot2)      # elegant data visualisation
library(gridExtra)    # arrange multiple objects on a page
library(ggalluvial)   # for alluvial/Sankey diagrams
library(scales)       # scale functions for visualisation (e.g. breaks_pretty() )
library(tm)           # import, clean, transform, and analyse textual data
library(wordcloud)    # create word clouds
library(ggwordcloud)  # for plotting word clouds as ggplot geoms
library(topicmodels)  # for Latent Dirichlet Allocation of text data
library(textstem)     # for stemming and lemmatizing text
library(janitor)      # for data cleaning
library(slam)         # data structures and algorithms for sparse arrays/matrices
library(RColorBrewer) # predefined, accessible colour palettes
library(dplyr)        # a grammar of data manipulation
library(reshape2)     # for restructuring data (melt / cast)
library(readr)        # read rectangular text data (like 'csv')
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## Phase Zero: Background & Rationale

# 0.0: Decision making in epilepsy neurosurgery

# Patients who are in the process of presurgical assessment for treatment of
# epilepsy undergo myriad assessments and consultations. In theory, they accrue
# information from experts and reflect on their options in relation to their
# personal values and priorities to reach a decision about whether or not to
# proceed with epilepsy neurosurgery.

# It is important to note that the individual patient's prior state is likely to
# influence how they respond to the sEEG as a decision-making intervention. For
# instance, Skowron et al. (2025) recently demonstrated that more accurate (less
# biased) decisions are made because people start out with broad priors and then
# refine their models of the latent decision space following successive
# encounters with decision-salient data (i.e. rather than individual differences
# in belief updating, per se).

# There is very little research specifically pertinent to the presurgical
# epilepsy patient decision-making experience. The present quality initiative
# sought to evaluate the impact of stereoelectroencephalographic electrical
# stimulation mapping on patient decision making in relation to epilepsy
# neurosurgery through an evaluation of our established practice.

# Our primary objectives were to:
# 1) consider whether and how sEEG mapping leads to decision progress
       # a) via addressing unmet decision-making needs
       # b) via causing changes in hopes and/or expectations
# 2) study the types and valuing of reasons patients prefer/avoid surgical treatment
# 3) determine whether patients/clinicians find the mapping studies useful

# 0.1: A naive causal model of decision making for epilepsy neurosurgery

# Decision progress occurs when decision making needs are addressed. Decision
# needs are queried on the OPDG. In particular, the experience of sEEG mapping
# would be expected to increase Knowledge and Certainty; however, it is also
# possible that patients would experience an increase in sufficient Support as
# well as a clarification of the relevant Values. Here, decision progress (DP)
# is operationalised as moving up the ordinal scale on the OPDG "how far along
# are you...?" item or in expressing a changing response on the OPDG "which
# option do you prefer?" item.

# ESM = electrical stimulation mapping 
#   K = knowledge
#   V = values
#   S = support
#   C = certainty
#  DP = decision progress
#   U = unobserved (unmeasured) mediating variables

dag_0.1 <- dagitty( "dag {
    ESM -> K
    ESM -> V
    ESM -> S
    ESM -> C
    ESM -> U
      K -> DP
      V -> DP
      S -> DP
      C -> DP
      U -> DP
      U [unobserved]
    }")
coordinates( dag_0.1 ) <- list(x=c(ESM=0,   K=1, V=1, S=1, C=1, U=1, DP=2  ) ,
                                   y=c(ESM=1.5, K=0, V=1, S=2, C=3, U=1.5, DP=1.5  ) )
drawdag( dag_0.1 )
# blank(bty="n") # this does some magical reset of graphical device after datitty

# We presume that patients are more likely to express a preference for the
# surgical treatment option when they have high hopes and/or expectations
# related to surgery. Here, hopes and expectations are measured using the PHEQ,
# and OPDG responses will show when patients express a preference for surgery vs
# medical treatment options. ESM may cause changes in hopes or expectations that
# could in turn lead to expression of a preference for surgery.

# ESM = electrical stimulation mapping 
#   H = hope
#   E = expectation
#  PS = prefer surgery
#   U = unobserved (unmeasured) mediating variables

dag_0.2 <- dagitty( "dag {
    ESM -> H
    ESM -> E
    ESM -> U
      H -> PS
      E -> PS
      U -> PS
      U [unobserved]
    }")
coordinates( dag_0.2 ) <- list(x=c(ESM=0,   H=1, E=1, U=1, PS=2  ) ,
                               y=c(ESM=0.5, H=0, E=1, U=0.5, PS=0.5  ) )
drawdag( dag_0.2 )
# blank(bty="n") # this does some magical reset of graphical device after datitty

# How might hopes and expectations relate to decision-making needs?
# That's a DAG for another day.

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## I: Import data & Transform variables
d0 <- read.csv("./Data/TOONSsEEG_QI_RawData_AnalysisIteration01.csv")

# Transform variables to appropriate classes for visualisation analysis
d1 <- d0
d1$ID <- as.factor(d1$ID) # patient ID number
d1$PrePost <- factor(d1$PrePost, 
                     levels=c("Pre","Post"), 
                     ordered=TRUE)

d1$FarAlong <- factor(d1$FarAlong, 
                      levels=1:4,                        # Explicit numeric order
                      labels=c("Not thinking about it",  # Corresponding text labels
                               "Thinking about it",
                               "Close to choosing",
                               "Made a choice"), 
                      ordered=TRUE)                      # Make it an ordered factor

d1$Prefer[d1$Prefer == ""] <- NA   # convert empty cells to NA
d1$Prefer <- factor(d1$Prefer)     # convert variable to factor

d1$Knowledge[d1$Knowledge == ""] <- NA   # convert empty cells to NA
d1$Knowledge <- factor(d1$Knowledge)     # convert variable to factor

d1$Values[d1$Values == ""] <- NA   # convert empty cells to NA
d1$Values <- factor(d1$Values)     # convert variable to factor

d1$Support[d1$Support == ""] <- NA   # convert empty cells to NA
d1$Support <- factor(d1$Support)     # convert variable to factor

d1$Certainty[d1$Certainty == ""] <- NA   # convert empty cells to NA
d1$Certainty <- factor(d1$Certainty)     # convert variable to factor

# ------------------------------------------------------------------------------
# II: Summarise & Describe (contingency tables and alluvial plots)
# ------------------------------------------------------------------------------
precis(d0)
# precis(d1) # throws an error because some variables are "ordered" = TRUE and "factor" = TRUE

# Decision progress
ggplot(d1,
       aes(x = PrePost, stratum = FarAlong, alluvium = ID,
           fill = FarAlong, label = FarAlong)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.6) +
  geom_stratum(alpha = 0.8) +
  # geom_text(stat = "stratum", size = 4, color = "white") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(breaks = breaks_pretty()) + # ensures integer-like spacing
  theme_minimal() +
  labs(title = "Change in Decision Progress from Before to After sEEG Mapping",
       x = "Measurement Occasion", y = "Number of Individuals")

t <- table(d1$FarAlong, d1$PrePost)
addmargins(t)


# Treatment preference
ggplot(d1,
       aes(x = PrePost, stratum = Prefer, alluvium = ID,
           fill = Prefer, label = Prefer)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.6) +
  geom_stratum(alpha = 0.8) +
  # geom_text(stat = "stratum", size = 4, color = "white") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(breaks = breaks_pretty()) + # ensures integer-like spacing
  theme_minimal() +
  labs(title = "Change in Treatment Preference from Before to After sEEG Mapping",
       x = "Measurement Occasion", y = "Number of Individuals")

t <- table(d1$Prefer, d1$PrePost)
addmargins(t)

## Decision-making needs
# Overall needs

MetNeeds = (
  ifelse(d1$Knowledge == "Yes", 1L, 0L) +
    ifelse(d1$Values == "Yes", 1L, 0L) +
    ifelse(d1$Support == "Yes", 1L, 0L) +
    ifelse(d1$Certainty == "Yes", 1L, 0L)
)
PP <- as.integer(d1$PrePost)
table(MetNeeds, PP)

# Individual needs
t <- table(d1$Knowledge, d1$PrePost)
addmargins(t)
t <- table(d1$Values, d1$PrePost)
addmargins(t)
t <- table(d1$Support, d1$PrePost)
addmargins(t)
t <- table(d1$Certainty, d1$PrePost)
addmargins(t)

# ------------------------------------------------------------------------------
# Reset graphics device
dev.off()
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Infer
# ------------------------------------------------------------------------------
# A) What is the effect of sEEG stimulation mapping on the measures of interest?
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Hopes and Expectations
# p.130
# For now, we will assume that summary scores from the PHEQ can be treated as
# continuous (i.e. interval/ratio) data. We simply want to see if there is a
# change in these scores from pre-sEEG to post-sEEG. We will also take the
# opportunity to do some prior predictive simulation.

# First, compute total scores for Hopes and Expectations
d1[,21:34] # Hope item scores
d1[,35:48] # Expectation item scores

# rowMeans computes the average of the 14 variables for each row, automatically
# adjusting the denominator based on how many non-missing values exist in that
# row (case).
d1$Hope <- rowMeans(d1[, 21:34], na.rm = TRUE)
d1$Expect <- rowMeans(d1[, 35:48], na.rm = TRUE)

dens(d1$Hope)
precis(d1$Hope)

dens(d1$Expect)
precis(d1$Expect)

# Create data list with standardised variables
dat_list <- list(
  H = standardize(d1$Hope),
  E = standardize(d1$Expect),
  ID = as.integer(d1$ID),
  PP = as.integer(d1$PrePost)
)

## R code 5.3
m0.0 <- ulam(
  alist(
    H ~ dnorm( muH , sigmaH ) ,          # likelihood
    E ~ dnorm( muE , sigmaE ) ,          # likelihood
    muH <- aH[ID] + bH[PP] ,             # linear model
    muE <- aE[ID] + bE[PP] ,             # linear model
    aH[ID] ~ dnorm( 0 , 1 ) ,            # priors
    aE[ID] ~ dnorm( 0 , 1 ) , 
    bH[PP] ~ dnorm( 0 , 0.5 ) ,          # more restrictive prior
    bE[PP] ~ dnorm( 0 , 0.5 ) ,          # more restrictive prior  
    sigmaH ~ dexp( 1 ) ,      
    sigmaE ~ dexp( 1 )  
  ) , data = dat_list, chains=4, cores=4, iter=1e3 )

traceplot(m0.0)

# Now let's examine the posterior for the effects of interest
plot(precis(m0.0, 2, pars=c("bH", "bE")))
# There appears to be a slight increase in hopes and expectations following
# sEEG, but we will need to examine the difference scores to be sure.

# Extract samples from the posterior
post <- extract.samples(m0.0)

diffs <- list(
  PPdiffH = post$bH[,2] - post$bH[,1],
  PPdiffE = post$bE[,2] - post$bE[,1]
)
labels <- c("Hopes", "Expectations")
plot( precis(diffs) , xlab="Expected Standardised PHEQ Change (Post-Pre)", labels=labels)

# There is a minuscule amount of data, but we can see trends suggestive of
# increases in both hopes and expectations after sEEG. There is a lot of
# variability, however.

# Finally, let's do a posterior predictive check.
# We will actually have to fit individual models for each outcome. So repeating
# a lot of what was done above in a single model, but useful for validating the
# 2 outcomes in a single model approach.

# We will also use complete cases instead of data imputation to save time.
# For this iteration, that means dropping cases with ID numbers 5 and 6.
# Identify which rows to keep (exclude certain IDs)
ids_to_drop <- c(5, 6)  # IDs you want to remove
keep_idx <- !(dat_list$ID %in% ids_to_drop)

# Subset all elements of the list
dat_list1 <- lapply(dat_list, function(x) x[keep_idx])

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Hopes
## R code 5.3
m0.0H <- ulam(
  alist(
    H ~ dnorm( muH , sigmaH ) ,          # likelihood
    muH <- aH[ID] + bH[PP] ,             # linear model
    aH[ID] ~ dnorm( 0 , 1 ) ,            # priors
    bH[PP] ~ dnorm( 0 , 0.5 ) ,          # more restrictive prior
    sigmaH ~ dexp( 1 ) 
  ) , data = dat_list1, chains=4, cores=4, iter=1e3 )

# Now let's examine the posterior for the effects of interest
plot(precis(m0.0H, 2, pars=c("bH")))

## R code 5.5
# compute percentile interval of mean
PP_seq <- c(1,2)
muH <- link( m0.0H , data=list(PP=PP_seq) )
muH.mean <- apply( muH , 2, mean )
muH.PI <- apply( muH , 2 , PI )

# Extract samples from the posterior
postH <- extract.samples(m0.0H)

diffs <- list(
  PPdiffH = post$bH[,2] - post$bH[,1]
)
labels <- c("Hopes")
plot( precis(diffs) , xlab="Expected Standardised PHEQ Change (Post-Pre)", labels=labels)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Expectations
## R code 5.3
m0.0E <- ulam(
  alist(
    E ~ dnorm( muE , sigmaE ) ,          # likelihood
    muE <- aE[ID] + bE[PP] ,             # linear model
    aE[ID] ~ dnorm( 0 , 1 ) ,             # priors
    bE[PP] ~ dnorm( 0 , 0.5 ) ,          # more restrictive prior  
    sigmaE ~ dexp( 1 )  
  ) , data = dat_list1, chains=4, cores=4, iter=1e3 )

# Now let's examine the posterior for the effects of interest
plot(precis(m0.0E, 2, pars=c("bE")))

## R code 5.5
# compute percentile interval of mean
PP_seq <- c(1,2)
muE <- link( m0.0E , data=list(PP=PP_seq) )
muE.mean <- apply( muE , 2, mean )
muE.PI <- apply( muE , 2 , PI )

# Extract samples from the posterior
postE <- extract.samples(m0.0E)

diffs <- list(
  PPdiffE = post$bE[,2] - post$bE[,1]
)
labels <- c("Expectations")
plot( precis(diffs) , xlab="Expected Standardised PHEQ Change (Post-Pre)", labels=labels)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# plot it all
par(mfrow=c(2,1)) # plots in 2 rows, 1 column

# Hopes
plot( H ~ PP , data=dat_list1 , col=rangi2 , xaxt = "n" )
axis(side = 1, at = seq(1,2, by = 1), labels=levels(d1$PrePost))
lines( PP_seq , muH.mean , lwd=2 )
shade( muH.PI , PP_seq )
# Expectations
plot( E ~ PP , data=dat_list1 , col=rangi2 , xaxt = "n" )
axis(side = 1, at = seq(1,2, by = 1), labels=levels(d1$PrePost))
lines( PP_seq , muE.mean , lwd=2 )
shade( muE.PI , PP_seq )

par(mfrow=c(1,1)) # reset (plots in 1 row, 1 column)

# Results are exactly the same, but now we can use postcheck (sigh...)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
postcheck(m0.0H)
abline(v = c(2.5, 4.5, 6.5), col = "black" , lwd = 1) # add vertical lines to demarcate separate participants
postcheck(m0.0E)
abline(v = c(2.5, 4.5, 6.5), col = "black" , lwd = 1) # add vertical lines to demarcate separate participants
# Blue points are observed scores for each row in the data. Open points, the
# vertical black lines within them, and the crosses are expected proportions,
# 89% intervals of the expectation, and 89% interval of simulated samples,
# respectively.

# You can see that there is shrinkage toward person/grand mean.
# # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# # Decision progress
# # p.392
# # Unlike a count, the differences in value of an ordered categorical scale are
# # not necessarily equal. It might be much harder to move someone’s preference
# # for fish from 1 to 2 than it is to move it from 5 to 6. Just treating ordered
# # categories as continuous measures is not a good idea.
# 
# # Luckily, there is a standard and accessible solution. In principle, an ordered
# # categorical variable is just a multinomial prediction problem (page 366). But
# # the constraint that the categories be ordered demands a special treatment.
# # What we’d like is for any associated predictor variable, as it increases, to
# # move predictions progressively through the categories in sequence. So for
# # example if preference for ice cream is positively associated with years of
# # age, then the model should sequentially move predictions upwards as age
# # increases: 3 to 4, 4 to 5, 5 to 6, etc. This presents a challenge: how to
# # ensure that the linear model maps onto the outcomes in the right order.
# 
# # The conventional solution is to use a cumulative link function. The cumulative
# # probability of a value is the probability of that value or any smaller value.
# # In the context of ordered categories, the cumulative probability of 3 is the
# # sum of the probabilities of 3, 2, and 1. Ordered categories by convention
# # begin at 1, so a result less than 1 has no probability at all. By linking a
# # linear model to cumulative probability, it is possible to guarantee the
# # ordering of the outcomes.
# 
# # Step 1 is to explain how to parameterize a distribution of outcomes on the
# # scale of log-cumulative-odds. Step 2 is to introduce a predictor (or more than
# # one predictor) to these log-cumulative-odds values, allowing you to model
# # associations between predictors and the outcome while obeying the ordered
# # nature of prediction.
# 
simplehist( as.integer(d1$FarAlong) , xlim=c(1,4) , xlab="Decision Progress" )

col1 <- d1$FarAlong[as.integer(d1$PrePost) == 1]
col2 <- d1$FarAlong[as.integer(d1$PrePost) == 2]
m <- cbind(col1, col2)
# simplehist fails because of unrepresented values..?
# simplehist( na.omit(m) , xlab="Decision Progress", ylab="Raw Frequency", xaxt = "n" )
# axis(side = 1, at = seq(1,4, by = 1), labels=levels(d_sim$FarAlong))
col1 <- d1$FarAlong[as.integer(d1$PrePost) == 1]
col2 <- d1$FarAlong[as.integer(d1$PrePost) == 2]
t1 <- table(col1)
t2 <- table(col2)
barplot(rbind(t1, t2), 
        beside = TRUE,
        xlab = "Decision Progress",
        ylab = "Raw Frequency",
        legend.text = c("Pre", "Post"),
        names.arg = levels(d1$FarAlong))


# discrete proportion of each response value
pr_k <- table( as.integer(d1$FarAlong) ) / nrow(d1)

# cumsum converts to cumulative proportions
cum_pr_k <- cumsum( pr_k )

# plot
plot( 1:4 , cum_pr_k , type="b" , xlab="Decision Progress" ,
      ylab="cumulative proportion" , ylim=c(0,1) )

# Then to re-describe the histogram as log-cumulative odds, we’ll need a series
# of intercept parameters. Each intercept will be on the log-cumulative-odds
# scale and stand in for the cumulative probability of each outcome. So this is
# just the application of the link function.

# We can compute these intercept parameters directly:
## R code 12.15
logit <- function(x) log(x/(1-x)) # convenience function
round( lco <- logit( cum_pr_k ) , 2 )

# plot
plot( 1:4 , lco , type="b" , xlab="Decision Progress" ,
      ylab="log-cumulative-odds" )

# In code form, the link function will be embedded in the likelihood function
# already. This makes the calculations more efficient and avoids forcing you to
# code all the routine intermediate calculations above. So to fit the basic
# model, incorporating no predictor variables:
## R code 12.16
m0.1 <- ulam(
  alist(
    D ~ dordlogit( 0 , cutpoints ),
    cutpoints ~ dnorm( 0 , 1.5 )
  ) , data=list( D=na.omit(as.integer(d1$FarAlong) )), # must exclude NAs
  chains=4 , cores=4 )

#The posterior distribution of the cutpoints is on the log-cumulative-odds
#scale:
# R code 12.18
precis( m0.1 , depth=2 )

# To get cumulative probabilities back:
## R code 12.19
round( inv_logit(coef(m0.1)) , 3 )

# You fit a model with predictors just as you’d expect, by adding the slopes and
# predictor variables to the phi parameter inside dordlogit:
## R code 12.24
dat <- list(
  D = as.integer(d1$FarAlong),
  ID = as.integer(d1$ID),
  PP = as.integer(d1$PrePost)
)

# We will also use complete cases instead of data imputation to save time.
# For this iteration, that means dropping cases with ID numbers 5 and 6.
# Identify which rows to keep (exclude certain IDs)
ids_to_drop <- c(5, 6)  # IDs you want to remove
keep_idx <- !(dat$ID %in% ids_to_drop)

# Subset all elements of the list
dat1 <- lapply(dat, function(x) x[keep_idx])

# Fit the model
m0.2 <- ulam(
  alist(
    D ~ dordlogit( phi , cutpoints ),
    phi <- a[ID] + b[PP] ,
    a[ID] ~ dnorm( 0 , 10 ),
    b[PP] ~ dnorm( 0 , 10 ),
    cutpoints ~ dnorm( 0 , 1.5 )
  ) , data=dat1 , chains=4 , cores=4 , iter=1e4)

traceplot(m0.2)

plot( precis(m0.2, 2) )
precis( m0.2 , 2)

plot( precis(m0.2, 2, pars="b") )
precis(m0.2, 2, pars="b")

# Another plotting option is to show the implied histogram of outcomes. All we
# have to do is use sim to simulate posterior outcomes:
## R code 12.29
kPP <- 1:2   # values of PrePost to calculate over
pdat <- data.frame(PP=kPP)
s <- sim( m0.2 , data=pdat )
simplehist( s , xlab="Decision Progress", xaxt = "n" )
axis(side = 1, at = seq(1,4, by = 1), labels=levels(d1$FarAlong))

col1 <- s[,1]
col2 <- s[,2]
t1 <- table(col1)
t2 <- table(col2)
barplot(rbind(t1, t2), 
        beside = TRUE,
        xlab = "Decision Progress",
        ylab = "Posterior Frequency",
        legend.text = c("Pre", "Post"),
        names.arg = levels(d1$FarAlong))


# 
# # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Treatment preference
# p.366
# First, restructure the data
str(d1$Prefer)

# Count the number of cases for each category at each measurement occasion
category_counts <- aggregate(d1$Prefer,
                             by = list(group = d1$PrePost,
                                       category = d1$Prefer),
                             FUN = length)
# Rename the columns
colnames(category_counts) <- c("PrePost", "Prefer", "Count")

# Create a complete grid of categories and groups
complete_grid <- expand.grid(group = levels(d1$PrePost),
                             category = levels(d1$Prefer))
# Rename the columns
colnames(complete_grid) <- c("PrePost", "Prefer")

# Merge the count data with the complete grid
final_counts <- merge(complete_grid, category_counts,
                      by = c("PrePost", "Prefer"), all.x = TRUE)

# Replace NAs with zeros
final_counts[is.na(final_counts$Count), "Count"] <- 0

# Print the counts
print(final_counts)

# Reschape the data to wide format
wide_counts <- reshape(final_counts,
                       idvar = "PrePost", timevar = "Prefer", direction = "wide")

# Rename columns for clarity
colnames(wide_counts) <- gsub("Count.", "", colnames(wide_counts))

# Verify
print(wide_counts)
str(wide_counts)

# Create data list
dat <- list(
  U = as.integer(wide_counts$Unsure), # Prefer = Unsure
  # M = as.integer(wide_counts$Medical), # Prefer = Medical  
  S = as.integer(wide_counts$Surgery), # Prefer = Surgical
 PP = as.integer(wide_counts$PrePost)
)

# Poisson model of overall treatment preferences from before to after sEEG
m_0.3 <- ulam(
  alist(
    U ~ dpois(lambdaU),
    # M ~ dpois(lambdaM),  
    S ~ dpois(lambdaS),
    log(lambdaU) <- aU,               # individual linear models for each category
    # log(lambdaM) <- aM,
    log(lambdaS) <- aS,
    # c(aU, aM, aS) ~ dnorm(0,1.5)      # priors
    c(aU, aS) ~ dnorm(0,1.5)      # priors
  ), data=dat , chains=3 , cores=3 )

plot(precis(m_0.3))
precis(m_0.3)

# Verify values
k <- coef(m_0.3)
# aU <- k['aU']; aM <- k['aM']; aS <- k['aS']
aU <- k['aU']; aS <- k['aS']
# print(c(exp(aU),exp(aM),exp(aS)))
print(c(exp(aU),exp(aS)))
wide_counts

# Does it make sense to estimate effect of PrePost when there are only single cells?
m_0.4 <- ulam(
  alist(
    U ~ dpois(lambdaU),
    # M ~ dpois(lambdaM),
    S ~ dpois(lambdaS),
    log(lambdaU) <- aU + bU[PP],         # individual linear models for each category
    # log(lambdaM) <- aM + bM[PP],
    log(lambdaS) <- aS + bS[PP],
    # c(aU, aM, aS) ~ dnorm(0,1.5),        # priors
    c(aU, aS) ~ dnorm(0,1.5),        # priors
    bU[PP] ~ dnorm(0,1),
    # bM[PP] ~ dnorm(0,1),
    bS[PP] ~ dnorm(0,1)
  ), data=dat , chains=3 , cores=3 )

plot(precis(m_0.4, 2))
precis(m_0.4, 2)
# Nice!!!!

trankplot(m_0.4)
traceplot(m_0.4)

# Extract samples from the posterior
post <- extract.samples(m_0.4)

# inverse-logit to transform back to outcome (probability) scale
p_Unsure_b <- exp( post$bU )
# p_Medical_b <- exp( post$bM )
p_Surgical_b <- exp( post$bS )
plot( precis( as.data.frame(p_Unsure_b) ) ,xlab="Estimated Count" )
# plot( precis( as.data.frame(p_Medical_b) ) ,xlab="Estimated Count" )
plot( precis( as.data.frame(p_Surgical_b) ) ,xlab="Estimated Count" )

# Now compute and plot Pre/Post difference scores
diffs <- list(
  PPdiffU = post$bU[,2] - post$bU[,1],
  # PPdiffM = post$bM[,2] - post$bM[,1],
  PPdiffS = post$bS[,2] - post$bS[,1]
)
# labels <- c("Unsure", "Medical", "Surgical")
labels <- c("Unsure", "Surgical")
plot( precis(diffs) , xlab="Log-Difference Score (Post-Pre)", labels=labels)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
## Decision-making needs

# Met needs (sum of 4 decision-making needs)
# trimmed data list
dat_list <- list(
  MetNeeds = (
             ifelse(d1$Knowledge == "Yes", 1L, 0L) +
             ifelse(d1$Values == "Yes", 1L, 0L) +
             ifelse(d1$Support == "Yes", 1L, 0L) +
             ifelse(d1$Certainty == "Yes", 1L, 0L)
             ),
  ID = as.integer(d1$ID),
  PrePost = as.integer(d1$PrePost) )

# Plot raw data
dens(dat_list1$MetNeeds[dat_list1$PrePost == 1], lwd=3, xlab="Decision-Making Needs Met",
     xlim = c(0, 4), ylim = c(0, 5))
dens(dat_list1$MetNeeds[dat_list1$PrePost == 2], lwd=3, col=rangi2, add=TRUE)


# We will also use complete cases instead of data imputation to save time.
# For this iteration, that means dropping cases with ID numbers 5 and 6.
# Identify which rows to keep (exclude certain IDs)
ids_to_drop <- c(5, 6)  # IDs you want to remove
keep_idx <- !(dat_list$ID %in% ids_to_drop)

# Subset all elements of the list
dat_list1 <- lapply(dat_list, function(x) x[keep_idx])

## R code 13.29
set.seed(13)
mN0.0 <- ulam(
  alist(
    MetNeeds ~ dbinom( 4 , p ) ,
    logit(p) <- a_bar + z[ID]*sigma_a + # actor intercepts
                b[PrePost] ,
    b[PrePost] ~ dnorm( 0 , 0.5 ),
    z[ID] ~ dnorm( 0 , 1 ),
    a_bar ~ dnorm( 0 , 1 ),
    sigma_a ~ dexp(1),
    gq> vector[ID]:a <<- a_bar + z*sigma_a
  ) , data=dat_list1 , chains=4 , cores=4 )

trankplot(mN0.0)
traceplot(mN0.0)

# Examine the posterior
plot(precis(mN0.0, 2, pars="b")) # parameter estimates on the logit (log-odds) scale
precis(mN0.0, 2, pars="b")

# Extract samples from the posterior
post <- extract.samples(mN0.0)

diffs <- list(
  PPdiffN = post$b[,2] - post$b[,1]
)
labels <- c("Met Needs")
plot( precis(diffs) , xlab="Expected Change in Met Needs (Post-Pre)", labels=labels)

## R code 12.29
kPP <- 1:2   # values of PrePost to calculate over
pdat <- data.frame(PrePost=kPP)
s <- sim( mN0.0 , data=pdat )
# simplehist( s , xlab="Decision-Making Needs Met" ) # I can't figure out how to show counts for zero :/
simplehist( s[,1])
simplehist( s[,2])
dens(s[,1], lwd=3, xlab="Decision-Making Needs Met")
dens(s[,2], lwd=3, col=rangi2, add=TRUE)

postcheck(mN0.0)
abline(h = 0.5, col = "red", lwd = 1 , lty = 2)
abline(v = c(2.5, 4.5, 6.5), col = "black" , lwd = 1) # add vertical lines to demarcate separate participants
# ------------------------------------------------------------------------------
## Decision-making pros and cons (free text analysis)

# Preprocess the text to create a Document-Term Matrix (DTM):

d1$document_id <- 1:nrow(d1)

# Identify text vector
text_vector <- d1$Surg_P

# Split the vector at semicolons and flatten into a single vector
split_text_vector <- unlist(strsplit(text_vector, ";"))

# Remove leading/trailing whitespace
split_text_vector <- trimws(split_text_vector)

# Create a text corpus
corpus <- Corpus(VectorSource(split_text_vector))

# Define a list of words to manually remove
# UserWords <- c("surgery", "surgical", "medication")
UserWords <- c("surgery", "surgical")

# Clean the text data
corpus <- tm_map(corpus, content_transformer(tolower))   # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)              # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                  # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))  # Remove common stop words
# corpus <- tm_map(corpus, stemDocument)                 # apply stemming
corpus <- tm_map(corpus, content_transformer(lemmatize_strings)) # apply lemmatization
corpus <- tm_map(corpus, removeWords, UserWords)  # Remove user-define words
corpus <- tm_map(corpus, stripWhitespace)                # Remove extra whitespace

content(corpus)

# Create a Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)  # Note: not = DocumentTermMatrix
# tdm <- TermDocumentMatrix(corpus, control=list(stemming=TRUE))  # use stemming?
matrix <- as.matrix(tdm)
words <- sort(rowSums(matrix), decreasing = TRUE)
word_freqs <- data.frame(word = names(words), freq = words)

# Now you can create the word cloud:
set.seed(1234)  # For reproducibility
wordcloud(words = word_freqs$word,
          freq = word_freqs$freq,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.2,
          colors = brewer.pal(8, "Dark2"))


### Latent Dirichlet Allocation (LDA) for topic modeling

# Document-Term Matrix (DTM): LDA requires a Document-Term Matrix as input,
# which represents the frequency of each term (word) in each document.
dtm <- DocumentTermMatrix(corpus) # Note: not = TermDocumentMatrix


### How to determine the optimal number of topics needed to describe the data?

# see p. 11 of:
#Grün, B., & Hornik, K. (2011). topicmodels: An R package for fitting topic
#models. Journal of statistical software, 40, 1-30.

dim(dtm) # shows the number of documents and the number of terms

# The mean term frequency-inverse document frequency (tf-idf) over documents
# containing this term is used to select the vocabulary. This measure allows to
# omit terms which have low frequency as well as those occurring in many
# documents. We only include terms which have a tf-idf value of at least 0.1
# which is a bit less than the median and ensures that the very frequent terms
# are omitted.

summary(col_sums(dtm)) # I think this is a summary of term frequency

# Calculate term frequency-inverse document frequency (TF-IDF)
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm) / col_sums(dtm > 0))

summary(term_tfidf)

# I will tweak the cutoff based on the median for this corpus of ~1.0 
# As such, we will only include terms which have a tf-idf value of at least 1.0
# which and ensures that the very frequent terms are omitted.

# filters the document-term matrix (dtm) to keep only those columns (terms)
# whose TF-IDF score is greater than or equal to specified value. This threshold
# helps to exclude very frequent terms, focusing on more relevant or less common
# terms.
dtm <- dtm[, term_tfidf >= 1.0]

# After filtering columns, this line removes any rows (documents) that have all
# zero values. This ensures that the resulting DTM consists only of documents
# that contain at least one of the remaining terms.
dtm <- dtm[row_sums(dtm) > 0,]

summary(col_sums(dtm))

# After this pre-processing we have the following document-term matrix with a
# reduced vocabulary which we can use to ﬁt topic models.
dim(dtm)

# Keep track of the original document indices
filtered_doc_ids <- seq_len(nDocs(dtm))
original_doc_indices <- as.integer(rownames(as.matrix(dtm)))

# In the following we ﬁt an LDA model with 30 topics using (1) VEM with α
# estimated, (2) VEM with α ﬁxed and (3) Gibbs sampling with a burn-in of 1000
# iterations and recording every 100th iterations for 1000 iterations. The
# initial α is set to the default value. By default only the best model with
# respect to the log-likelihood log(p(w|z)) observed during Gibbs sampling is
# returned. In addition a CTM is ﬁtted using VEM estimation.
k <- 30
SEED <- 2010
TM <- list(
  VEM = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))

# To compare the ﬁtted models we ﬁrst investigate the α values of the models
# ﬁtted with VEM and α estimated and with VEM and α ﬁxed.
sapply(TM[1:2], slot, "alpha")

# We see that if α is estimated it is set to a value much larger than the
# default. This indicates that in this case the Dirichlet distribution has less
# mass at the corners and hence, documents consist of several topics.

# The entropy measure can also be used to indicate how the topic distributions
# differ for the four ﬁtting methods. We determine the mean entropy for each
# ﬁtted model over the documents. The term distribution for each topic as well
# as the predictive distribution of topics for a document can be obtained with
# posterior(). A list with components "terms" for the term distribution over
# topics and "topics" for the topic distributions over documents is returned.
sapply(TM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))

# Higher values indicate that the topic distributions are more evenly spread
# over the topics. Looks like the Gibbs/CTM provide marginally higher measures
# of entropy, which may indicate a greater diversity or uncertainty in topic
# distribution, whereas the/VEM/VEM_fixed models provide lower entropy -- this
# may suggest that the topics are more focused, and defined by fewer words.

# The estimated topics for a document and estimated terms for a topic can be
# obtained using the convenience functions topics() and terms(). The most likely
# topic for each document is obtained by
Topic <- topics(TM[["VEM"]], 1)

# The ﬁve most frequent terms for each topic are obtained by
Terms <- terms(TM[["VEM"]], 5)
Terms[, 1:5]

################################################################################
# Now, fit LDA models with different numbers of topics
# Set the range of topics to test
# topic_numbers <- c(2, 3, 5, 10, 15)
topic_numbers <- as.numeric(2:20) # models with a reasonable/interpretable range of topics

# Initialize a list to store models and their log-likelihood values
models <- list()
log_likelihoods <- c()

# # Fit the LDA models
# for (k in topic_numbers) {
#   model <- LDA(dtm, k = k, method = "VEM", iter=1e4)
#   models[[as.character(k)]] <- model
#   log_likelihoods <- c(log_likelihoods, model@loglikelihood)
# }

# Fit the CTM models
for (k in topic_numbers) {
  model <- CTM(dtm, k = k, 
               control = list(seed = SEED, var = list(tol = 10^-4), 
                              em = list(tol = 10^-3)))
  models[[as.character(k)]] <- model
  log_likelihoods <- c(log_likelihoods, model@loglikelihood)
}

# After fitting the models, you can retrieve the last value from the
# log_likelihoods vector corresponding to each model based on the number of
# topics. You can create a new vector or data frame to store these final values.

# Initialize a vector to hold final log-likelihood values
final_log_likelihoods <- c()

# Loop through the number of topics and extract the final log-likelihood for each model
for (k in topic_numbers) {
  final_log_likelihood <- tail(models[[as.character(k)]]@loglikelihood, n = 1)
  final_log_likelihoods <- c(final_log_likelihoods, final_log_likelihood)
}

# Create a data frame for better visualization
loglikelihood_df <- data.frame(
  Topics = topic_numbers,
  Final_Log_Likelihood = final_log_likelihoods
)

# View the results
print(loglikelihood_df)

# Find the model with the highest log-likelihood
best_model_index <- which.max(final_log_likelihoods)
best_model_topics <- loglikelihood_df$Topics[best_model_index]
best_model_score <- loglikelihood_df$Final_Log_Likelihood[best_model_index]

cat("The best model has", best_model_topics, "topics with a final log-likelihood of", best_model_score, "\n")

# Create a plot of final log-likelihood vs. number of topics
p1 <- ggplot(loglikelihood_df, aes(x = Topics, y = Final_Log_Likelihood)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparison of Final Log-Likelihoods",
       x = "Number of Topics",
       y = "Final Log-Likelihood")
print(p1)

# To compare the models, calculate coherence scores for each model. Coherence
# measures the degree of semantic similarity between high-probability words in
# the topics.

# Function to calculate a basic coherence score based on term frequencies
calculate_coherence <- function(model, dtm, top_n = 5) {
  terms_matrix <- terms(model, top_n)
  coherence_score <- 0
  
  for (i in 1:nrow(terms_matrix)) {
    words <- terms_matrix[i, ]
    word_indices <- unique(match(words, colnames(dtm)))
    
    # Count the occurrences of each word in the documents
    if (!any(is.na(word_indices)) && length(word_indices) > 0) {
      term_counts <- colSums(as.matrix(dtm[, word_indices, drop = FALSE]))
      coherence_score <- coherence_score + sum(term_counts^2)
    }
  }
  
  return(coherence_score)
}

# Calculate basic coherence for each model
coherence_scores <- sapply(models, calculate_coherence, dtm = dtm)

# Visualize results
# Create a data frame for plotting
coherence_df <- data.frame(
  Topics = topic_numbers,        # Assuming topic_numbers is defined
  Coherence = coherence_scores       # Assuming coherence_scores is defined
)

p2 <- ggplot(coherence_df, aes(x = Topics, y = Coherence)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparison of Coherences",
       x = "Number of Topics",
       y = "Coherence")

# ------------------------------------------------------------------------------
# Stack the plots
grid.arrange(p1, p2, nrow = 2)
# ------------------------------------------------------------------------------
# Examine the log-likelihood and coherence scores to determine the optimal
# number of topics. The best model typically has the highest coherence while
# also maintaining a reasonable log-likelihood.

# Understanding the Metrics

# Coherence
# Definition: Coherence measures how semantically related the top words of a
# topic are. Higher coherence values generally indicate more interpretable and
# meaningful topics. 
# Interpretation: A higher coherence score indicates that the terms within the
# topic frequently appear together across documents, implying a well-defined
# topic.

# Log-Likelihood 
# Definition: Log-likelihood assesses how well the model fits the data. It
# reflects the probability of the observed data given the model parameters.
# Interpretation: Higher log-likelihood values suggest a better model fit.
# However, simply maximizing log-likelihood doesn’t ensure interpretability, as
# it can lead to overfitting.

# Create word clouds for each of the topics
# Install gridExtra if not already installed
# install.packages("gridExtra")

#-------------------------------------------------------------------------------
# Input parameters
#-------------------------------------------------------------------------------
# Choose a model
topic_model <- models$'3'  # manually select model based on number of topics

plot_columns <- 3 # the number of columns in the grid of word clouds

# Specify number of top words to display globally
top_n <- 30  # Adjust this value as needed
#-------------------------------------------------------------------------------
# Extract the topic-term probabilities and transpose
posterior_probs <- posterior(topic_model)
topic_term_probs <- t(posterior_probs$terms)  # Topic-term probabilities

# Number of topics
num_topics <- ncol(topic_term_probs)

# Create a list to store ggplot objects for word clouds
wordcloud_plots <- lapply(1:num_topics, function(i) {
  term_probs <- topic_term_probs[, i]
  word_freqs <- data.frame(word = names(term_probs), freq = term_probs)
  
  # Check for valid frequencies
  if (nrow(word_freqs) == 0 || all(word_freqs$freq == 0)) {
    message(paste("No words found for Topic", i))
    return(NULL)  # Skip if no valid words
  }
  
  # Print the number of words and a sample for debugging
  print(paste("Topic", i, "has", nrow(word_freqs), "words."))
  # print(head(word_freqs))  # View a sample of the data
  print(word_freqs[order(-word_freqs$freq), ][1:10, ]) # View most freqent terms by topic
  
  # Define a color palette using RColorBrewer
  num_colors <- min(8, nrow(word_freqs))  # Limit the number of colors to 8
  my_colors <- brewer.pal(n = num_colors, name = "Set2")  # Change to a lighter palette
  
  # Filter to top N words using the globally defined top_n variable
  top_words <- head(word_freqs[order(-word_freqs$freq), ], top_n)  # Apply global top_n
  
  # Create ggplot for word cloud
  ggplot(data = top_words, aes(label = word, size = freq, color = freq)) +
    geom_text_wordcloud() +  # Use filtered data for the word cloud
    scale_color_gradientn(colors = my_colors) +  # Use the color palette
    scale_size_area(max_size = 10) +  # Increase max font size
    labs(title = paste("Topic", i)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "black", color = NA),  # Dark background for the plot, no plot borders
      panel.background = element_rect(fill = "black", color = NA),,  # Dark background for the panel, no panel borders
      plot.title = element_text(hjust = 0.5, color = "white", size = 14, margin = margin(5, 0, 5, 0)),  # Center and color the title
      text = element_text(color = "white"),  # Change text color to white
      plot.margin = unit(c(2, 2, 2, 2), "mm"),  # Minimal margins around each plot
      legend.position = "none"  # Remove legend to save space
    )
})

# Remove NULL entries from wordcloud_plots
wordcloud_plots <- Filter(Negate(is.null), wordcloud_plots)

#-------------------------------------------------------------------------------
# Draw plot
# Arrange the word cloud plots in a grid
do.call(grid.arrange, c(wordcloud_plots, ncol = plot_columns))
#-------------------------------------------------------------------------------
# Save plot
# Set the output file name and dimensions (tweak dimensions based on topic number)
jpeg("./Plots/wordclouds.jpg", 
     width = 1920, height = 1080,  # 16:9 widescreen aspect ratio
     quality = 100,                  # Full quality and largest file size
     res = 120)                      # Higher resolution for clarity

# Use `grid.arrange` to arrange word clouds with reduced spacing
do.call(grid.arrange, c(wordcloud_plots, 
                        ncol = plot_columns,
                        top = "",           # Remove top padding
                        bottom = "",        # Remove bottom padding
                        left = "",          # Remove left padding
                        right = ""))        # Remove right padding

# Close the device to save the file
dev.off()
#-------------------------------------------------------------------------------
# Sort the source documents by topic-scores, print the top-scoring documents 
# from each topic to illustrate the raw-data gist.

# Get the document-topic matrix
doc_topic_matrix <- posterior(topic_model)$topics

# Convert to data frame and keep track of document IDs
doc_topic_scores <- as.data.frame(doc_topic_matrix)
doc_topic_scores$document_id <- original_doc_indices # use original document indices

# Melt the data for processing
melted_scores <- melt(doc_topic_scores, id.vars = "document_id", variable.name="topic", value.name="score")

# Get top documents per topic
top_docs <- melted_scores %>%
  group_by(topic) %>%
  slice_max(score, n = 2) %>%  # top 2 documents per topic
  arrange(topic, desc(score))

# Join with original document text
top_docs_with_text <- top_docs %>%
  mutate(text = text_vector[document_id])  # Adjusted to original documents

# Print results
print(top_docs_with_text, n=nrow(top_docs))

# ------------------------------------------------------------------------------
# Generate topic-scores for each participant for use as outcomes/predictors
# Merge the scores with participant information
colnames(doc_topic_scores) <- c("Topic1", "Topic2", "document_id")
combined_data <- merge(d1, doc_topic_scores, by = "document_id", all.x = TRUE)


# Summarize the topic scores for each participant
participant_topic_scores <- combined_data %>%
  group_by(ID) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE), .names = "mean_{.col}")) 

# View the results
print(participant_topic_scores)

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# B) How are the treatment effects on the Decision Progress or Treatment
# Preference mediated by changes in decision making needs, hopes, expectations?
# (i.e. see the proposed DAGs)
# ## Note: meaningful answers to these questions will likely require sample size
# of n >> 10 to answer!!
# ------------------------------------------------------------------------------
# Including effect of Knowledge:
## R code 12.24
dat <- list(
  D = as.integer(d1$FarAlong),
  ID = as.integer(d1$ID),
  PP = as.integer(d1$PrePost),
  K = ifelse(as.integer(d1$Knowledge) == 1, 0, 1) # need to recode to 0/1 for interaction effect
)

# We will also use complete cases instead of data imputation to save time.
# For this iteration, that means dropping cases with ID numbers 5 and 6.
# Identify which rows to keep (exclude certain IDs)
ids_to_drop <- c(5, 6)  # IDs you want to remove
keep_idx <- !(dat$ID %in% ids_to_drop)

# Subset all elements of the list
dat1 <- lapply(dat, function(x) x[keep_idx])

# Fit the model
m0.3 <- ulam(
  alist(
    D ~ dordlogit( phi , cutpoints ),
    phi <- a[ID] + b[PP] + c*K ,
    c <- aK[ID] + bK[PP] ,       # accessory linear model for interaction of Knowledge with ID and PrePost
    a[ID] ~ dnorm( 0 , 10 ),
    b[PP] ~ dnorm( 0 , 10 ),
    aK[ID] ~ dnorm( 0 , 10 ),
    bK[PP] ~ dnorm( 0 , 10 ),
    cutpoints ~ dnorm( 0 , 1.5 )
  ) , data=dat1 , chains=4 , cores=4 )

precis( m0.3 , 2)
precis( m0.3 , 2, pars="bK")

plot( precis(m0.3, 2) )
plot(precis( m0.3 , 2, pars="bK"))
# ------------------------------------------------------------------------------
# Difference plots
# Extract samples from the posterior
post <- extract.samples(m0.3)

diffs <- list(
  PPdiff_bK = post$bK[,2] - post$bK[,1]
)
labels <- c("Effect of Knowledge")
plot( precis(diffs) , xlab="Expected Decision-Progress Change (Post-Pre)", labels=labels)
# ------------------------------------------------------------------------------
# Make a 2-facet plot of histograms for each value of Knowledge.
# ------------------------------------------------------------------------------
par(mfrow=c(1,2)) # plots in 1 row, 2 columns
# ------------------------------------------------------------------------------
## R code 12.29
kK <- 0     # value for Knowledge
kPP <- 1:2   # values of PrePost to calculate over
pdat <- data.frame(K=kK,PP=kPP)
s <- sim( m0.3 , data=pdat )
simplehist( s , xlab="Decision Progress", xaxt = "n" , main="Knowledge = No" )
axis(side = 1, at = seq(1,4, by = 1), labels=levels(d_sim$FarAlong))
# ------------------------------------------------------------------------------
## R code 12.29
kK <- 1     # value for Knowledge
kPP <- 1:2   # values of PrePost to calculate over
pdat <- data.frame(K=kK,PP=kPP)
s <- sim( m0.3 , data=pdat )
simplehist( s , xlab="Decision Progress", xaxt = "n" , main="Knowledge = Yes" )
axis(side = 1, at = seq(1,4, by = 1), labels=levels(d_sim$FarAlong))
# ------------------------------------------------------------------------------
par(mfrow=c(1,1)) # reset (plots in 1 row, 1 column)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Including effect of MetNeeds (sum of 4 decision-making needs):
dat_list <- list(
  MN = (
    ifelse(d1$Knowledge == "Yes", 1L, 0L) +
      ifelse(d1$Values == "Yes", 1L, 0L) +
      ifelse(d1$Support == "Yes", 1L, 0L) +
      ifelse(d1$Certainty == "Yes", 1L, 0L)
  ),
  D = as.integer(d1$FarAlong),
  ID = as.integer(d1$ID),
  PP = as.integer(d1$PrePost) )

# We will also use complete cases instead of data imputation to save time.
# For this iteration, that means dropping cases with ID numbers 5 and 6.
# Identify which rows to keep (exclude certain IDs)
ids_to_drop <- c(5, 6)  # IDs you want to remove
keep_idx <- !(dat_list$ID %in% ids_to_drop)

# Subset all elements of the list
dat_list1 <- lapply(dat_list, function(x) x[keep_idx])

# Fit the model
m0.4 <- ulam(
  alist(
    D ~ dordlogit( phi , cutpoints ),
    phi <- a[ID] + b[PP] + c*MN ,
    c <- aMN[ID] + bMN[PP] ,       # accessory linear model for interaction of MetNeeds with ID and PrePost
    a[ID] ~ dnorm( 0 , 10 ),
    b[PP] ~ dnorm( 0 , 10 ),
    aMN[ID] ~ dnorm( 0 , 10 ),
    bMN[PP] ~ dnorm( 0 , 10 ),
    cutpoints ~ dnorm( 0 , 1.5 )
  ) , data=dat_list1 , chains=4 , cores=4 )

precis( m0.4 , 2)
precis( m0.4 , 2, pars="bMN")

plot( precis(m0.4, 2) )
plot(precis( m0.4 , 2, pars="bMN"))
# ------------------------------------------------------------------------------
# Difference plots
# Extract samples from the posterior
post <- extract.samples(m0.4)

diffs <- list(
  PPdiff_bMN = post$bMN[,2] - post$bMN[,1]
)
labels <- c("Effect of Met Needs")
plot( precis(diffs) , xlab="Expected Decision-Progress Change (Post-Pre)", labels=labels)
# ------------------------------------------------------------------------------
# This single plot shows:
#   
# Individual trajectories (colored lines) connecting each person's D from PP = 1
# to PP = 2
# Colors by MN: Blue (MN=1), Orange (MN=2), Dark red (MN=3)
# Clear mediation: The spacing and slopes of the trajectories reveal how MN
# mediates within-person change in D

plot(NULL, 
     main = "Within-Person Changes in D Across PP (Mediated by MN)",
     xlab = "Measurement Occasion", ylab = "D",
     xlim = c(0.8, 2.2), ylim = c(0, 5),
     type = "n", xaxt = "n")
grid(nx = NA, ny = NULL)

# Custom x-axis
axis(1, at = c(1, 2), labels = c("Pre", "Post"))

# Define colors for each MN value
colors_mn <- c("1" = "steelblue", "2" = "orange", "3" = "darkred")

# Draw trajectories for each individual
unique_ids <- unique(dat_list1$ID)

for (id in unique_ids) {
  id_indices <- which(dat_list1$ID == id)
  sorted_idx <- id_indices[order(dat_list1$PP[id_indices])]
  
  mn_val <- dat_list1$MN[id_indices[1]]
  col <- colors_mn[as.character(mn_val)]
  
  lines(dat_list1$PP[sorted_idx], dat_list1$D[sorted_idx], 
        col = col, lwd = 2, alpha = 0.6)
  points(dat_list1$PP[sorted_idx], dat_list1$D[sorted_idx], 
         pch = 16, cex = 1.5, col = col)
}

# Legend
legend("topright", 
       legend = c("MN = 1", "MN = 2", "MN = 3"),
       col = c("steelblue", "orange", "darkred"),
       lwd = 2, pch = 16)

# ------------------------------------------------------------------------------
# Appendices #
# ------------------------------------------------------------------------------
# Session info (for reproducibility)
# ------------------------------------------------------------------------------
sessionInfo()
