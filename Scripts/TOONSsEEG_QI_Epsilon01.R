# TOONSsEEG_QI.R
# Quality initiative to evaluate patient experience of sEEG mapping
# The Ottawa Operative Neuropsychology System (TOONS)
# Bryce P Mulligan, PhD, CPsych
# 19 January 2026
# ------------------------------------------------------------------------------
## ReadMe
# This R script is meant to document our rationale and plans for data
# collection, analysis, and visualisation. Page numbers, "R code" headers, and
# some text excerpts borrow heavily from -- and with sincere appreciation for --
# Richard McElreath's Statistical Rethinking (2nd Edition) and the accompanying
# code file. These are included to illustrate and provide context to our plans,
# expectations, and interpretations. In no way do we intend to portray excerpted
# content as our own original intellectual property.

# This script is intended as a record of our thinking at the time of creation
# and upload. It will further serve as a useful benchmark against which we can
# gauge the evolution of our understanding and approach to these clinical
# issues.

# Finally, all data included in public repositories are simulated (i.e. no data
# from actual persons is included).
# ------------------------------------------------------------------------------
# remove all elements for a clean start (careful, now!)

rm(list=ls(all=TRUE))  # clears the R environment
dev.off()            # clears all plots
cat("\014")            # clears R terminal
# ------------------------------------------------------------------------------
# Upgrade R (run periodically)

# library (installr) # for updating R
# updateR()
# ------------------------------------------------------------------------------
# load packages from library
library(rethinking) # McElreath (2020) Statistical Rethinking
library(dagitty)    # graphical analysis of structural causal models
library(ggplot2)    # elegant data visualisation
library(ggalluvial) # for alluvial/Sankey diagrams
library(scales)     # scale functions for visualisation (e.g. breaks_pretty() )
library(readr)      # read rectangular text data (like 'csv')
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
    CSM -> K
    CSM -> V
    CSM -> S
    CSM -> C
    CSM -> U
      K -> DP
      V -> DP
      S -> DP
      C -> DP
      U -> DP
      U [unobserved]
    }")
coordinates( dag_0.1 ) <- list(x=c(CSM=0,   K=1, V=1, S=1, C=1, U=1, DP=2  ) ,
                                   y=c(CSM=1.5, K=0, V=1, S=2, C=3, U=1.5, DP=1.5  ) )
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
    CSM -> H
    CSM -> E
    CSM -> U
      H -> PS
      E -> PS
      U -> PS
      U [unobserved]
    }")
coordinates( dag_0.2 ) <- list(x=c(CSM=0,   H=1, E=1, U=1, PS=2  ) ,
                               y=c(CSM=0.5, H=0, E=1, U=0.5, PS=0.5  ) )
drawdag( dag_0.2 )
# blank(bty="n") # this does some magical reset of graphical device after datitty

# How might hopes and expectations relate to decision-making needs?
# That's a DAG for another day.

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## Phase I: Simulate anticipated data
N <- 10  # number of patients for whom to simulate data
source("./Scripts/Source/sEEG_QI_DataSim.R")

# ------------------------------------------------------------------------------
# Summarise (contingency tables and Sankey plots)
# ------------------------------------------------------------------------------
# Hopes and Expectations
precis(d_sim$Hope)
precis(d_sim$Expect)

# Decision progress
ggplot(d_sim,
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

t <- table(d_sim$FarAlong, d_sim$PrePost)
addmargins(t)

# Treatment preference
ggplot(d_sim,
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

t <- table(d_sim$Prefer, d_sim$PrePost)
addmargins(t)

# Decision-making needs
# Overall needs

MetNeeds = (
  ifelse(d_sim$Knowledge == "Yes", 1L, 0L) +
    ifelse(d_sim$Values == "Yes", 1L, 0L) +
    ifelse(d_sim$Support == "Yes", 1L, 0L) +
    ifelse(d_sim$Certainty == "Yes", 1L, 0L)
)
PP <- as.integer(d_sim$PrePost)
table(MetNeeds, PP)

# Individual needs
t <- table(d_sim$Knowledge, d_sim$PrePost)
addmargins(t)
t <- table(d_sim$Values, d_sim$PrePost)
addmargins(t)
t <- table(d_sim$Support, d_sim$PrePost)
addmargins(t)
t <- table(d_sim$Certainty, d_sim$PrePost)
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

# Create data list with standardised variables
dat_list <- list(
  H = standardize(d_sim$Hope),
  E = standardize(d_sim$Expect),
  ID = as.integer(d_sim$ID),
  PP = as.integer(d_sim$PrePost)
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
    bH[PP] ~ dnorm( 0 , 1 ) ,
    bE[PP] ~ dnorm( 0 , 1 ) ,  
    sigmaH ~ dexp( 1 ) ,      
    sigmaE ~ dexp( 1 )  
  ) , data = dat_list, chains=4, cores=4, iter=1e3 )

# Before we look at the posterior, let's see if we need to tweak the priors
## R code 5.4
set.seed(10)
prior <- extract.prior( m0.0 )
mu <- link( m0.0 , post=prior , data=list( PP=c(1,2) ) )
muH <- mu[[1]]
muE <- mu[[2]]
par(mfrow=c(2,1)) # reset (plots in 1 row, 1 column)
plot( NULL , xlim=c(1,2) , ylim=c(-3,3) , xlab="PrePost", ylab="PHEQ Score", main="Hope")
for ( i in 1:50 ) lines( c(1,2) , muH[i,] , lwd=2, col=col.alpha("black",0.4) )
plot( NULL , xlim=c(1,2) , ylim=c(-3,3) , xlab="PrePost", ylab="PHEQ Score",main="Expect")
for ( i in 1:50 ) lines( c(1,2) , muE[i,] , lwd=2, col=col.alpha("red",0.4) )
par(mfrow=c(1,1)) # reset (plots in 1 row, 1 column)

# The priors expect a decent range of intercepts (i.e. overall height of the
# lines), however some of the slopes seem a bit extreme (resulting in absoluate
# values > 3). It is worthwhile to consider a model with more restrictive
# priors for the PrePost effec (i.e. bH and bE).

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

## R code 5.4
set.seed(10)
prior <- extract.prior( m0.0 )
mu <- link( m0.0 , post=prior , data=list( PP=c(1,2) ) )
muH <- mu[[1]]
muE <- mu[[2]]
par(mfrow=c(2,1)) # plots in 2 rows, 1 column)
plot( NULL , xlim=c(1,2) , ylim=c(-3,3) , xlab="PrePost", ylab="PHEQ Score", main="Hope")
for ( i in 1:50 ) lines( c(1,2) , muH[i,] , lwd=2, col=col.alpha("black",0.4) )
plot( NULL , xlim=c(1,2) , ylim=c(-3,3) , xlab="PrePost", ylab="PHEQ Score",main="Expect")
for ( i in 1:50 ) lines( c(1,2) , muE[i,] , lwd=2, col=col.alpha("red",0.4) )
par(mfrow=c(1,1)) # reset (plots in 1 row, 1 column)

# That looks more reasonable (all absolute values < 3)

# Now let's examine the posterior for the effects of interest
plot(precis(m0.0, 2, pars=c("bH", "bE")))
# These are of course ridiculous, based on very crude simulated data effects,
# but they convey the idea that hopes are reduced and expectations are increased
# following sEEG.

## R code 5.5
# compute percentile interval of mean
PP_seq <- c(1,2)
mu <- link( m0.0 , data=list(PP=PP_seq) )
# Hopes
muH <- mu[[1]]
muH.mean <- apply( muH , 2, mean )
muH.PI <- apply( muH , 2 , PI )
# Expectations
muE <- mu[[2]]
muE.mean <- apply( muE , 2, mean )
muE.PI <- apply( muE , 2 , PI )

# plot it all
par(mfrow=c(2,1)) # plots in 2 rows, 1 column

# Hopes
plot( H ~ PP , data=dat_list , col=rangi2 , xaxt = "n" )
axis(side = 1, at = seq(1,2, by = 1), labels=levels(d_sim$PrePost))
lines( PP_seq , muH.mean , lwd=2 )
shade( muH.PI , PP_seq )
# Expectations
plot( E ~ PP , data=dat_list , col=rangi2 , xaxt = "n" )
axis(side = 1, at = seq(1,2, by = 1), labels=levels(d_sim$PrePost))
lines( PP_seq , muE.mean , lwd=2 )
shade( muE.PI , PP_seq )

par(mfrow=c(1,1)) # reset (plots in 1 row, 1 column)

# This type of plot doesn't really make sense with a categorical (indexed)
# predictor. What we're really interested in is the Post-Pre difference scores.

# Extract samples from the posterior
post <- extract.samples(m0.0)

diffs <- list(
  PPdiffH = post$bH[,2] - post$bH[,1],
  PPdiffE = post$bE[,2] - post$bE[,1]
)
labels <- c("Hopes", "Expectations")
plot( precis(diffs) , xlab="Expected Standardised PHEQ Change (Post-Pre)", labels=labels)

# Again, these simulated data produce unrealistically astronomical effect sizes,
# but serve our purpose of illustrating our pre-data collection expectations and
# analysis/visualisation plan.

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Decision progress
# p.392
# Unlike a count, the differences in value of an ordered categorical scale are
# not necessarily equal. It might be much harder to move someone’s preference
# for fish from 1 to 2 than it is to move it from 5 to 6. Just treating ordered
# categories as continuous measures is not a good idea.

# Luckily, there is a standard and accessible solution. In principle, an ordered
# categorical variable is just a multinomial prediction problem (page 366). But
# the constraint that the categories be ordered demands a special treatment.
# What we’d like is for any associated predictor variable, as it increases, to
# move predictions progressively through the categories in sequence. So for
# example if preference for ice cream is positively associated with years of
# age, then the model should sequentially move predictions upwards as age
# increases: 3 to 4, 4 to 5, 5 to 6, etc. This presents a challenge: how to
# ensure that the linear model maps onto the outcomes in the right order.

# The conventional solution is to use a cumulative link function. The cumulative
# probability of a value is the probability of that value or any smaller value.
# In the context of ordered categories, the cumulative probability of 3 is the
# sum of the probabilities of 3, 2, and 1. Ordered categories by convention
# begin at 1, so a result less than 1 has no probability at all. By linking a
# linear model to cumulative probability, it is possible to guarantee the
# ordering of the outcomes.

# Step 1 is to explain how to parameterize a distribution of outcomes on the
# scale of log-cumulative-odds. Step 2 is to introduce a predictor (or more than
# one predictor) to these log-cumulative-odds values, allowing you to model
# associations between predictors and the outcome while obeying the ordered
# nature of prediction.

simplehist( as.integer(d_sim$FarAlong) , xlim=c(1,4) , xlab="Decision Progress" )

# discrete proportion of each response value
pr_k <- table( as.integer(d_sim$FarAlong) ) / nrow(d_sim)

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
  ) , data=list( D=as.integer(d_sim$FarAlong) ), chains=4 , cores=4 )

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
  D = as.integer(d_sim$FarAlong),
  ID = as.integer(d_sim$ID),
  PP = as.integer(d_sim$PrePost)
)
m0.2 <- ulam(
  alist(
    D ~ dordlogit( phi , cutpoints ),
    phi <- a[ID] + b[PP] ,
    a[ID] ~ dnorm( 0 , 10 ),
    b[PP] ~ dnorm( 0 , 10 ),
    cutpoints ~ dnorm( 0 , 1.5 )
  ) , data=dat , chains=4 , cores=4 )

plot( precis(m0.2, 2) )
precis( m0.2 , 2)

# Another plotting option is to show the implied histogram of outcomes. All we
# have to do is use sim to simulate posterior outcomes:
## R code 12.29
kPP <- 1:2   # values of PrePost to calculate over
pdat <- data.frame(PP=kPP)
s <- sim( m0.2 , data=pdat )
simplehist( s , xlab="Decision Progress", xaxt = "n" )
axis(side = 1, at = seq(1,4, by = 1), labels=levels(d_sim$FarAlong))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Treatment preference
# p.366
# First, restructure the data
str(d_sim$Prefer)

# Count the number of cases for each category at each measurement occasion
category_counts <- aggregate(d_sim$Prefer, 
                             by = list(group = d_sim$PrePost, 
                                       category = d_sim$Prefer), 
                             FUN = length)
# Rename the columns
colnames(category_counts) <- c("PrePost", "Prefer", "Count")

# Create a complete grid of categories and groups
complete_grid <- expand.grid(group = levels(d_sim$PrePost), 
                             category = levels(d_sim$Prefer))
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
  M = as.integer(wide_counts$Medical), # Prefer = Medical
  S = as.integer(wide_counts$Surgical), # Prefer = Surgical
 PP = as.integer(wide_counts$PrePost)
)

# Poisson model of overall treatment preferences from before to after sEEG
m_0.3 <- ulam(
  alist(
    U ~ dpois(lambdaU),
    M ~ dpois(lambdaM),
    S ~ dpois(lambdaS),
    log(lambdaU) <- aU,               # individual linear models for each category
    log(lambdaM) <- aM,
    log(lambdaS) <- aS,           
    c(aU, aM, aS) ~ dnorm(0,1.5)      # priors
  ), data=dat , chains=3 , cores=3 )

plot(precis(m_0.3))
precis(m_0.3)

# Verify values
k <- coef(m_0.3)
aU <- k['aU']; aM <- k['aM']; aS <- k['aS']
print(c(exp(aU),exp(aM),exp(aS)))
wide_counts

# Does it make sense to estimate effect of PrePost when there are only single cells?
m_0.4 <- ulam(
  alist(
    U ~ dpois(lambdaU),
    M ~ dpois(lambdaM),
    S ~ dpois(lambdaS),
    log(lambdaU) <- aU + bU[PP],         # individual linear models for each category
    log(lambdaM) <- aM + bM[PP],
    log(lambdaS) <- aS + bS[PP],           
    c(aU, aM, aS) ~ dnorm(0,1.5),        # priors
    bU[PP] ~ dnorm(0,1),
    bM[PP] ~ dnorm(0,1),
    bS[PP] ~ dnorm(0,1)
  ), data=dat , chains=3 , cores=3 )

plot(precis(m_0.4, 2))
precis(m_0.4, 2)
# Nice!!!!

# Extract samples from the posterior
post <- extract.samples(m_0.4)

# inverse-logit to transform back to outcome (probability) scale
p_Unsure_b <- exp( post$bU )
p_Medical_b <- exp( post$bM )
p_Surgical_b <- exp( post$bS )
plot( precis( as.data.frame(p_Unsure_b) ) ,xlab="Estimated Count" )
plot( precis( as.data.frame(p_Medical_b) ) ,xlab="Estimated Count" )
plot( precis( as.data.frame(p_Surgical_b) ) ,xlab="Estimated Count" )

# Now compute and plot Pre/Post difference scores
diffs <- list(
  PPdiffU = post$bU[,2] - post$bU[,1],
  PPdiffM = post$bM[,2] - post$bM[,1],
  PPdiffS = post$bS[,2] - post$bS[,1]
)
labels <- c("Unsure", "Medical", "Surgical")
plot( precis(diffs) , xlab="Log-Difference Score (Post-Pre)", labels=labels)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
## Decision-making needs

# Met needs (sum of 4 decision-making needs)
# trimmed data list
dat_list <- list(
  MetNeeds = (
             ifelse(d_sim$Knowledge == "Yes", 1L, 0L) +
             ifelse(d_sim$Values == "Yes", 1L, 0L) +
             ifelse(d_sim$Support == "Yes", 1L, 0L) +
             ifelse(d_sim$Certainty == "Yes", 1L, 0L)
             ),
  ID = as.integer(d_sim$ID),
  PrePost = as.integer(d_sim$PrePost) )

mN0.0 <- ulam(
  alist(
    MetNeeds ~ dbinom( 4 , p ) ,
    logit(p) <- a[ID] + b[PrePost],
    b[PrePost] ~ dnorm( 0 , sigma_b ),
    a[ID] ~ dnorm( a_bar , sigma_a ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_b ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4, iter=1e4, log_lik=TRUE )

plot(precis(mN0.0, 2)) # parameter estimates on the logit (log-odds) scale
precis(mN0.0, 2)

## R code 12.29
kPP <- 1:2   # values of PrePost to calculate over
pdat <- data.frame(PP=kPP)
s <- sim( mN0.0 , data=pdat )
# simplehist( s , xlab="Decision-Making Needs Met" ) # I can't figure out how to show counts for zero :/
dens(s[,1], lwd=3, xlab="Decision-Making Needs Met")
dens(s[,2], lwd=3, col=rangi2, add=TRUE)


# Knowledge
# trimmed data list
dat_list <- list(
  Knowledge = ifelse(d_sim$Knowledge == "Yes", 1L, 0L),
  ID = as.integer(d_sim$ID),
  PrePost = as.integer(d_sim$PrePost) )

mK0.0 <- ulam(
  alist(
    Knowledge ~ dbinom( 1 , p ) ,
    logit(p) <- a[ID] + b[PrePost] ,
    b[PrePost] ~ dnorm( 0 , sigma_b ),
    a[ID] ~ dnorm( a_bar , sigma_a ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_b ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4, iter=1e4, log_lik=TRUE )

plot(precis(mK0.0, 2)) # parameter estimates on the logit (log-odds) scale
precis(mK0.0, 2)

# ------------------------------------------------------------------------------
# B) How are the treatment effects on the Decision Progress or Treatment
# Preference mediated by changes in decision making needs, hopes, expectations?
# (i.e. see the proposed DAGs)
# ## Note: meaningful answers to these questions will likely require sample size
# of n >> 10 to answer!!

# Including effect of Knowledge:
## R code 12.24
dat <- list(
  D = as.integer(d_sim$FarAlong),
  ID = as.integer(d_sim$ID),
  PP = as.integer(d_sim$PrePost),
  K = ifelse(as.integer(d_sim$Knowledge) == 1, 0, 1) # need to recode to 0/1 for interaction effect
)
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
  ) , data=dat , chains=4 , cores=4 )
precis( m0.3 , 2)
precis( m0.3 , 2, pars="bK")

plot( precis(m0.3, 2) )
plot(precis( m0.3 , 2, pars="bK"))

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
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## Phase II: Import and analyse actual data

# Import data
d <- read.csv("./Data/TOONSsEEG_QI_RawData_Alpha00.csv")
# ------------------------------------------------------------------------------
precis(d)

# ------------------------------------------------------------------------------
# Appendices #
# ------------------------------------------------------------------------------
# Session info (for reproducibility)
# ------------------------------------------------------------------------------
sessionInfo()
