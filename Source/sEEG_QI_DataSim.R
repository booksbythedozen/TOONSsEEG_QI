# sEEG_QI_DataSim.R

ID <- factor(rep(1:N, times=2)) # patient ID number
PrePost <- ordered(rep(c("Pre","Post"), each=N), levels=c(c("Pre","Post")))  # factor identifying whether data is from before or after the sEEG study
# Simulate increased ordinal scale responses following sEEG study
FarAlong_Pre  <- sample(c("Not thinking about it",
                          "Thinking about it",
                          "Close to choosing",
                          "Made a choice"), size = N, replace=TRUE, prob=c(0.8, 0.5, 0.2, 0.1))
FarAlong_Post <- sample(c("Not thinking about it",
                          "Thinking about it",
                          "Close to choosing",
                          "Made a choice"), size = N, replace=TRUE, prob=c(0.1, 0.2, 0.5, 0.8))
FarAlong <- ordered(c(FarAlong_Pre, FarAlong_Post), levels=c("Not thinking about it",
                                                             "Thinking about it",
                                                             "Close to choosing",
                                                             "Made a choice"))
# Simulate a change in treatment preference following sEEG study
Prefer_Pre  <- sample(c("Unsure", "Medical", "Surgical"), size = N, replace=TRUE, prob=c(0.6, 0.1, 0.3))
Prefer_Post <- sample(c("Unsure", "Medical", "Surgical"), size = N, replace=TRUE, prob=c(0.2, 0.5, 0.5))
Prefer <- factor(c(Prefer_Pre, Prefer_Post), levels=c("Unsure", "Medical", "Surgical"))
# Simulate increase in met decision needs following sEEG study
Knowledge_Pre  <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.8, 0.2))
Knowledge_Post <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.2, 0.8))
Knowledge <- factor(c(Knowledge_Pre, Knowledge_Post))
Values_Pre  <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.8, 0.2))
Values_Post <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.2, 0.8))
Values <- factor(c(Values_Pre, Values_Post))
Support_Pre  <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.8, 0.2))
Support_Post <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.2, 0.8))
Support <- factor(c(Support_Pre, Support_Post))
Certainty_Pre  <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.8, 0.2))
Certainty_Post <- sample(c("No", "Yes"), size = N, replace=TRUE, prob=c(0.2, 0.8))
Certainty <- factor(c(Certainty_Pre, Certainty_Post))
# Simulate change in hopes/expectations (i.e. PHEQ scores) following sEEG study
Hope_Pre <- rnorm(N, mean=1, sd=1)
Hope_Post <- rnorm(N, mean=-1, sd=1)
Hope <- c(Hope_Pre, Hope_Post)
Expect_Pre <- rnorm(N, mean=-1, sd=1)
Expect_Post <- rnorm(N, mean=1, sd=1)
Expect <- c(Expect_Pre, Expect_Post)

# Bind to a dataframe
d_sim <- data.frame(ID, PrePost, 
                    FarAlong, Prefer, 
                    Knowledge, Values, Support, Certainty,
                    Hope, Expect)
str(d_sim)