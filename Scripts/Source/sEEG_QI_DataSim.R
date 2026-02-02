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
# Simulate free text statements that could reflect decision-making pros and cons
Surg_5P_Pre <- sample(c(
  "I prefer surgery to minimize my reliance on medications.",
  "Reducing medication intake through surgery would enhance my quality of life.",
  "I value the possibility of becoming medication-free after successful surgery.",
  "Surgery might eliminate the side effects I experience from long-term medication.",
  "Fewer medications would simplify my daily routine.",
  "A successful surgical outcome could reduce the financial burden of medications.",
  "I believe surgery can lead to better overall health without the need for drugs.",
  "I want to explore how surgery could decrease my medication dosage over time.",
  "Surgery may offer a permanent solution to my seizure management.",
  "Reducing the number of medications I take would improve my mental clarity.",
  "I appreciate that surgery could help me avoid the trial-and-error process of medication management.",
  "I find the idea of a medication-free lifestyle appealing with surgical intervention.",
  "By pursuing surgery, I hope to reduce the frequency of pharmacy visits.",
  "A surgical option could streamline my epilepsy management plan.",
  "Fewer medications may lead to fewer doctor appointments, which I value.",
  "Understanding how surgery can help me stop taking medications is crucial.",
  "The prospect of surgery excites me because it could reduce my reliance on drugs.",
  "I want to discuss how my treatment plan could change if I choose surgery.",
  "Surgery could enhance my treatment efficacy without added medications.",
  "I appreciate hearing success stories of patients who have reduced medications through surgery.",
  "Having fewer medications would mean fewer side effects to manage.",
  "The freedom from daily medications is a significant factor in considering surgery.",
  "I want to know how surgery might allow for a more straightforward treatment plan.",
  "A surgical intervention could provide long-term control of seizures without pills.",
  "I value the idea of participating in life without the burden of medications.",
  "Surgery offers a chance to reclaim my life from the daily grind of medications.",
  "I seek reassurance that surgery can help reduce my medication needs.",
  "Understanding the potential for medication reduction post-surgery motivates my choice.",
  "The thought of fewer pharmacy bills is enticing with a surgical option.",
  "Reducing my medication load could enhance my participation in social activities.",
  "I hope surgery can bring long-lasting freedom from seizure medications.",
  "The chance to live medication-free after surgery is a significant motivator.",
  "I want clarity on how surgery can effectively replace ongoing drug therapy.",
  "Personal experiences shared by others encourage me to consider surgery for fewer medications.",
  "I trust that a good surgical outcome can reduce my dependence on medications.",
  "Seeing improvements in seizure control post-surgery reinforces my hope.",
  "I value the opportunity for less medicinal burden through surgery.",
  "Choosing surgery might allow me to focus on other aspects of my health.",
  "I want support in navigating the decision between surgery and medication.",
  "The thought of fewer medication-based side effects from surgery intrigues me.",
  "Surgery may provide an avenue to manage my epilepsy without pills.",
  "Looking forward to a life with fewer medications encourages me to pursue surgery.",
  "I appreciate discussions on how surgery can impact my medication regimen.",
  "My goal is to explore all options that facilitate reducing my reliance on medications.",
  "I want my healthcare team to guide me through the surgical decision process for this reason.",
  "I believe surgery could enhance my independence in daily activities.",
  "Participating in social events without fear of seizures is a priority for me.",
  "Surgery may allow me to drive again, increasing my independence.",
  "I value the possibility of returning to work post-surgery without medication concerns.",
  "Surgery could provide the freedom to engage in physical activities I love.",
  "Increasing my functional independence is a major reason I’m considering surgery.",
  "I want the confidence to enjoy life fully without limitations from epilepsy.",
  "Surgery might reduce the fear of having a seizure in public settings.",
  "Being independent from medications can empower me in my daily life.",
  "I seek a life where I can fully participate in family gatherings and outings.",
  "Surgery could allow me to be more active in my community.",
  "I appreciate the idea of being able to travel freely without worrying about medications.",
  "Having more independence would significantly improve my overall well-being.",
  "Surgery can potentially help me maintain a routine without interruptions.",
  "Reclaiming my independence is fundamental to my happiness.",
  "The chance to manage my epilepsy without affecting my work life is important.",
  "I want to discuss how surgery can enhance my ability to socialize.",
  "Independence is key to my self-confidence and emotional health.",
  "Surgeries that increase my functionality appeal greatly to me.",
  "I believe improved independence will positively influence my mental health.",
  "Surgery might provide the freedom to explore new hobbies and interests.",
  "I want my healthcare team to help me assess how surgery can improve my life quality.",
  "Participating in group activities without limitations is essential to me.",
  "Surgery may alleviate restrictions placed on my lifestyle due to seizures.",
  "I want to understand how post-surgery arrangements can support my independence.",
  "Being able to live without constant supervision is very appealing with surgery.",
  "Surgery could greatly enhance my ability to care for myself and my needs.",
  "Increased independence would help me build stronger personal relationships.",
  "I value the opportunity to pursue my career goals with fewer limitations.",
  "I want to know how long it typically takes to regain independence after surgery.",
  "Surgery may allow me to transition back into work more effectively.",
  "Overcoming barriers to independence through surgery excites me.",
  "Enhanced functional independence motivates my choice to pursue surgery.",
  "I believe that surgical intervention can significantly improve my daily life.",
  "I want to share my experiences with others who have regained their independence after surgery.",
  "Knowing that I can engage with friends without restrictions is very important to me.",
  "Surgery could reframe my identity beyond epilepsy and medications.",
  "I seek assurance that my independence can be restored after surgery.",
  "Living a life with improved functionality is a driving force behind my decision.",
  "Surgery might restore my confidence to participate in spontaneous activities.",
  "I want my healthcare providers to discuss realistic expectations post-surgery."
),
  size = N, replace=TRUE)

Surg_5P_Post <- sample(c(
  "I prefer surgery to minimize my reliance on medications.",
  "Surgery may help me live a more medication-free life.",
  "Reducing daily medications through surgery is appealing to me.",
  "I value the possibility of fewer side effects from not taking medications.",
  "A successful surgery could eliminate the need for ongoing drug regimens.",
  "I want to explore how surgery can reduce my medication dosage.",
  "The prospect of living without the constant burden of medications is motivating.",
  "I seek reassurance that surgery can lead to significant medication reduction.",
  "Surgery could simplify my life by decreasing my need for pills.",
  "Living medication-free after surgery offers hope for a better quality of life.",
  "Fewer medications would lead to lower healthcare costs over time.",
  "I believe surgery can restore my health without relying on drugs.",
  "Reducing my medication load can enhance my overall well-being.",
  "Surgery might provide a longer-lasting solution compared to medication.",
  "The appeal of fewer pharmacy visits encourages me to consider surgery.",
  "I appreciate the chance for surgery to alleviate the struggle of finding the right medications.",
  "Having fewer drug interactions to worry about is important to me.",
  "I want a treatment plan that prioritizes decreasing my medications.",
  "Surgery may help free me from the constant management of meds.",
  "The idea of fewer side effects from meds after surgery excites me.",
  "I value hearing success stories about patients who reduced medications through surgery.",
  "Surgery could help me regain control over my daily routine.",
  "I want to discuss how a surgical option can help manage my condition without drugs.",
  "Fewer medications would allow me to focus more on living my life.",
  "A permanent solution through surgery intrigues me.",
  "I want to understand how surgical intervention could facilitate lower medication use.",
  "Surgery might help me transition to a healthier lifestyle without heavy medications.",
  "The possibility of a life with minimal meds strengthens my interest in surgery.",
  "I value the potential of surgery to eliminate the need for rescue medications.",
  "I want to know about the long-term benefits of reducing my reliance on medications through surgery.",
  "The thought of living more freely without constant doses of medication inspires me.",
  "I appreciate discussions about how to effectively reduce my meds with surgical options.",
  "I find the idea of being less dependent on medications empowering.",
  "I believe surgery could significantly enhance my overall quality of life.",
  "Living with fewer restrictions would improve my happiness.",
  "Surgery might allow me to enjoy activities I’ve avoided due to seizures.",
  "I want to explore how surgery can help me feel more like myself.",
  "Improving my well-being is a major motivation for considering surgery.",
  "I appreciate the potential for surgery to alleviate daily struggles.",
  "Surgery can offer me a chance to experience life more fully.",
  "Enhanced quality of life is an essential goal in my health journey.",
  "I believe that reducing seizures through surgery can uplift my mood.",
  "Surgery may provide an opportunity to reconnect with my passions.",
  "I want discussions about how surgical options can lead to a better lifestyle.",
  "The possibility of renewed energy and vitality through surgery is appealing.",
  "Surgery may help me break free from the limitations imposed by my condition.",
  "I appreciate that improved quality of life can positively affect my relationships.",
  "I want to understand how surgery could help me participate more actively in my community.",
  "Knowing that surgery might free me from constant worry is motivating.",
  "I feel optimistic about the possibility of enjoying everyday moments after surgery.",
  "Surgery could reduce feelings of isolation and increase my joy.",
  "The chance for a more fulfilling life drives my interest in surgical options.",
  "I want to reclaim experiences I’ve missed due to my condition through surgery.",
  "Having fewer health-related anxieties would greatly improve my happiness.",
  "Surgery might allow me to enjoy family events without fear of seizures.",
  "I value pursuing interests and hobbies that surgery could make possible.",
  "I seek reassurance about how surgery can enhance my quality of life.",
  "Surgery may provide an opportunity for greater emotional stability.",
  "I want to embrace a future where I feel more in control of my life.",
  "Surgery could foster a stronger sense of independence and self-esteem.",
  "I believe that improving my quality of life is worth pursuing surgical options.",
  "The benefits of surgery may lead to a more balanced, joyful existence.",
  "I appreciate hearing stories of others who improved their quality of life through surgery.",
  "Surgery may help me find contentment beyond the confines of epilepsy.",
  "I want my healthcare team to support my desire for a better quality of life.",
  "The prospect of living without limitations brings me hope.",
  "Surgery might empower me to thrive instead of just survive.",
  "Improved quality of life is a significant factor in my treatment goals.",
  "I want to explore how to optimize my life experience post-surgery."
),
  size = N, replace=TRUE)

Surg_5P <- c(Surg_5P_Pre, Surg_5P_Post)
  

# Bind to a dataframe
d_sim <- data.frame(ID, PrePost, 
                    FarAlong, Prefer, 
                    Knowledge, Values, Support, Certainty,
                    Hope, Expect,
                    Surg_5P)
str(d_sim)