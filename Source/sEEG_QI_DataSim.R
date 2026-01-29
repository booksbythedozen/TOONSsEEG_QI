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
  "Medical treatment can be less invasive compared to surgery.",
  "Surgery may lead to significant freedom from seizures for some patients.",
  "Medications often have side effects that can affect my daily life.",
  "Surgical options may provide a permanent solution to seizures.",
  "I appreciate that medications offer more control over my treatment plan.",
  "Surgery involves risks, including complications from anesthesia.",
  "Finding the right medication regimen can take time and adjustments.",
  "Surgery may reduce or eliminate the need for ongoing medications.",
  "Medicines can be adjusted easily if side effects occur.",
  "The benefits of surgery can take time to evaluate post-operation.",
  "I appreciate the flexibility of using medication to manage symptoms.",
  "Surgical options may require a lengthy recovery period.",
  "Medical treatment allows me to keep a level of autonomy in decision-making.",
  "Not all patients are eligible for surgery, based on the type of epilepsy.",
  "Medications can help with seizure control without undergoing surgery.",
  "I want to weigh the long-term effectiveness of surgery against medication.",
  "Surgery may provide a quicker seizure response time after treatment.",
  "Medical therapy may not provide the desired level of seizure control.",
  "Surgery carries the risk of cognitive effects, depending on location.",
  "I appreciate that starting medication is often a straightforward process.",
  "Surgery may enhance my overall quality of life if successful.",
  "I want to understand the potential for both surgical and medication failures.",
  "Cost considerations of long-term medication use are important to me.",
  "Surgical recovery can be difficult both physically and emotionally.",
  "I value the straightforward nature of medical treatment options.",
  "Surgical outcomes can vary significantly from patient to patient.",
  "Medications allow gradual adjustments based on my individual response.",
  "I want to know the long-term implications of both treatment options.",
  "Surgery may involve multiple specialists and longer hospital stays.",
  "Medical treatment can be monitored regularly for effectiveness.",
  "I appreciate clear pathways for adjusting medication dosages.",
  "Surgery may eliminate debilitating side effects from long-term medication use.",
  "The fear of not controlling seizures effectively influences my decision.",
  "Understanding the recovery process after surgery is vital for my planning.",
  "I value the support available with long-term medical management.",
  "Surgery could impose limitations on certain activities post-recovery.",
  "I need clear evaluations of medication effectiveness over time.",
  "I want to understand how surgery may affect my brain function.",
  "Medications can often be tailored to my specific needs and lifestyle.",
  "Surgery may come with a higher upfront cost compared to long-term meds.",
  "I want transparency regarding the success rates of surgical interventions.",
  "Not all medications work effectively for every patient.",
  "Surgical candidacy requires thorough evaluations and discussions.",
  "Managing my condition with medications allows for daily adaptability.",
  "The prospect of freedom from medications is appealing with surgery.",
  "I want to balance risks associated with both treatment types.",
  "Understanding the tapering process for medications is important for me.",
  "I recognize surgery may not guarantee seizure freedom for all patients.",
  "I appreciate ongoing monitoring to ensure my medications remain effective.",
  "Potential emotional impacts from surgery need to be considered.",
  "I want to discuss experiences of others who have undergone surgery.",
  "Medical treatments may lead to improved cognitive function for some.",
  "Surgical recovery can provide renewed hope in managing epilepsy.",
  "I value discussions about my lifestyle in the context of both options.",
  "Surgery can involve a multi-disciplinary approach, which I find beneficial.",
  "Medical therapy may have consequences that need to be managed long-term.",
  "I appreciate quick access to medications compared to surgical procedures.",
  "I want to compare lifestyle changes associated with both treatment paths.",
  "The permanence of surgical results can be reassuring for many patients.",
  "Medications carry the risk of dependency for some individuals.",
  "Understanding the limitations of surgical candidacy is crucial.",
  "I want to hear about the long-term success and challenges of both options.",
  "Surgical options may enhance my confidence in social settings.",
  "I recognize that some patients prefer to avoid surgery altogether."
),
  size = N, replace=TRUE)

Surg_5P_Post <- sample(c(
  "Medication provides a well-established method for managing seizures.",
  "Surgery has the potential to cure epilepsy in select patients.",
  "The variety of medications allows for personalized treatment plans.",
  "Surgery can significantly reduce the frequency of seizures in some cases.",
  "Medications can lead to a trial-and-error process to find the right fit.",
  "Surgical intervention may relieve patients from the burden of daily medication.",
  "I appreciate that ongoing medical treatment can be conveniently managed.",
  "Surgery can provide immediate results in seizure control.",
  "Long-term medication use can result in cumulative side effects.",
  "The surgical option carries risks that must be thoroughly assessed.",
  "I value the availability of immediate help from medications during a seizure.",
  "Surgery may involve a longer recovery period post-operation.",
  "Medications can be monitored and adjusted easily as needed.",
  "The potential for cognitive changes post-surgery is a concern.",
  "Medication management allows flexibility to adjust dosage and frequency.",
  "Surgery might disrupt my daily routine during recovery.",
  "I want to discuss the potential for a medication-free life post-surgery.",
  "Access to medications can be quicker than scheduling surgical procedures.",
  "I appreciate having consistent follow-up appointments for medication management.",
  "Surgical options may require extensive pre-operative testing.",
  "Understanding potential long-term impacts of medications helps me decide.",
  "I want to know how surgical options can change my lifestyle positively.",
  "Medications may need lifelong management planning.",
  "Surgery may not be a viable option for all types of epilepsy.",
  "Having multiple medications increases the chance of interactions.",
  "I value the ability to consult with my healthcare team throughout my treatment.",
  "Surgery may involve significant emotional and psychological adjustments.",
  "Medication availability can vary based on insurance coverage.",
  "A successful surgical outcome could significantly improve my social interactions.",
  "I want full transparency regarding the risks and benefits of both options.",
  "Some drugs have limited effectiveness for certain seizure types.",
  "I appreciate hearing about other patients' experiences with surgery.",
  "Surgery could potentially lead to fewer overall healthcare visits.",
  "The psychological impact of surgery can vary widely among individuals.",
  "I need to understand how both treatments will affect my work life.",
  "Medications can sometimes exacerbate mood disorders.",
  "I value that surgery may ultimately offer a more straightforward solution.",
  "Surgical approaches often require a multi-disciplinary healthcare team.",
  "I want to discuss lifestyle changes that might enhance treatment effectiveness.",
  "Access to post-operative support is essential for my recovery.",
  "I want honest discussions about the probability of seizure reduction.",
  "Having a clear understanding of my condition empowers me in decision-making.",
  "Surgical candidates must meet specific eligibility criteria.",
  "Medication regimens can be complex, impacting adherence.",
  "The perception of surgery as a last resort can influence my choice.",
  "I value the chance to explore all possible treatment options.",
  "Understanding potential impacts on my family dynamics is essential.",
  "Surgery may resolve issues caused by medications like cognitive fatigue.",
  "Medications require regular assessments to maintain effectiveness.",
  "The long-term benefits of surgery must outweigh the initial risks.",
  "I want to ensure my treatment choice aligns with my personal values.",
  "Some patients experience fewer side effects from surgical intervention.",
  "Medications can stabilize my condition during the decision process.",
  "The decision-making process should involve my entire healthcare team.",
  "Surgical success rates vary and should be thoroughly understood.",
  "Access to reliable resources about treatment options is essential.",
  "I want to discuss how my lifestyle will be impacted by both treatments.",
  "Medications can aid in managing mild seizures effectively.",
  "The emotional burden of long-term medication is a significant concern.",
  "I seek to understand how surgical options will be presented to me.",
  "I value the opportunity to evaluate and reassess my treatment plan.",
  "Surgery may promote greater independence post-recovery.",
  "I want clear guidance during the post-treatment adjustment phase."
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