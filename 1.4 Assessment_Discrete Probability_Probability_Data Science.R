library(gtools)
library(tidyverse)

#Question 1: Olympic running
  #In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).

  #Question 1a: How many different ways can the 3 medals be distributed across 8 runners?
    Names <- c("Usain_Bolt", "Yohan_Blake", "Warren_Weir", "Unknown1", "Unknown2", "Unknown3", "Unknown4", "Unknown5")
    Winners <- permutations(8, 3, v = Names)
    cat("1a Answer:", nrow(Winners), "\n")
    #answer: 336
  
  #Question 1b: How many different ways can the three medals be distributed among the 3 runners from Jamaica?
    Jamacia_Names <- c("Usain_Bolt", "Yohan_Blake", "Warren_Weir")
    Jamacia_Winners <- permutations(3,3, v = Jamacia_Names)
    cat("1b Answer:", nrow(Jamacia_Winners), "\n")
    #answer: 6 
  
  #Question 1c: What is the probability that all 3 medals are won by Jamaica?
    cat("1c Answer:", nrow(Jamacia_Winners)/nrow(Winners), "\n")
    #answer:0.0179 
  
  #Question 1d: Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race: 
    runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
  #For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
  #Calculate the probability that all the runners are from Jamaica.
  #Hint (1 of 1): Consider using the all function to determine whether the medalists are from Jamaica, which is similar to any. All checks whether all elements of a vector meet a certain condition.
    set.seed(1)
    Winners <- permutations(6, 3, v = runners)
    Jamaica_Winners <- function(Winners){
      Winners <- sample(runners, 3)
      all(Winners %in% "Jamaica")
    }
    B = 10000
    results <- replicate(B, Jamaica_Winners())
    cat("1d Answer:", mean(results), "\n")
    #answer: 0.0174
    
    
#Question 2: Restaurant management
  #Use the information below to answer the following five questions.
  #A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
  #A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.
    
  #Question 2a: How many meal combinations are possible with the current menu?
    entrees <- as.character(c(1:6))
    sides <- combinations(6, 2)
    sides <- sides[,1]
    drinks <- as.character(c(1:2))
    lunch_special <- expand.grid(entree=entrees, sides=sides, drink=drinks)
    lunch_special <- paste(lunch_special$entree, lunch_special$sides, lunch_special$drink)
    cat("2a Answer:", length(lunch_special), "\n")
    #Answer: 180
    
  #Question 2b: The manager has one additional drink he could add to the special. How many combinations are possible if he expands his original special to 3 drink options?
    entrees <- as.character(c(1:6))
    sides <- combinations(6, 2)
    sides <- sides[,1]
    drinks <- as.character(c(1:3))
    lunch_special <- expand.grid(entree=entrees, sides=sides, drink=drinks)
    lunch_special <- paste(lunch_special$entree, lunch_special$sides, lunch_special$drink)
    cat("2b Answer:", length(lunch_special), "\n")
    #Answer: 270
    
  #Question 2c: The manager decides to add the third drink but needs to expand the number of options. The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting customers choose more sides.
    #How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
      entrees <- as.character(c(1:6))
      sides <- combinations(6, 3)
      sides <- sides[,1]
      drinks <- as.character(c(1:3))
      lunch_special <- expand.grid(entree=entrees, sides=sides, drink=drinks)
      lunch_special <- paste(lunch_special$entree, lunch_special$sides, lunch_special$drink)
      cat("2c Answer:", length(lunch_special), "\n")
      #Answer: 360
    
  #Question 2d: The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
    #Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
    #Use sapply() to apply the function to entree option counts ranging from 1 to 12.
    #What is the minimum number of entree options required in order to generate more than 365 combinations?
      increase_entrees <- function(n){
        entrees <- c(1:n)
        sides <- combinations(6, 2)
        sides <- sides[,1]
        drinks <- c(1:3)
        lunch_special <- expand.grid(entree=entrees, sides=sides, drink=drinks)
        lunch_special <- paste(lunch_special$entree, lunch_special$sides, lunch_special$drink)
        lunch_special <- data.frame(lunch_special)
        nrow(lunch_special)
      }
      n <- seq(1,12)
      combos <- sapply(n, increase_entrees)
      n_2d <- min(n[combos > 365])
      qplot(n, combos, main = "Question 2d", xlab = "Entrees", ylab = "Combinations") +
        geom_vline(xintercept = n_2d, linetype = "dashed", color = "blue")  +
        scale_x_continuous(breaks = seq(1, 12, by = 1))
      cat("2d Answer:", n_2d, "\n")
      #Answer: 9
      
  #Question 2e:The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations. 
    #Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
    #Use sapply() to apply the function to side counts ranging from 2 to 12.
    #What is the minimum number of side options required in order to generate more than 365 combinations?
      increase_entrees <- function(n){
        entrees <- c(1:6)
        sides <- combinations(n, 2)
        sides <- sides[,1]
        drinks <- c(1:3)
        lunch_special <- expand.grid(entree=entrees, sides=sides, drink=drinks)
        lunch_special <- paste(lunch_special$entree, lunch_special$sides, lunch_special$drink)
        lunch_special <- data.frame(lunch_special)
        nrow(lunch_special)
      }
      n <- seq(2,12)
      combos <- sapply(n, increase_entrees)
      n_2e <- min(n[combos > 365])
      qplot(n, combos, main = "Question 2e", xlab = "Entrees", ylab = "Combinations") +
        geom_vline(xintercept = n_2e, linetype = "dashed", color = "blue")  +
        scale_x_continuous(breaks = seq(1, 12, by = 1))
      cat("1e Answer:", n_2e, "\n")
      #Answer: 7
      
      
#Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
#Question 3: Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
  #The dataset is available in base R and can be called with the variable name esoph: head(esoph) 
  #You will be using this dataset to answer the following four multi-part questions (Questions 3-6).
  #You may wish to use the tidyverse package: library(tidyverse)
  #The following three parts have you explore some basic characteristics of the dataset.
  #Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.
    library(tidyverse)
    esoph <- data.frame(esoph)
    head(esoph)
    str(esoph)
    
  #Question 3a: How many groups are in the study?
    cat("3a Answer:", nrow(esoph), "\n") 
    #Answer: 88
    
  #Question 3b: How many cases are there? (Save this value as all_cases for later problems.)
    all_cases <- sum(esoph$ncases)
    cat("3b Answer:", all_cases, "\n") 
    #Answer: 200
    
  #Question 3c: How many controls are there? (Save this value as all_controls for later problems.)
    all_controls <- sum(esoph$ncontrols)
    cat("3c Answer:", all_controls, "\n") 
    #Answer:775


#Question 4: The following four parts ask you to explore some probabilities within this dataset related to alcohol and tobacco consumption.
  #Question 4a: What is the probability that a subject in the highest alcohol consumption group is a cancer case? (Report your answer to 3 significant figures.)
    H_alcgp <- filter(esoph, alcgp == "120+")
    C_in_H_alcgp <- sum(H_alcgp$ncase)
    Pr_a <- C_in_H_alcgp/(C_in_H_alcgp + sum(H_alcgp$ncontrols))
    cat("4a Answer:", Pr_a, "\n") 
    #Answer: 0.672
    
  #Question 4b: What is the probability that a subject in the lowest alcohol consumption group is a cancer case? (Report your answer to 3 significant figures.)
    L_alcgp <- filter(esoph, alcgp == "0-39g/day")
    all_L_alc_cases <- sum(L_alcgp$ncases)
    all_L_alc_controls <- sum(L_alcgp$ncontrols)
    cat("4b Answer:", sum(L_alcgp$ncases)/(all_L_alc_cases+all_L_alc_controls), "\n") 
    #Answer: 0.0699
    
  #Question 4c: Given that a person is a case, what is the probability that they smoke 10g or more a day?
    H_tobgp <- filter(esoph, tobgp != "0-9g/day")
    Pr_c <- (sum(H_tobgp$ncase))/(sum(esoph$ncase))
    cat("4c Answer:", Pr_c, "\n") 
    #Answer: 0.61
    
  #Question 4d: Given that a person is a control, what is the probability that they smoke 10g or more a day? (Report your answer to 3 significant figures.)
    smoke <- filter(esoph, tobgp != "0-9g/day")
    Pr_d <- (sum(smoke$ncontrols))/(sum(esoph$ncontrols))
    cat("4d Answer:", Pr_d, "\n") 
    #Answer: 0.423
    
#Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2
#Question 5: The following four parts look at probabilities related to alcohol and tobacco consumption among the cases.
 
  #Question 5a: For cases, what is the probability of being in the highest alcohol group?
    Pr_5a <- (sum(H_alcgp$ncases))/(sum(esoph$ncases))
    cat("5a Answer:", Pr_5a, "\n") 
    #Answer: 0.225
  
  #Question 5b: For cases, what is the probability of being in the highest tobacco group?
    H_tobgp <- filter(esoph, tobgp == "30+")
    Pr_5b <- (sum(H_tobgp$ncases))/(sum(esoph$ncases))
    cat("5b Answer:", Pr_5b, "\n") 
    #Answer: 0.155
    
  #Question 5c: For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
    H_alcgp <- filter(esoph, alcgp == "120+")
    H_tobgp_and_alcgp <- filter(H_alcgp, tobgp == "30+")
    Pr_5c <- (sum(H_tobgp_and_alcgp$ncases))/(sum(esoph$ncases))
    cat("5c Answer:", Pr_5c, "\n") 
    #Answer: 0.05
    
  #Question 5d: For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
   Pr_CH_alcgp <- sum(H_alcgp$ncase)/sum(esoph$ncase)
   Pr_CH_tobgp <- sum(H_tobgp$ncases)/sum(esoph$ncases)
   Pr_5d <- Pr_CH_alcgp + Pr_CH_tobgp - Pr_5c
   cat("5d Answer:", Pr_5d, "\n") 
   #Answer: 0.33

#Question 6: The following six parts look at probabilities related to alcohol and tobacco consumption among the controls and also compare the cases and the controls. 
  #Question 6a: For controls, what is the probability of being in the highest alcohol group? (Report your answer to 3 significant figures.)
    Pr_6a <- sum(H_alcgp$ncontrols)/all_controls
    cat("6a Answer:", Pr_6a, "\n") 
    #Answer: 0.0284
    
  #Question 6b: How many times more likely are cases than controls to be in the highest alcohol group? (Report your answer to 3 significant figures.)
    #Reminder of Previous Code:
      #Pr_5a = probability of highest alcohol group in cases
      #Pr_6a = probability of highest alcohol group in controls
    b <- Pr_5a/Pr_6a
    cat("6b Answer:", b, "\n") 
    #Answer: 7.926
    
  #Question 6c: For controls, what is the probability of being in the highest tobacco group? (Report your answer to 3 significant figures.)
    H_tobgp <- filter(esoph, tobgp == "30+")
    Pr_6c <- sum(H_tobgp$ncontrols)/all_controls
    cat("6c Answer:", Pr_6c, "\n") 
    #Answer: 0.0658
    
  #Question 6d: For controls, what is the probability of being in the highest alcohol group and the highest tobacco group? (Report your answer to 3 significant figures.)
    H_alcgp <- filter(esoph, alcgp == "120+")
    H_tobgp_and_alcgp <- filter(H_alcgp, tobgp == "30+")
    Pr_6d <- (sum(H_tobgp_and_alcgp$ncontrols))/(sum(esoph$ncontrols))
    cat("6d Answer:", Pr_6d, "\n") 
    #Answer: 0.00387
    
    H_alcgp_and_H_tobgp <- filter(H_tobgp, alcgp == "120+")
    Pr_6d <- sum(H_tobgp_and_alcgp$ncontrols)/sum(esoph$ncontrols)
    cat("6d Answer(alternate code):", Pr_6d, "\n") 
    #Answer:0.00387 
    
  #Question 6e: For controls, what is the probability of being in the highest alcohol group or the highest tobacco group? (Report your answer to 3 significant figures.)
    #Reminder of Previous Code:
        #Pr_6c = probability of highest tobacco group in controls
        Pr_6c <- sum(H_tobgp$ncontrols)/all_controls
        #Pr_6a = probability of highest alcohol group in controls
        Pr_6a <- sum(H_alcgp$ncontrols)/all_controls
        #Pr_6d = probability of Pr_6c and Pr_6a
        H_alcgp_and_H_tobgp <- filter(H_tobgp, alcgp == "120+")
        Pr_6d <- sum(H_tobgp_and_alcgp$ncontrols)/sum(esoph$ncontrols)
    Pr_6e <- Pr_6c + Pr_6a - Pr_6d
    cat("6e Answer(alternate code):", Pr_6e, "\n") 
    #Answer: 0.0903
    
  #Question 6f: How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group? (Report your answer to 3 significant figures.)
    #Reminder of Previous Code:
      #Pr_6e = probability of highest alcohol group or the highest tobacco group in control
      #Pr_5d = probability of highest alcohol group or the highest tobacco group in cases
    f <- Pr_5d/Pr_6e 
    cat("6f Answer:", f, "\n") 
    #Answer: 3.65
          
          
            