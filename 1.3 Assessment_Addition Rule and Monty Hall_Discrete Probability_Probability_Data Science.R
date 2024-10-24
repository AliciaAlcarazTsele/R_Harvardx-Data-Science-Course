##Exercise 1. The Cavs and the Warriors
  #Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. The teams are equally good, so they each have a 50-50 chance of winning each game.
  #If the Cavs lose the first game, what is the probability that they win the series?
  
  #Alicia added this instruction: Assign 'p' as the number of games played
    p_1 <- 1
  
  #Assign a variable 'n' as the number of remaining games.
    n <- 7 - p_1
  
  #Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
    outcomes <- c(loss = 0, win = 1)
  
  #Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
    l <- rep(list(outcomes), n)
  
  #Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
    possibilities <- expand.grid(l)
  
  #Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
    results <- rowSums(possibilities)>3
  
  #Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
    cat("1 Answer:", sum(results)/64, "\n")
  
    
#Exercise 2. The Cavs and the Warriors - Monte Carlo
  #Use the replicate function to replicate the sample code for B <- 10000 simulations.
  #Use the sample function to simulate a series of 6 games with random, independent outcomes of either a loss for the Cavs (0) or a win for the Cavs (1) in that order. Use the default probabilities to sample.
  #Use the sum function to determine whether a simulated series contained at least 4 wins for the Cavs.
  #Use the mean function to find the proportion of simulations in which the Cavs win at least 4 of the remaining games. Print your answer to the console.
  
  #Confirm the results of the previous question with a Monte Carlo simulation to estimate the probability of the Cavs winning the series after losing the first game.
  #The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
    B <- 10000
  
  #Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
    set.seed(1)
  
  #Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
    l <- function(){
      s <- sample(c(0, 1), 6, replace = TRUE)
      sum(s) >= 4
      }
    results <- replicate(B, l())
  
  #Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
    cat("2 Answer:", mean(results), "\n")
  
    
#Exercise 3. A and B play a series - part 1
  #Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a chance of winning each game.
  #Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
  #Then plot the result plot(p, Pr).
 
  #Let's assign the variable 'p' as the vector of probabilities that team A will win.
    p <- seq(0.5, 0.95, 0.025)
  
  #Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
    prob_win <- function(p){
      B <- 10000
      result <- replicate(B, {
        b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
        sum(b_win)>=4
      })
      mean(result)
    }
  
  #Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
    Pr_3 <- sapply(p, prob_win)
  
  #Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
    plot(p, Pr_3, main = "Exercise 3", xlab = "Probability A Wins", ylab = "Probability B Wins")
    cat("3 Answer: See Graph", "\n")
    
    
#Exercise 4. A and B play a series - part 2
  #Use Pr to compute the probability of team B winning based where they are in the series.
  #Then plot the result plot(N, Pr).
  
  #Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
    prob_win <- function(N, p=0.75){
      B <- 10000
      result <- replicate(B, {
        b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
        sum(b_win)>=(N+1)/2
      })
      mean(result)
    }
  
  #Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
    N <- seq(from = 1, to = 25, by = 2)
  
  #Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
    Pr_4 <- sapply(N, prob_win)
  
  #Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
    plot(N, Pr, main = "Exercise 4", xlab = "Series Length", ylab = "Probability B Wins")
    cat("4 Answer: See Graph", "\n")