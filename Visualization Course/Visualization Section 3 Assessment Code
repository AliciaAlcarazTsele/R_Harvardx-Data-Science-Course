library(dslabs)
data(heights)
str(heights)
options(digits = 3)
cat("total obs =", nrow(heights), "\n")

#question 2
  average <- mean(heights$height)
  ind <- heights$height>average & heights$sex == "Female"
  ind
  cat("2 Answer:", sum(ind), "\n")

#question 3
  filter(heights, heights$sex == "Female")
  filter(heights, heights$sex == "Male")
  proportions_females= 238/(238+500+312)
  cat("3 Answer:", proportions_females, "\n")

#question 4a: Determine the minimum height in the heights dataset.
  h <- heights$height
  min_height <- min(h)
  cat("4a Answer:", min_height, "\n")
      #OR see the first entry for sort(h) = 50


#question 4b: Use the match() function to determine the index of the first individual with the minimum height.
  index_min_height <- match("50", h)
  cat("4b Answer:", index_min_height, "\n")
    #OR use order(h) and see the first entry is 1032
    

#question 4c: Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
  s <- heights$sex
  s[index_min_height]
  cat("4c Answer: Male", "\n")

#question 5a: Determine the maximum height.
  cat("4c Answer:",max(h), "\n")

#question 5b: 
  x <- 50:82

#question 5c: How many of the integers in x are NOT heights in the dataset? 
#Use the sum() and %in% functions in addition to the ! operator.
#Use 2.13.5
  y <- c("50":"82") %in% h
  cat("5c Answer:",sum(!y), "\n")

#Question 6a: What is the height in centimeters of the 18th individual (index 18)?
  heights2 <- mutate(heights, ht_cm = h * 2.54)
  str(heights2)
  cat("6a Answer:",heights2$ht_cm[18], "\n")

#Question 6b:
  cat("6a Answer:",mean(heights2$ht_cm), "\n")

#Question 7a: How many females are in the heights2 dataset?
  h2_Female <- filter(heights2, sex == "Female")
  cat("7a Answer:", nrow(h2_Female), "\n")

#Question 7b: What is the mean height of the females in centimeters?
  mean_h2_Female <- heights2 |> filter(sex == "Female") |> summarize(average = mean(ht_cm))
  cat("7a Answer:", as.character(mean_h2_Female), "\n")

#Question 8: Create a graph with palmitic and palmitoleic percentages from the olive database.
  library(dslabs)
  data(olive)
  head(olive)
  
  olive2 <- mutate(olive, percent_palmitic = palmitic/(palmitic+ palmitoleic+ stearic+ oleic+ linoleic+ linolenic+ arachidic+ eicosenoic), percent_palmitoleic = palmitoleic/(palmitic+ palmitoleic+ stearic+ oleic+ linoleic+ linolenic+ arachidic+ eicosenoic))
  head(olive2)
  qplot(x = olive2$percent_palmitic, olive2$percent_palmitoleic, main = "Q8: % of Palmitic VS Palmitoleic Acid in Olives", xlab = "Palmitic %", ylab = "Palmitoleic %")


#Question 9: Create a histogram of the percentage of eicosenoic acid in olives.
  hist(with(olive, eicosenoic), main = "Q9: % of Olives with Eicosenoic Acid", xlab = "Percentage", ylab = "Amount of Olives")

#Question 10: 
  boxplot(olive$palmitic~region, data = olive, main ="Q10: Region & Amount of Palmitic", ylab = NULL, xlab = NULL)
