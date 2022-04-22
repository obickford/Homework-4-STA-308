####
## April 21, 2022
## Olivia Bickford 
##
## This is homework 4
##  Working with the Collatz Sequence
##  Using more practice with conditional code 
##  to loop together functions and create 
##  sequences and user specified code 
##  
##  I also created plots and bar graphs and learned 
##  more about how those functions worked and how I 
##  can create different graphs
##
##  In the first question I made a functions outlining 
##  the specifics for collatz sequence such as is a number 
##  is divided by 2 and equals 0 it returns that number, or 
##  if not the number goes through 3*num +1 
##
##  Then using conditional code, I made a function that printed out the 
##  full sequence for an n value used from 1-10000
##

directions <- function(num) { ##basic collatz sequence 
  if(num == 1) {
    return(NULL)
  }
  
  if(num %% 2 == 0) {
    return(num/2)
  } 
  else{
    return(3*num+1)
  }
}

collatzloop_trial <- function(n) { ## Prints out the collatz sequence for a given n value 
  stated_problem <- n
  while (n != 1) {
    n <- directions(n=n)
    stated_problem <- c(stated_problem, n)
  }
  return(stated_problem)
}
collatzloop_trial(7)

collatzloop <- c()
for (i in 1:10000) {
  collatzloop <- c(collatzloop, length(collatzloop_trial(i)))
}
hist(collatzloop)
summary(collatzloop)

#############################################
## BONUS ##

collatzloop_2_bonus <- c()
for (i in 1:10000) {
  collatzloop_2_bonus <- c(collatzloop_2_bonus, (collatzloop_trial(i)))
}
collatzloop

collatzloop_bonus <- tabulate(collatzloop_2_bonus, nbins = 100)
collatzloop_bonus
summary(collatzloop_bonus)

minimum_bonus <- which(collatzloop_bonus == min(collatzloop_bonus)) ## number that appears the
## least amount of times from 1-100
minimum_bonus



##https://stackoverflow.com/questions/9390749/return-index-of-the-smallest-value-in-a-vector
## source used for guidance above

library(ggplot2)
library(tidyverse)

collatzloop_bonus_df <- as.data.frame(collatzloop_bonus)
View(collatzloop_bonus_df)

integers <- (1:100)
frequency_of_integers <- collatzloop_bonus

ggplot(data = collatzloop_bonus_df, aes(x= integers, y = frequency_of_integers )) + 
  geom_col(fill= "lightblue", colour="black") 

df_odd_integers <- data.frame(integers = 1:100, y =frequency_of_integers) 
df_odd_integers

row_odd <- seq_len(nrow(df_odd_integers)) %% 2              
row_odd  ## specifying odd numbers for data frame 

odd_nums_df <- df_odd_integers[row_odd == 1, ]


arrange(odd_nums_df, desc(y)) %>%
  slice(2)



## https://r-graphics.org/recipe-bar-graph-basic-bar
## Used this link to help guide in making the bar plot
##    -Learned about geom_col for colors and outlining
##    -Attempted to add x and y variables to have labels for the axis'
##      and have numeric labels to show frequency range and integers


