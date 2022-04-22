

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
barplot(collatzloop_bonus, xlab = "Each Integer from 1-100", ylab = "Frequency of Each Integer")

summary(collatzloop_bonus)
minimum_bonus <- which(collatzloop_bonus == min(collatzloop_bonus))
minimum_bonus



##https://stackoverflow.com/questions/9390749/return-index-of-the-smallest-value-in-a-vector


library(ggplot2)
collatzloop_bonus_df <- as.data.frame(collatzloop_bonus)
View(collatzloop_bonus_df)

integers <- (1:100)
frequency_of_integers <- collatzloop_bonus

ggplot(data = collatzloop_bonus_df, aes(x= integers, y = frequency_of_integers )) + 
  geom_col(fill= "lightblue", colour="black") 

library(tidyverse)

df_odd_integers <- data.frame(integers = 1:100, y =frequency_of_integers) 
df_odd_integers


row_odd <- seq_len(nrow(df_odd_integers)) %% 2              
row_odd  

odd_nums_df <- df_odd_integers[row_odd == 1, ]


arrange(odd_nums_df, desc(y)) %>%
  slice(2)




## https://r-graphics.org/recipe-bar-graph-basic-bar
## Used this link to help guide in making the bar plot
##    -Learned about geom_col for colors and outlining
##    -Attempted to add x and y variables to have labels for the axis'
##      and have numeric labels to show frequency range and integers


