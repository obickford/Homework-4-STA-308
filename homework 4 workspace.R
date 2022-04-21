

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

###################################################

collatzloop_bonus2 <- c()
for (i in 1:100) {
  collatzloop_bonus2 <- c(collatzloop_bonus2, (collatzloop_trial(i)))
}
barplot(collatzloop_bonus2)

integers <- c(integers, collatzloop)


