

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
collatzloop_trial(n=3)



###########################################
## Combination of the above functions, adding in vectors

CollatzLoop1 <- function(x,n){ ##This i think loops over 1-10000, i get the lengths 
  for(x in 1:n){
    collatz_vector <- c()
    for(i in 1:10000){
      if (x == 1){
        collatz_vector <- c(collatz_vector, x)
        break
      } else {
        if (x %% 2 == 0){
          collatz_vector <- c(collatz_vector, x)
          x <- x/2
        } else {
          collatz_vector <- c(collatz_vector, x)
          x <- 3*x+1
        }
      }
    }
  }
  length(collatz_vector)
}
CollatzLoop1(n=3)



###################################################
## Trial

collatzlength <- function(n) { ## Also gives sequence length 
  collatz_length2 <- c()
  for(i in 1:10000) {
    collatz_length2 <- c(collatz_length2,
                         CollatzLoop1(n=n))
  }
  mean(collatz_length2)
}
collatzlength(n=3)


