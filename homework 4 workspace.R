

directions <- function(num) {
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

stated_hw_problem <- function(n) {
  stated_problem <- n
  while (n != 1) {
    n <- directions(n)
    stated_problem <- c(stated_problem, n)
  }
  return(stated_problem)
}

stated_hw_problem(6)


collatz_length <- function(n) { ##without the looping over 1:10000
  stated_problem <- n
    while (n != 1) {
    n <- directions(n)
    stated_problem <- c(stated_problem, n)
    }
  length(stated_problem)
}
collatz_length(6)

#######################################
col_length <- c()
  for(i in 1:10000) {
    col_length <- c(col_length,
                    collatz_length)
    length(col_length)
  }

hist(col_length)

#######################################







