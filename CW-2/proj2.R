# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

# Team member contributions to project:

Pone <- function(n, k, strategy, nreps) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored 
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- sample(1:(2*n), 2*n)
    
    number_of_attempts <- 0
    prisoner_number <- match(k, numbered_boxes)
    
    if (strategy == 1) {
      try <- k
      while (number_of_attempts <= n) {
        if (try == prisoner_number) {
          number_of_success <- number_of_success + 1
          break
        }
        else {
          try <- numbered_boxes[try]
          number_of_attempts <- number_of_attempts + 1
        }
      }
    }
    else if (strategy == 2) {
      
    }
    else {
      
    }
    
  }
  
  prob_one <- number_of_success/nreps
  return (prob_one)
}