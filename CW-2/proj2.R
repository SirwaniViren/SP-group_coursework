# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943
# Git repo Link:
# Team member contributions to project:

success_check <- function(n, k, numbered_boxes) {
  number_of_attempts <- 0
  number_of_success <- 0
  guess <- numbered_boxes[k]
  while (number_of_attempts <= n) {
    if (guess == k) {
      number_of_success <- 1
      break
    }
    else {
      guess <- numbered_boxes[guess]
      number_of_attempts <- number_of_attempts + 1
    }
  }
  
  return (number_of_success)
}

produce_random_numberes_boxes <- function(n, quantity) {
  return (sample(1:n, quantity))
}

Pone <- function(n, k, strategy, nreps = 10000) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # index of numbered_boxes corresponds to the box number, and the value stored
    # at that index corresponds to the numbered card randomly placed in the box
    numbered_boxes <- produce_random_numberes_boxes(2 * n, 2 * n)
    
    if (strategy == 1) {
      check <- success_check(n, k, numbered_boxes)
      number_of_success <- number_of_success + check
    }
    else if (strategy == 2) {
      random_start_box <- produce_random_numberes_boxes(2 * n, 1)
      check <- success_check(n, random_start_box, numbered_boxes)
      number_of_success <- number_of_success + check
    }
    else if (strategy == 3) {
      #picking n boxes at random
      random_box <- produce_random_numberes_boxes(2 * n, n)
      for (box in random_box) {
        #if this random box contains the prisoners number
        if (box == k) {
          #count successes
          number_of_success <- number_of_success + 1
          break
        }
      }
    }
  }
  prob_one_estimate <- number_of_success / nreps
  return (prob_one_estimate)
}

Pall <- function(n, strategy, nreps = 10000) {
  number_of_success <- 0
  for (reps in 1:nreps) {
    # Outside inner loop since the room is returned to original state once 
    # prisoner leaves
    numbered_boxes <- produce_random_numberes_boxes(2 * n, 2 * n)
    for (prisoner_number in 1:(2 * n)) {
      if (strategy == 1) {
        check <- success_check(n, prisoner_number, numbered_boxes)
        if (check == 0)
          break
      }
      else if (strategy == 2) {
        random_start_box <- produce_random_numberes_boxes(2 * n, 1)
        check <- success_check(n, random_start_box, numbered_boxes)
        if (check == 0)
          break
      }
      else if (strategy == 3) {
        #picking n boxes at random
        random_box <- produce_random_numberes_boxes(2 * n, n)
        check <- 0
        for (box in random_box) {
          #if this random box contains the prisoners number
          if (box == prisoner_number) {
            check = 1
            break
          }
        }
        if (check == 0) {
          break
        }
      }
    }
    number_of_success <- number_of_success + check
  }
  prob_all_estimate <- number_of_success / nreps
  return (prob_all_estimate)
}

# cat("Probability estimate of a single prisoner succeeding in finding their number:")
# cat("\nn=5\nStrategy 1:", Pone(5, 7, 1))
# cat("\nStrategy 2:", Pone(5, 7, 2))
# cat("\nStrategy 3:", Pone(5, 7, 3))
# 
# cat("\nn=50\nStrategy 1:", Pone(50, 7, 1))
# cat("\nStrategy 2:", Pone(50, 7, 2))
# cat("\nStrategy 3:", Pone(50, 7, 3))
# 
# cat("\nProbability estimate of all prisoners succeeding in finding their numbers:")
# cat("\nn=5\nStrategy 1:", Pall(5, 1))
# cat("\nStrategy 2:", Pall(5, 2))
# cat("\nStrategy 3:", Pall(5, 3))
# 
# cat("\nn=50\nStrategy 1:", Pall(50, 1))
# cat("\nStrategy 2:", Pall(50, 2))
# cat("\nStrategy 3:", Pall(50, 3))

dloop <- function(n, nreps) {
  
}