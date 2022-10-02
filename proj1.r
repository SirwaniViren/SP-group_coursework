# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

#3
#setwd("C:/UNI/4th Year/1st Sem/Statistical Programming/SP-group_coursework")
#setwd("C:/Users/alann/Desktop/Statistical programming")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#4
#function to split given punctuation mark from a given list of words
split_punct <- function(words, punc_mark) {
  #string concatenating white space and punctuation mark  
  space_punc <- paste("", punc_mark)
  #puts a space in between the word and the punctuation mark 
  words <- gsub(punc_mark, space_punc, words, fixed = TRUE)
  
  #list of words collapsed to a string separated by a space
  words = paste(words, collapse = " ")
  #converts collapsed string back to list of words 
  words = strsplit(words, " ")[[1]]
  
  return(words)
}

#5
a <- split_punct(a, ",")
a <- split_punct(a, ".")
a <- split_punct(a, ";")
a <- split_punct(a, "!")
a <- split_punct(a, ":")
a <- split_punct(a, "?")

#6
#a)
a_lower<-tolower(a)
a_unique<-unique(a_lower)

#b)
index <- match(a_lower, a_unique)

#c)
freq<-tabulate(index)

#d) 
m = 500
threshold <- 5
#while loop to check the threshold limit such that m ~ 500
while (length(freq[freq>threshold]) >= m) {
  over_thresh <- threshold
  threshold <- threshold + 5
}
under_thresh <- threshold

#equates threshold to a value(either threshold that makes m >= 500 or m < 500) 
#such that m is closest to 500.
threshold <- if ((length(freq[freq>over_thresh]) - m) <= (m - length(freq[freq>under_thresh]))) over_thresh else under_thresh
cat(threshold)

#e)
j <- 1
#empty vector
b <- c()
#for loop to iterate over freq vector
for (count in freq) {
  #checks if current element of freq vector is >= threshold found
  if (count >= threshold) {
    #if it is, append the word in vector b
    b = append(b, a_unique[j])
  }
  j <- j + 1
}

#7
#a)
text_index <- match(a_lower, b)

#b)

#function to shift left common word vector by certain amount and then
#a specified amount is removed from the tail of the new vector
shift_left_and_cut <- function(vector, shift_amount, cut_amount){
  n <- length(vector)
  #create NA vector of length n
  new_vector <- rep(NA, n)
  # shift the original vector by the amount specified
  new_vector[1:(n-abs(shift_amount))] <- vector[(1+abs(shift_amount)):n]
  # remove entries from the end of the new vector to maintain same size
  new_vector_cut <- new_vector[1:(length(new_vector)-cut_amount)]
  return(new_vector_cut)
}

# columns for matrix, names matrix_coli for i = 1,2,3 
matrix_col1 <- text_index[1:(length(text_index)-2)]
matrix_col2 <- shift_left_and_cut(text_index,1,2)
matrix_col3 <- shift_left_and_cut(text_index,2,2)

# bind the above vectors into desired matrix
matrix <- cbind(matrix_col1, matrix_col2, matrix_col3)

#c)
#the new matrix now only contains rows without NA. They way this was done was
# by counting how many NA's were in each row, keeping only the ones with none
matrix_new <- matrix[rowSums(is.na(matrix))==0, ]

#d)
b_n <- length(b)
# 3D array initialized to all 0s
T <- array(c(0,0), dim=c(b_n, b_n, b_n))
# loop to go through every row of matrix_new and adding 1 to the corresponding
# slot in the array
for (i in 1:nrow(matrix_new)){
  T[matrix_new[i,1],matrix_new[i,2],matrix_new[i,3]] = 
    T[matrix_new[i,1],matrix_new[i,2],matrix_new[i,3]] + 1
}

#e)
# not sure if we have to do anything here


#f)

# matrix A to fill in with probabilities
A <- array(c(0,0), dim=c(b_n, b_n))
for (i in 1:nrow(matrix_new)){
  A[matrix_new[i,1],matrix_new[i,3]] = 
    A[matrix_new[i,1],matrix_new[i,3]] + 1 
}
 # vector S to fill in with probabilities
S <- rep(0, b_n)
for (i in 1:nrow(matrix_new)){
  S[matrix_new[i,1]] = S[matrix_new[i,1]] + 1
}
  
#8
num_words <- 50
sim_text <- rep("", num_words)
# randomly pick a word from b, based on the probabilities in S
sim_text[1] <- sample(b, size = 1, prob = S)

# if statement used for second word as we have to fall back to S if the sample 
# word does not follow the first one
if (sample(b, size = 1, prob = A[match(sim_text[1], b), ]) != 0){
  sim_text[2] <- sample(b, size = 1, prob = A[match(sim_text[1], b), ])
}else sim_text[2] <- sample(b, size = 1, prob = S)

# for loop to iterate to fill in remaining words
for (i in 3:length(sim_text)){
  # check if there are more than 0 occurrences of the sampled word coming after the
  # previous one
  if(sample(b, size=1, prob = T[match(sim_text[i-2], b), match(sim_text[i-1], b), ]) != 0){
    sim_text[i] <- sample(b, size=1, prob = T[match(sim_text[i-2], b), match(sim_text[i-1], b), ])
  # fall back to A if word does not follow the pair b_i, b_k
  }else if (sample(b, size = 1, prob = A[match(sim_text[i-2], b), ]) != 0){
    sim_text[i] <- sample(b, size = 1, prob = A[match(sim_text[i-2], b), ])
  # fall back to S
  } else sim_text[i] <- sample(b, size = 1, prob = S)

}

#9
# sim_text_S = sample(b, size = 50, replace = TRUE, prob = S)

# same thing as question 8 but only relying on vector S
sim_text_S <- rep("", num_words)
for (i in 1:length(sim_text)){
  sim_text_S[i] = sample(b, size = 1, prob = S)
}

#10
#find all the unique words, ie including those with capitals
a_unique_caps<-unique(a)

#finding the difference between two vectors to find the capitalised words
difference<-setdiff(a_unique_caps,a_unique)
difference_lower<-tolower(difference)

#10
#if the word exists in the uppercase we want to replace the lower case version of it
#within the text. So we find each instance where its a lowercase and replace
#it with its uppercase

for (i in 1:length(sim_text))
{
  result<-sample(b, size =1, prob = S)
  #find index of lowercase words
  lowerIndex = match(result,difference_lower)[1]
  
  #if lowerindex is a valid number => there exists a capital number
  if (!is.na(lowerIndex)) 
  {
    #find the uppercase word in 'difference' using the same index
    sim_text_S[i]= difference[lowerIndex]
  } 
  #else if lowerindex is NOT a valid number =/> there DOES NOT exist a capital number
  else 
  {
    #else leave it alone
    sim_text_S[i] = result
  }
}
