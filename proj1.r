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

# #removes all punctuation
# a2<-gsub("[^[:alnum:][:space:]']", "", a)

#6
#a)
a_lower<-tolower(a)
a_unique<-unique(a_lower)

#b)
index <- match(a_lower, a_unique)
#match<-match(c(a_lower),c(unique(a_lower)))

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
  new_vector <- rep(NA, n)
  new_vector[1:(n-abs(shift_amount))] <- vector[(1+abs(shift_amount)):n]
  new_vector_cut <- new_vector[1:(length(new_vector)-cut_amount)]
  return(new_vector_cut)
}

matrix_col1 <- text_index[1:(length(text_index)-2)]
matrix_col2 <- shift_left_and_cut(text_index,1,2)
matrix_col3 <- shift_left_and_cut(text_index,2,2)

matrix <- cbind(matrix_col1, matrix_col2, matrix_col3)

#c)
#the new matrix now only contains rows without NA. They way this was done was
# by counting how many NA's were in each row, keeping only the ones with none
matrix_new <- matrix[rowSums(is.na(matrix))==0, ]

#d)
b_n <- length(b)
T <- array(c(0,0), dim=c(b_n, b_n, b_n))
for (i in 1:nrow(matrix_new)){
  T[matrix_new[i,1],matrix_new[i,2],matrix_new[i,3]] = 
    T[matrix_new[i,1],matrix_new[i,2],matrix_new[i,3]] + 1
}

#e)


#f)
A <- array(c(0,0), dim=c(b_n, b_n))
for (i in 1:nrow(matrix_new)){
  A[matrix_new[i,1],matrix_new[i,3]] = 
    A[matrix_new[i,1],matrix_new[i,3]] + 1 
}

S <- rep(0, b_n)
for (i in 1:nrow(matrix_new)){
  S[matrix_new[i]] = S[matrix_new[i]] + 1
}
  
#8