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

#             <-------------Q4------------->
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

#             <-------------Q5------------->
a <- split_punct(a, ",")
a <- split_punct(a, ".")
a <- split_punct(a, ";")
a <- split_punct(a, "!")
a <- split_punct(a, ":")
a <- split_punct(a, "?")


#             <-------------Q6------------->
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
  #incrementing theshold value
  threshold <- threshold + 5
}
under_thresh <- threshold

#equates threshold to a value(either threshold that makes m >= 500 or m < 500) 
#such that m is closest to 500.
threshold <- if ((length(freq[freq>over_thresh]) - m) <= (m - length(freq[freq>under_thresh]))) over_thresh else under_thresh

#e)
j <- 1
#empty vector
b <- c()
#for loop to iterate over freq vector
for (count in freq) {
  #checks if current element of freq vector is >= threshold found
  if (count >= threshold) {
    #if it is, append the word to vector b
    b = append(b, a_unique[j])
  }
  j <- j + 1
}

#             <-------------Q7------------->
#a)
text_index <- match(a_lower, b)

#b)
# columns for matrix
matrix_col1 <- text_index[1:(length(text_index)-2)]
matrix_col2 <- text_index[2:(length(text_index)-1)]
matrix_col3 <- text_index[3:(length(text_index))]

# bind the above vectors into desired matrix
matrix <- cbind(matrix_col1, matrix_col2, matrix_col3)

#c)
#the new matrix now only contains rows without NA. The way this was done was
#by counting how many NA's were in each row, keeping only the ones with none
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
  
#             <-------------Q8------------->
num_words <- 50
sim_text <- rep("", num_words)
# randomly pick a word from b, based on the probabilities in S
sim_text[1] <- sample(b, size = 1, prob = S)

# if statement used for second word as we have to fall back to S if the sample 
# word does not follow the first one. We also check if there are any non-zero 
# values
if (sum(A[match(sim_text[1], b), ] != 0) != 0 && 
     sample(b, size = 1, prob = A[match(sim_text[1], b), ]) != 0){
  sim_text[2] <- sample(b, size = 1, prob = A[match(sim_text[1], b), ])
}else sim_text[2] <- sample(b, size = 1, prob = S)

# for loop to iterate to fill in remaining words
for (i in 3:length(sim_text)){
  # check if there are more than 0 occurrences of the sampled word coming after the
  # previous one. We also check if there are any non-zero elements in the vector, if not, then
  # we fall back to A.
  if(sum(T[match(sim_text[i-2], b), match(sim_text[i-1], b), ] != 0) != 0 && 
     sample(b, size=1, prob = T[match(sim_text[i-2], b), match(sim_text[i-1], b), ]) != 0){
    sim_text[i] <- sample(b, size=1, prob = T[match(sim_text[i-2], b), match(sim_text[i-1], b), ])
  # fall back to A if word does not follow the pair b_i, b_k
  }else if (sum(A[match(sim_text[i-2], b), ] != 0) != 0 &&
            sample(b, size = 1, prob = A[match(sim_text[i-2], b), ]) != 0){
    sim_text[i] <- sample(b, size = 1, prob = A[match(sim_text[i-2], b), ])
  # fall back to S
  } else sim_text[i] <- sample(b, size = 1, prob = S)
}

#printing the text
cat(sim_text)

#             <-------------Q9------------->
# same thing as question 8 but only relying on vector S
sim_text_S <- rep("", num_words)
for (i in 1:length(sim_text)){
  sim_text_S[i] = sample(b, size = 1, prob = S)
}

# printing the text
cat(sim_text_S)


#             <-------------Q10------------->
#find all the unique words, ie including those with capitals and without
a_unique_caps<-unique(a)

#finding the difference between the two vectors to find just the capitalised words
difference<-setdiff(a_unique_caps,a_unique)

#find index and frequency of each capitalised word 
cap_index<-match(a,difference)
cap_freq<-tabulate(cap_index)#4651

#lowercase the difference between the two vectors
low_difference<-tolower(difference)#4651

#now find how often all words show up in the text in general (in terms of lower case so we dont get dupes)
total<-match(a_lower,low_difference)
index_total<-tabulate(total)#4651


k <- 1
#empty vector to hold capitalised words
b_cap <- c() #3712

#find the fraction of capitalised words/all words, this will be used to find decimal of how often the word shows up
div<-(cap_freq/index_total)

k <- 1
#for every element in div
for (i in 1:length(div))
{
  #if a words shows up more then 50% of the time add it to a new vector b_cap
  if (div[i] > 0.5)
  {
    b_cap<-append(b_cap,difference[k])
  }
  k<-k+1
}


b_low<-tolower(b_cap)#3712


#b also has 498

UpperCase <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

new_b<-Reduce(intersect, list(b,b_low))

new_cap_b<-UpperCase(new_b)


#will need the lower case of b_cap so we can compare them with our sample.
#if a word matches in b_low and result2 this means it will be a word we need to replace with its capitalised version
b_low<-tolower(b_cap)

sim_text_C <- rep("", num_words)
for (i in 1:length(sim_text_C))
{
  result2<-sample(b, size =1,prob = S)
  #now we want to compare our sample with b_cap as this is where the most commonly occuring 
  #Capital words are stored
  lowerindex = match(result2,new_b)[1]
  
  #if lowerindex is a valid number => there exists a capital
  if (!is.na(lowerindex)) 
  {
    #find the uppercase word in 'b_cap' using the same index
    sim_text_C[i]= new_cap_b[lowerindex]
  } 
  #else if lowerindex is NOT a valid number =/> there DOES NOT exist a capital number
  else 
  {
    #else leave it alone
    sim_text_C[i] = result2
  }
}

#printing the text
cat(sim_text_C)
