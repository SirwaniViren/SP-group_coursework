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

#7
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

#printing the text
cat(sim_text)

#9
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

#First find how often each capitalised word shows up 
cap_index<-match(a,a_unique_caps)
cap_freq<-tabulate(cap_index)

#next part is essentially the same as Q6d and e with some modifications in variable names
#using the same m and threshold as earlier 

#check threshold limit
while (length(cap_freq[cap_freq>threshold]) >= m) 
{
  over_thresh <- threshold
  threshold <- threshold + 5
}
under_thresh <- threshold

#ensures m is close to 500
threshold <- if ((length(cap_freq[cap_freq>over_thresh]) - m) <= (m - length(cap_freq[cap_freq>under_thresh]))) over_thresh else under_thresh

k <- 1
#empty vector
b_cap <- c()
#if a words has frequency over the threshold within the capital words
#add it to a new vector b_cap
for (count in cap_freq) 
{
  #checks if current element of cap_freq>= the current threshold 
  if (count >= threshold) 
    {
    #add to b_cap
    b_cap = append(b_cap, difference[k])
    }
  k <- k + 1
}

#if the word exists in the uppercase we want to replace the lower case version of it
#within the text. So we find each instance where its a lowercase and replace
#it with its uppercase

b_low<-na.omit(tolower(b_cap))#perhaps this is causing issue - noted below
#check for myself edit out later
print(b_low)

for (i in 1:length(sim_text))
{
  result2<-sample(b, size =1,prob = S)
  print(result2)
  #now we want to compare our sample with b_cap as this is where the most commonly occuring 
  #Capital words are stored
  lowerindex = match(result2,b_low)[1]
  
  #if lowerindex is a valid number => there exists a capital number
  if (!is.na(lowerindex)) 
  {
    #find the uppercase word in 'difference' using the same index
    sim_text_S[i]= b_cap[lowerindex]
  } 
  #else if lowerindex is NOT a valid number =/> there DOES NOT exist a capital number
  else 
  {
    #else leave it alone
    sim_text_S[i] = result2
  }
}


#NOTE
#for q10 rather than capitalising every word that's capitalised in main text,
# shouldn't we capitalise words that are capitalised the most in main text

#Think I did Q10 better now please check over it and let me know of any constructive criticism :]
#although i noticed in one output "Jesus" wasnt capitalised even though it would be a commonly
#capitalised word perhaps due to something with line 208 i'll look at this again later this evening and tidy it up