# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

setwd("C:/UNI/4th Year/1st Sem/Statistical Programming/SP-group_coursework")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#split the string wrt words and punctuation
#asplit_punct <-strsplit(a,"[[:space:]]|(?=[.!?:;,])",perl=TRUE)

#function to split given punctuation mark from a given list of words
split_punct <- function(words, punc_mark) {
  #finds index of given puctuation mark
  #index <- grep(punc_mark, words, fixed = TRUE)
  #removes the punctuation mark from all words in the list
  space_punc <- paste("", punc_mark)
  words <- gsub(punc_mark, space_punc, words, fixed = TRUE)
  
  #pastes punctuation mark after a space at indices found above
  #words[index] = paste(words[index], punc_mark, sep=" ")
  
  #list of words collapsed to a string
  words = paste(words, collapse = " ")
  #converts collapsed string back to list of words
  words = strsplit(words, " ")[[1]]
  
  return(words)
}

a <- split_punct(a, ",")
a <- split_punct(a, ".")
a <- split_punct(a, ";")
a <- split_punct(a, "!")
a <- split_punct(a, ":")
a <- split_punct(a, "?")

# #removes all punctuation
# a2<-gsub("[^[:alnum:][:space:]']", "", a)
# 
# 
# 
# a_lower<-tolower(a2)
# a_unique<-unique(a_lower)
# 
# #Using the index vector and the tabulate function, count up how many time each unique word occurs
# #in the text
# match<-match(c(a_lower),c(unique(a_lower)))
# freq<-tabulate(match)






