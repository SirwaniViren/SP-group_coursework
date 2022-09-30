# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

setwd("C:/UNI/4th Year/1st Sem/Statistical Programming/SP-group_coursework")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers


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






