# Karman Singh s1936373
# Viren Sirwani Mulani s1949143
# Alannah Hounat s2434943

##setwd("C:/Users/alann/Desktop/Statistical programming")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#split the string wrt words and punctuation
split_punct <-strsplit(a,"[[:space:]]|(?=[.!?:;,])",perl=TRUE)


#removes all punctuation
a2<-gsub("[^[:alnum:][:space:]']", "", a)

#gsub(".","!","?",":",";",",",'',a2)


a_lower<-tolower(a2)
a_unique<-unique(a_lower)

#Using the index vector and the tabulate function, count up how many time each unique word occurs
#in the text
match<-match(c(a_lower),c(unique(a_lower)))
freq<-tabulate(match)






