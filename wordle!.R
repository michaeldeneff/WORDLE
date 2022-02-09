##################################################################
#DIVING INTO W-O-R-D-L-E
##################################################################




#********ALL STATS UP TO DATE AS OF FEBRUARY 9, 2022*********


##################################################################

library('xml2')
library(rvest)
library(tidyverse)
library(wfindr)
library(dplyr)

#Scraping all previous WORDLE answers and date of response.
scrape <- function() {
  
  todays_date= Sys.Date()
  dates= seq(as.Date("2021-06-21"), as.Date(todays_date), by="days")
  dates=rev(dates)
  
  url = "https://www.reviewgeek.com/todays-wordle-answer/"
  
  session<-html_session(url)
  
  the_full_html = read_html(url)
  
  word = the_full_html %>%
    html_nodes( "td~ td+ td" ) %>%     
    html_text()
  
  df= data.frame( word,dates,
                  stringsAsFactors = FALSE)
  df=df%>%
    separate(word, into = c("first", "second","third","fourth","fifth"), 
             sep= c(1:5))
  df$full_word=word
  df=df[, c(7,1,2,3,4,5,6)]

  
  return(df)
}

wordle= scrape()
View(wordle)

write.csv(wordle,"C:/Users/Mdene/OneDrive/Documents/og_wordle.csv", row.names=FALSE)


#count of letter totals
total_letter_sums=table(factor(unlist(strsplit(wordle$full_word, ""), use.names=FALSE), levels=letters))
View(total_letter_sums)

write.csv(total_letter_sums,"C:/Users/Mdene/OneDrive/Documents/wordle1.csv", row.names=FALSE)


#last ten days of results
last_ten= wordle[c(1:10),]
last_ten_sums=table(factor(unlist(strsplit(last_ten$full_word, ""), use.names=FALSE), levels=letters))
last_ten_sums= as.data.frame(last_ten_sums)
View(last_ten_sums)

#hot vowels
vowels=last_ten_sums[c(1,5,9,15,21,25),]
vowels=as.data.frame(vowels)
View(vowels)

vowels <-vowels[order(vowels$Freq, decreasing = TRUE),]
hot_vowels= vowels[c(1:3),]
hot_vowels

write.csv(hot_vowels,"C:/Users/Mdene/OneDrive/Documents/hot_vowels.csv", row.names=FALSE)


#hot consonants
consonants= last_ten_sums[c(2:4,6:8,10:14,16:20,22:24,26),]
consonants= as.data.frame(consonants)
View(consonants)

consonants <-consonants[order(consonants$Freq, decreasing = TRUE),]
hot_consonants= consonants[c(1:7),]
hot_consonants

write.csv(hot_consonants,"C:/Users/Mdene/OneDrive/Documents/hot_consonants.csv", row.names=FALSE)


#cold vowels
vowels=last_ten_sums[c(1,5,9,15,21,25),]
vowels=as.data.frame(vowels)
View(vowels)

vowels <-vowels[order(vowels$Freq),]
cold_vowels= vowels[c(1:3),]
cold_vowels

write.csv(cold_vowels,"C:/Users/Mdene/OneDrive/Documents/cold_vowels.csv", row.names=FALSE)


#cold consonants
consonants= last_ten_sums[c(2:4,6:8,10:14,16:20,22:24,26),]
consonants= as.data.frame(consonants)
View(consonants)

consonants <-consonants[order(consonants$Freq),]
cold_consonants= consonants[c(1:7),]
cold_consonants

write.csv(cold_consonants,"C:/Users/Mdene/OneDrive/Documents/cold_consonants.csv", row.names=FALSE)



#                        Recommend words
#
#takes: The three least used consonants over last 10 days
#                         AND
#       The two least used vowels over the last 10 days
#Uses the answer history to find a word which matches each of the five letters
# above, leaving us with 5 words which were once WORDLE answers.
#
#With those 5 words, I used the 'wfindr' package and its find_word() function
# to create scramble words from our 5 words above. 
#The function returns roughly 30 results, so I randomize the vector and return 
# THREE words as recommendations to the user.

word_1= grep(vowels$Var1[1], wordle$full_word, fixed = TRUE, value = TRUE)
word_1=word_1[1]

word_2= grep(vowels$Var1[2], wordle$full_word, fixed = TRUE, value = TRUE)
word_2=word_2[1]

word_3= grep(consonants$Var1[3], wordle$full_word, fixed = TRUE, value = TRUE)
word_3=word_3[1]

word_4= grep(consonants$Var1[4], wordle$full_word, fixed = TRUE, value = TRUE)
word_4=word_4[1]

word_5=grep(consonants$Var1[5], wordle$full_word, fixed = TRUE, value = TRUE)
word_5=word_5[1]

my_words=c(word_1,word_2,word_3,word_4,word_5)

my_words= find_word(".....", type= "usual", my_words)
set.seed(872436)           
my_words <- sample(my_words)       
my_words= my_words[1:5]
my_words
#[1] "perky" "wrung" "query" "favor" "proxy"

#Find letter counts for each place holder.
#For example, S has the most uses as the first letter of the word,
#and R has the most uses as the second letter of the word. 

first= wordle %>% group_by(first) %>% count(first)
first <-first[order(first$n, decreasing = TRUE),]
first= first[1:5,]
first

second= wordle %>% group_by(second) %>% count(second)
second <-second[order(second$n, decreasing = TRUE),]
second= second[1:5,]
second

third= wordle %>% group_by(third) %>% count(third)
third <-third[order(third$n, decreasing = TRUE),]
third= third[1:5,]
third

fourth= wordle %>% group_by(fourth) %>% count(fourth)
fourth <-fourth[order(fourth$n, decreasing = TRUE),]
fourth= fourth[1:5,]
fourth

fifth= wordle %>% group_by(fifth) %>% count(fifth)
fifth <-fifth[order(fifth$n, decreasing = TRUE),]
fifth= fifth[1:5,]
fifth

write.csv(fifth,"C:/Users/Mdene/OneDrive/Documents/fifth.csv", row.names=FALSE)











#my attempt at finding 'trending' letters. Wanted to find letters that were hot last week
#but cold this week and vice versa, but didn't have patience to do.
ten_before= wordle[c(11:20),]
ten_before=table(factor(unlist(strsplit(last_ten$full_word, ""), use.names=FALSE), levels=letters))
ten_before= as.data.frame(last_ten_sums)
ten_before