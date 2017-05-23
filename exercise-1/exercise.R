# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# Load booksinto a dataframe using the austen_books() function
books <- austen_books()

# How many books are in the dataset?
length(unique(books$book))

# Which book has the most lines?
lines <- books %>% group_by(book) %>% tally() %>% arrange(-n)

# Use the unnest_tokens function to generate the full list of words
word.list <- books %>% unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
word.freq <- word_list %>% group_by(word) %>% tally() %>% arrange(-n)

# Remove stop words by performing an anti_join with the stop_words dataframe
cleaned.words <- anti_join(word_list, stop_words)

# Which non stop-words are most common?
nonstop.freq <- cleaned_words %>% group_by(word) %>% tally() %>% arrange(-n)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
nonstop.top <- nonstop.freq %>% filter(n > 500) %>%
                                ggplot(aes(x = word, y = n)) + 
                                  geom_col() + 
                                  coord_flip()

nonstop.top
