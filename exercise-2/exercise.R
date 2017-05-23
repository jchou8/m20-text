# Exercise-2
# What are informatics courses about?

# Set up
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)

# Read in web page
courses <- read_html("https://www.washington.edu/students/crscat/info.html")

# Extract descriptions of each course into a dataframe (may take multiple steps)
course.titles <- courses %>% html_nodes("p b") %>% html_text()
course.descs <- courses %>% html_nodes("p") %>% html_text()
course.data <- data.frame(title = course.titles, desc = course.descs[2:length(course.descs)], stringsAsFactors = FALSE)

# How many courses are in the catalogue?
nrow(course.data)

# Create a tidytext sturcture of all words
word.list <- course.data %>% unnest_tokens(word, desc)

# Which words do we use to describe our courses?
word.freq <- word.list %>% count(word) %>% arrange(-n)

# Create a set of stop words by adding (more) irrelevant words to the stop_words dataframe
new.stop.words <- data.frame(word = c("info", "view", "course", "details", "myplan", "prerequisite", "5"), lexicon = "INFO")
full.stop.words <- rbind(stop_words, new.stop.words)

# Remove stop words by performing an anti_join with the stop_words dataframe
nonstop <- anti_join(word.list, full.stop.words)

# Which non stop-words are most common?
nonstop.freq <- nonstop %>% count(word) %>% arrange(-n)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
nonstop.top <- nonstop.freq %>% filter(n > 10) %>%
                ggplot(aes(x = word, y = n)) + 
                geom_col() + 
                coord_flip()

nonstop.top
