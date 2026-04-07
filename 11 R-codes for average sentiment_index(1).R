# 1. Load libraries and lexicon
library(tidytext)
library(dplyr)
library(tidyverse)
# ------------------------------------------------------------
# Step 1: Load and Clean
# ------------------------------------------------------------
setwd("C:/R/budget")
files <- list.files(pattern = "*.txt")
speeches <- map_df(files, function(f) {
  tibble(docs = tools::file_path_sans_ext(f),
         text = paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = " "))
})

# Load standard stop words
data("stop_words")

# Define custom stop words to remove non-conceptual terms
custom_stop_words <- tibble(
  word = c("million", "billion", "government", "ringgit", "budget"),
  lexicon = "custom"
)

tidy_speeches <- speeches %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace_all(word, "[^a-zA-Z]", "")) %>%
  filter(word != "", nchar(word) > 2) %>%
  anti_join(stop_words, by = "word") %>%      # Remove standard words
  anti_join(custom_stop_words, by = "word")   # Remove your specific list

# -----------------------------------------------------------
# Step 1: Sentiment analysis
# ------------------------------------------------------------
# Join with the AFINN lexicon
afinn_lexicon <- get_sentiments("afinn")
volatility_data <- tidy_speeches %>%
  inner_join(afinn_lexicon, by = "word")

# Calculate Volatility (Mean and SD)
afinn_stats <- volatility_data %>%
  group_by(docs) %>%
  summarise(
    mean_sentiment = mean(value),
    volatility = sd(value)
  )

print(afinn_stats)

# ------------------------------------------------------------
# Step 4: Visualize Intensity
# ------------------------------------------------------------
p <- ggplot(afinn_stats, aes(x = docs, y =  mean_sentiment, fill = docs)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sentiment analysis",
       subtitle = "",
       y = "Mean Score", x = "Document") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)
