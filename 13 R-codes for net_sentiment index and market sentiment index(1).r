# ------------------------------------------------------------
# Step 0: Library
# ------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(tm)
library(plotly)
# ------------------------------------------------------------
# Step 1: Load and Clean
# ------------------------------------------------------------
setwd("C:/R/budget")
files <- list.files(pattern = "*.txt")
speeches <- map_df(files, function(f) {
  text_data <- readLines(f, encoding = "UTF-8", warn = FALSE)
  tibble(docs = tools::file_path_sans_ext(f),
         text = paste(text_data, collapse = " "))
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
# Step 2: NRC Sentiment Analysis (Fixed)
# ------------------------------------------------------------
# 1. Load the NRC lexicon
nrc_lexicon <- get_sentiments("nrc")

# 2. Join and Count Emotions per Document
nrc_counts <- tidy_speeches %>%
  inner_join(nrc_lexicon, by = "word") %>%
  group_by(docs, sentiment) %>%
  summarise(word_count = n(), .groups = 'drop')

# --- SHOW RESULTS ---
print("--- NRC EMOTION COUNTS ---")
print(nrc_counts,n = Inf)

# -----------------------------------------------------------
# Step 3: Index contruction
# ------------------------------------------------------------
# 3. Pivot wider to calculate indices
nrc_index <- nrc_counts %>%
  pivot_wider(names_from = sentiment, values_from = word_count, values_fill = 0) %>%
  mutate(
    # Index 1: Net Sentiment Index (NSI)
    Net_Sentiment_Index = ((positive - negative) / (positive + negative)) * 100,

    # Index 2: Budget Confidence Index (BCI)
    # Adding +1 to the denominator to prevent division by zero errors
#    Market_Sentiment_Index = (trust + anticipation + joy) / (fear + anger + sadness + disgust + 1)
#  )
    Market_Sentiment_Index = ((anticipation - fear) / (anticipation + fear + 1))*100
  )
# --- SHOW RESULTS ---
print("--- NRC INDICES SUMMARY ---")
summary_table <- nrc_index %>%
  select(docs, Net_Sentiment_Index, Market_Sentiment_Index)
print(summary_table, n = Inf)

# ------------------------------------------------------------
# Step 4: Visualize NRC Emotions
# ------------------------------------------------------------
p1 <- ggplot(nrc_counts, aes(x = sentiment, y = word_count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~docs) + # This creates one small graph for each speech
  theme_minimal() +
  labs(title = "Emotional Profile",
       y = "Frequency", x = "Emotion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

print(p1)

# ------------------------------------------------------------
# Step 5: Visualize Index2 market sentiment
# ------------------------------------------------------------
p2 <- ggplot(nrc_index, aes(x = docs, y = Net_Sentiment_Index, fill = Market_Sentiment_Index > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), guide = "none") +
  theme_minimal() +
  labs(title = "Net Sentiment Index (NSI)",
       subtitle = "",
       y = "NSI Score", x = "Document") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)
# ------------------------------------------------------------
# Step 6: Visualize Index2 market sentiment
# ------------------------------------------------------------
p3 <- ggplot(nrc_index, aes(x = docs, y = Market_Sentiment_Index, fill = docs)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  # Zoom in the Y-axis to see the variance between 30 and 50
  coord_cartesian(ylim = c(30, 50)) +
  labs(title = "Market Sentiment (MS) Index",
       subtitle = "",
       y = "MS Score", x = "Document") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

print(p3)
