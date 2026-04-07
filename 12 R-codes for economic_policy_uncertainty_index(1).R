# ------------------------------------------------------------
# Step O: Library
# ------------------------------------------------------------
# --- Load libraries (Your original list) ---
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

# ------------------------------------------------------------
# Step 2: Install and use library
# ------------------------------------------------------------
# 1. Install (only if not already installed)
devtools::install_github("Lingbing/epuR")

# 2. Load the library
library(epuR)

# 3. Fetch data
epu_data <- get_EPU()

# ------------------------------------------------------------
# Step 3: EPU Intensity Analysis
# ------------------------------------------------------------
# 1. Define roots (CRITICAL: These must be defined before use)
economy_roots <- "econom|finance|gdp|fiscal|growth"
policy_roots <- "tax|spend|regul|budget|policy|ministry|central|bank|debt|allocation|subsidy"
uncertainty_roots <- "uncertain|risk|unclear|instabil|volatil|crisis"

# 2. Tag the words in your speech
analysis_epu <- tidy_speeches %>%
  mutate(category = case_when(
    str_detect(word, economy_roots) ~ "Economy",
    str_detect(word, policy_roots) ~ "Policy",
    str_detect(word, uncertainty_roots) ~ "Uncertainty",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(category))

# 3. Create Wide Summary Table for Intensity Calculation
speech_summary <- analysis_epu %>%
  group_by(docs, category) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = category, values_from = count, values_fill = 0)

# 4. Ensure all columns exist
for(cat in c("Uncertainty", "Economy", "Policy")) {
  if(!(cat %in% names(speech_summary))) speech_summary[[cat]] <- 0
}

# 5. Calculate Intensity
speech_summary <- speech_summary %>%
  mutate(epu_intensity = (Uncertainty / (Economy + Policy)) * 100) %>%
  mutate(epu_intensity = ifelse(is.nan(epu_intensity) | is.infinite(epu_intensity), 0, epu_intensity))

# --- SHOW RESULTS ---
print("--- DETAILED EPU SUMMARY ---")
print(speech_summary)

# ------------------------------------------------------------
# Step 4: Visualize Intensity
# ------------------------------------------------------------
# Now we plot 'epu_intensity' instead of 'word_count'
p <- ggplot(speech_summary, aes(x = docs, y = epu_intensity, fill = docs)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Economic Policy Uncertainty (EPU)",
       subtitle = "",
       y = "Intensity Score (%)", x = "Document") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)
