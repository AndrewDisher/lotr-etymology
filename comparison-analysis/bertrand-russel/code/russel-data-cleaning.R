# Author: Andrew Disher
# Date: 10/17/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`, case_when, distinct, filter, group_by, join_by, left_join, mutate, 
        n, select, summarize, ungroup], 
  ggplot2[...],
  scales[...],
  udpipe[...]
)

# -------------------------------------
# ----- Negation of %in% function -----
# -------------------------------------

`%notin%` <- Negate(`%in%`)

# ---------------------------
# ----- Import the Data -----
# ---------------------------
file_name <- "comparison-analysis/bertrand-russel/data/russel-essay.txt"

# Iterate through each file name and read text
my_file <- readChar(con = file_name,
                    nchars = file.info(file_name)$size) %>% 
  # Convert to character (just in case)
  as.character()

# ---------------------------------------
# ----- Annotate Tokens in Data Set -----
# ---------------------------------------

# Download udpipe english model
udmodel <- udpipe_download_model(language = "english")
udmodel

# Get words and parts of speech from all books in book_df
essay_data <- udpipe(x = my_file,
                       object = udmodel)

# --------------------------------------
# ----- Remove all Unwanted Tokens -----
# --------------------------------------

# Unwanted tokens include those classified as punctuation (PUNCT) and proper nouns (PROPN). 
# Punctuation is not useful, and proper nouns are too domain specific to be of
# use. Also, numerals (NUM), symbols (SYM), particles (PART), determiners (DET),
# and other (X) are all not useful. 

unwanted_tags <- c("PUNCT", "PROPN", "NUM", "SYM", "PART", "DET", "X")

intermediate_df <- essay_data %>% 
  filter(upos %notin% unwanted_tags)

# ----------------------------------------------
# ----- Create a custom POS tag definition -----
# ----------------------------------------------

# The word etymology data contains a small subset of the UPOS tags for its tagging system.
# To make use of the etymologies, we need to convert the UPOS tags to the etymology 
# data's unique tagging system.

intermediate_df <- intermediate_df %>% 
  mutate(custom_pos = case_when(
    upos == "NOUN" ~ "n",
    upos == "AUX" ~ "v",
    upos == "ADV" ~ "adv",
    upos == "ADJ" ~ "adj",
    upos == "ADP" ~ "prep",
    upos == "CCONJ" ~ "conj",
    upos == "SCONJ" ~ "conj",
    upos == "PRON" ~ "pron",
    upos == "VERB" ~ "v",
    upos == "INTJ" ~ "interj"
  ), .after = 12)

# -----------------------------------------------------------
# ----- Create a Data Frame (for further data cleaning) -----
# -----------------------------------------------------------

# New data frame
cleaned_essay_data <- intermediate_df %>% 
  select(token, lemma, custom_pos)

# Force all characters in lemma column to be lower case
cleaned_essay_data <- cleaned_essay_data %>% 
  mutate(lemma = tolower(lemma))

# -------------------------------------------------------------------
# ----- FUNCTION to strip away symbols from words (like dashes) -----
# -------------------------------------------------------------------

strip_symbols <- function(word, bad_symbols) {
  # Split word up by character
  characters_in_word <- strsplit(word, "")[[1]]
  
  # Find position of last character
  last_char_position <- length(characters_in_word)
  
  # Determine if first and/or last characters should be stripped
  if (characters_in_word[1] %in% bad_symbols){
    characters_in_word[1] <- ""
  }
  if (characters_in_word[last_char_position] %in% bad_symbols) {
    characters_in_word[last_char_position] <- ""
  } 
  
  # Reassemble the word and return it
  return_string <- paste(characters_in_word, collapse = "")
  
  return(return_string)
}

# Define a vector of symbols to remove from the ends of words
bad_symbols <- c("'", "\"", "?", "!", "-", ",", ".", "’", "—", "_")

# Apply the function to remove symbols before aggregating
cleaned_essay_data <- cleaned_essay_data %>% 
  mutate(lemma = lapply(lemma, FUN = strip_symbols, bad_symbols = bad_symbols))

# -------------------------------------------------------
# ----- Aggregate Data to Acquire Lemma Frequencies -----  
# -------------------------------------------------------

essay_data_freq <- cleaned_essay_data %>% 
  group_by(lemma, custom_pos) %>% 
  summarize(Frequency = n()) %>% 
  ungroup()

# Redefine book_data_freq such that columns aren't list type
essay_data_freq <- essay_data_freq %>% 
  mutate(lemma = unlist(lemma),
         custom_pos = unlist(custom_pos),
         Frequency = unlist(Frequency))

# ------------------------------------------------
# ----- Import the Lexicon and Language Data -----
# ------------------------------------------------

word_lexicon <- read.csv(file = "data/cleaned_data/word_lexicon.csv")
language_data <- read.csv(file = "data/scraped_data/language-data.csv")

# -------------------------------------------
# ----- Compile Problem Word Data Frame -----
# -------------------------------------------

prob_words_df <- word_lexicon %>% 
  group_by(Word_Only, POS_Only) %>% 
  filter(n() > 1) %>% 
  ungroup()

# NOTE: You can examine why they are problem words by viewing the `Words` column.

# We can acquire just the words and part of speech themselves, without repeats. This
# will be what we use to filter out our main lexicon. 

filter_df <- prob_words_df %>% 
  select(Word_Only, POS_Only) %>%  
  distinct() 

# ----------------------------------------------
# ----- Perform Data Filtering for Lexicon -----
# ----------------------------------------------

new_lexicon <- word_lexicon %>% 
  filter(!(Word_Only %in% filter_df$Word_Only & POS_Only %in% filter_df$POS_Only))

# ---------------------------------------------------
# ----- Join the Frequency Data and New Lexicon -----
# ---------------------------------------------------

joined_data <- left_join(essay_data_freq,
                         new_lexicon,
                         by = join_by(lemma == Word_Only,
                                      custom_pos == POS_Only)
)

joined_data <- joined_data %>% 
  filter(!is.na(Definitions)) %>% 
  select(lemma, custom_pos, Frequency, Definitions)

# ------------------------------------------------------------------
# ----- Extract Languages Mentioned in Etymologies/Definitions -----
# ------------------------------------------------------------------

# First, create a new column for language mention count in language_data
language_data <- language_data %>% 
  mutate(Count = numeric(1),
         Weighted_Count = numeric(1))

# Extract languages present in Definitions column using language data frame
for (row in 1:nrow(joined_data)) {
  # Compare against current definition string
  string_to_check <- joined_data$Definitions[row]
  
  # For each language in the language list
  for (lang in 1:nrow(language_data)) {
    # If definition contains a mention to the current language
    if (grepl(pattern = language_data$Language[lang],
              x = string_to_check)) {
      language_data$Count[lang] <- language_data$Count[lang] + 1
      language_data$Weighted_Count[lang] <- language_data$Weighted_Count[lang] + joined_data$Frequency[row]
    }
  }
}

# Now subset the data frame to contain on positive counts
language_counts <- language_data %>% 
  filter(Count > 0) %>% 
  select(Language, Count, Weighted_Count) 

# -------------------------------------
# ----- Preliminary Visualization -----
# -------------------------------------

# Color list for all relevant languages
color_list <- c("Old English" = "#FFC0CB",
                "German" = "#98FB98",
                "Germanic" = "#E6E6FA",
                "Dutch" = "#89CFF0",
                "Frisian" = "#FFDAB9",
                "Old High German" = "#FFFF99",
                "Old Norse" = "#C8A2C8",
                "Latin" = "#B0E0E6",
                "French" = "#F08080",
                "Old French" = "#C3CDE6",
                "Middle English" = "#FFB6C1")

# Bar graph of top ten language influences (by appearance count)
p1_data <- language_counts[order(language_counts$Count, decreasing = TRUE), ]

ggplot(data = p1_data[1:7,], mapping = aes(x = reorder(Language, Count, decreasing = TRUE), 
                                           y = Count, fill = Language)) +
  geom_bar(stat = "identity", color = "black") +
  geom_label(aes(label = Count), vjust = 2, fill = "white") +
  theme_minimal() +
  labs(x = "Language",
       y = "Count",
       title = "Counts for Language Influences") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = label_number(),
                     breaks = breaks_extended(8)) +
  scale_fill_manual(values = color_list)


# Bar graph of top ten language influences (by weighted count)
p2_data <- language_counts[order(language_counts$Weighted_Count, decreasing = TRUE), ]

ggplot(data = p2_data[1:7,], mapping = aes(x = reorder(Language, Weighted_Count, decreasing = TRUE), 
                                           y = Weighted_Count, fill = Language)) +
  geom_bar(stat = "identity", color = "black") +
  geom_label(aes(label = Weighted_Count), vjust = 2, fill = "white") +
  theme_minimal() +
  labs(x = "Language",
       y = "Weighted Count",
       title = "Weighted Counts for Language Influences") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                     breaks = breaks_extended(8)) +
  scale_fill_manual(values = color_list)

# ---------------------------------------------------
# ----- Write to csv the essay data frequencies -----
# ---------------------------------------------------
write.csv(essay_data_freq, file = "comparison-analysis/bertrand-russel/data/cleaned_data/essay_data_freq.csv", row.names = FALSE)

