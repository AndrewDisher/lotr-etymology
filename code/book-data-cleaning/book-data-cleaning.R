# Author: Andrew Disher
# Date: 10/17/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`, case_when, filter, group_by, mutate, n, select, summarize, ungroup], 
  ggplot2[...],
  udpipe[...]
)

# -------------------------------------
# ----- Negation of %in% function -----
# -------------------------------------

`%notin%` <- Negate(`%in%`)

# ---------------------------
# ----- Import the Data -----
# ---------------------------

# File names
file_names <- c("data/raw_data/LOTR1_book_CLEAN.txt",
                "data/raw_data/LOTR2_book_CLEAN.txt",
                "data/raw_data/LOTR3_book_CLEAN.txt")

# Initialize list of book strings
book_df <- data.frame(doc_id = 1:3,
                      text = c("", "", ""))

# Iterate through each file name and read text
for (file_index in 1:length(file_names)) {
  my_file <- readChar(con = file_names[file_index], 
                      nchars = file.info(file_names[file_index])$size) %>% 
    # Convert to character (just in case)
    as.character()
  
  # Store the result
  book_df[file_index, 2] <- my_file
}

# ---------------------------------------
# ----- Annotate Tokens in Data Set -----
# ---------------------------------------

# Download udpipe english model
udmodel <- udpipe_download_model(language = "english")
udmodel

# Get words and parts of speech from all books in book_df
book_data_df <- udpipe(x = book_df,
                       object = udmodel)

# Write the results to a csv file
write.csv(x = book_data_df,
          file = "data/cleaned_data/annotated_books.csv",
          row.names = FALSE)

# --------------------------------------
# ----- Remove all Unwanted Tokens -----
# --------------------------------------

# Unwanted tokens include those classified as punctuation (PUNCT) and proper nouns (PROPN). 
# Punctuation is not useful, and proper nouns are too domain specific to be of
# use. Also, numerals (NUM), symbols (SYM), particles (PART), determiners (DET),
# and other (X) are all not useful. 

unwanted_tags <- c("PUNCT", "PROPN", "NUM", "SYM", "PART", "DET", "X")

intermediate_df <- book_data_df %>% 
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
cleaned_book_data <- intermediate_df %>% 
  select(token, lemma, custom_pos)

# Force all characters in lemma column to be lower case
cleaned_book_data <- cleaned_book_data %>% 
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
bad_symbols <- c("'", "\"", "?", "!", "-", ",", ".", "’", "—")

# Apply the function to remove symbols before aggregating
cleaned_book_data <- cleaned_book_data %>% 
  mutate(lemma = lapply(lemma, FUN = strip_symbols, bad_symbols = bad_symbols))

# -------------------------------------------------------
# ----- Aggregate Data to Acquire Lemma Frequencies -----  
# -------------------------------------------------------

book_data_freq <- cleaned_book_data %>% 
  group_by(lemma, custom_pos) %>% 
  summarize(Frequency = n()) %>% 
  ungroup()

# Redefine book_data_freq such that columns aren't list type
book_data_freq <- book_data_freq %>% 
  mutate(lemma = unlist(lemma),
         custom_pos = unlist(custom_pos),
         Frequency = unlist(Frequency))

# -------------------------------------------
# ----- Write the Frequency Data to CSV -----
# -------------------------------------------

# Write the results to a csv file
write.csv(x = book_data_freq,
          file = "data/cleaned_data/word_frequencies.csv",
          row.names = FALSE)




