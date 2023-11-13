# Author: Andrew Disher
# Date: 10/01/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`, distinct, join_by, left_join, mutate]
)

# ---------------------------
# ----- Import the Data -----
# ---------------------------

words <- read.csv(file = "data/scraped_data/word-etymologies.csv")

# ------------------------------------------------------
# ----- Function to Extract Word from Words Column -----
# ------------------------------------------------------
extract_word <- function(word_pos) {
  word_only <- strsplit(word_pos, 
                        split = " (", 
                        fixed = TRUE)[[1]][1]
  
  return(word_only)
}

# ----------------------------------------------
# ----- Function to Extract Part of Speech -----
# ----------------------------------------------
extract_pos <- function(word_pos) {
  # Remove first parenthesis
  pos_only <- strsplit(word_pos,
                       split = " (",
                       fixed = TRUE)[[1]][2]
  
  # Remove second parenthesis
  pos_only <- strsplit(pos_only,
                       split = ")",
                       fixed = TRUE)[[1]]
  
  # Check if string contains a comma or slash and split
  if (grepl(pattern = ", ", x = pos_only)) {
    pos_only <- strsplit(pos_only,
                         split = ", ",
                         fixed = TRUE)
  } else {
    pos_only <- strsplit(pos_only,
                         split = "/",
                         fixed = TRUE)
  }
  
  # If there are multiple parts of speech, address each separately
  # if (length(pos_only[[1]]) > 1) {
    # Split on period (parts of speech are abbreviated with a period)
    for (element in 1:length(pos_only[[1]])) {
      pos_only[[1]][element] <- strsplit(pos_only[[1]][element],
                                         split = "[.].*")[[1]]
    }
  # }
  
  # Split on period (parts of speech are abbreviated with a period)
  # pos_only <- strsplit(pos_only,
  #                      split = ".",
  #                      fixed = TRUE)[[1]]
  
  # If part of speech is NA, instead of text
  if (is.na(pos_only)) { 
    return("unclear") 
  }
  else { return(pos_only) }

  # return(pos_only)
}

# ----------------------------------------------
# ----- Isolate Words from Parts of Speech -----
# ----------------------------------------------

# Isolate parts of speech from word strings
cleaned_words <- words %>% 
  mutate(Word_Only = lapply(Words, FUN = extract_word), 
         POS_Only = lapply(Words, FUN = extract_pos), 
         .after = 2) 

# ----------------------------
# ----- Remove Non-Words -----
# ----------------------------

# Function to remove strings from data set that begin or end with a dash, '-' 
is_non_word <- function(word) {
  # Split word up by character
  characters_in_word <- strsplit(word, "")[[1]]
  
  # Find position of last character
  last_char_position <- length(characters_in_word)
  
  # Define characters that indicate non-words
  bad_symbols <- c("-", "*")
  
  # Check if first character is in bad_symbols
  if (characters_in_word[1] %in% bad_symbols || characters_in_word[last_char_position] %in% bad_symbols) {
    return(TRUE)
  } 
  else {
    return(FALSE)
  }
}

# Use the function to remove non-words
cleaned_words <- cleaned_words %>% 
  mutate(is_non_word = lapply(Word_Only, FUN = is_non_word))

cleaned_words <- subset(cleaned_words, is_non_word == FALSE)

# --------------------------------------------
# ----- Remove Proper Nouns and Acronyms -----
# --------------------------------------------

# Function to remove words beginning with capital letters
is_proper <- function(word) {
  # Split word up by character
  characters_in_word <- strsplit(word, "")[[1]]
  
  # Determine if first character is capitalized
  if(characters_in_word[1] == toupper(characters_in_word[1])) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# Use the function to remove proper nouns and acronyms
cleaned_words <- cleaned_words %>% 
  mutate(is_proper = lapply(Word_Only, FUN = is_proper))

cleaned_words <- subset(cleaned_words, is_proper == FALSE)

# -------------------------------------------------
# ----- Unlist POS Column and Insert New Rows -----
# -------------------------------------------------

# Unlist the parts of speech
cleaned_words <- cleaned_words %>% 
  mutate(POS_Only = lapply(POS_Only, FUN = unlist))

# Reset row names
rownames(cleaned_words) <-  NULL

# Initialize another data frame to contain surplus parts of sppech rows
surplus_df <- data.frame()

# For loop to treat words with >1 POS, by creating new rows
for (row in 1:nrow(cleaned_words)) {
  current_pos <- cleaned_words[row, 4][[1]]
  
  # Check length of current_pos
  if (length(current_pos) > 1) {
    for (pos_index in 2:length(current_pos)) {
      # Create a new df row
      new_row <- cleaned_words[row, ]
      
      # Define its Part of Speech
      new_row[4] <- current_pos[pos_index]
      
      # Bind the new row to the surplus data frame
      surplus_df <- surplus_df %>% 
        rbind(new_row)
    }
    
    # Finally, redefine cleaned_words's POS as the first value in its previous POS_Only vector
    cleaned_words[row, 4] <- current_pos[1]
  }
}

# Bind the surplus data frame to cleaned words
cleaned_words <- cleaned_words %>% 
  rbind(surplus_df)

# Remove last two boolean columns
cleaned_words <- cleaned_words[, 1:5]

# Rest row names again
rownames(cleaned_words) <- NULL

# Some rows are repeated (not just the word with different part of speech)
# Remove non-unique rows

cleaned_words <- distinct(cleaned_words)


# ---------------------------------
# ----- Write the Data to CSV -----
# ---------------------------------

# Convert list structure of cleaned_words to data frame
cleaned_words <- cleaned_words %>% 
  mutate(Letters = unlist(Letters),
         Words = unlist(Words),
         Word_Only = unlist(Word_Only),
         POS_Only = unlist(POS_Only),
         Definitions = unlist(Definitions))

# Now save the data
write.csv(x = cleaned_words,
          file = "data/cleaned_data/word_lexicon.csv",
          row.names = FALSE)
