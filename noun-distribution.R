# Author: Andrew Disher
# Date: 10/02/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`, group_by, mutate, n, summarize, ungroup], 
  ggplot2[...],
  gridExtra[grid.arrange],
  openNLP[Maxent_Sent_Token_Annotator, Maxent_Word_Token_Annotator, Maxent_POS_Tag_Annotator],
  NLP[annotate, as.String],
  tibble[tibble]
)


# ---------------------------
# ----- Import the Data -----
# ---------------------------

# File names
file_names <- c("data/LOTR1_book_CLEAN.txt",
                "data/LOTR2_book_CLEAN.txt",
                "data/LOTR3_book_CLEAN.txt")

# Initialize list of book strings
book_list <- list(book1 = "",
                  book2 = "",
                  book3 = "")

# Iterate through each file name and read text
for (file_index in 1:length(file_names)) {
  my_file <- readChar(con = file_names[file_index], 
                      nchars = file.info(file_names[file_index])$size) %>% 
    # Convert to special string object defined by NLP package. 
    as.String()
  
  # Store the result
  book_list[[file_index]] <- my_file
}

# -------------------------------------
# ----- Create openNLP Annotators -----
# -------------------------------------

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
POS_token_annotator <- Maxent_POS_Tag_Annotator() 

# Initialize list of annotated books
annotated_book_list <- list(book1 = "",
                            book2 = "",
                            book3 = "")

# Iterate through each book and annotate the text with above functions
for (book_index in 1:length(book_list)) {
  current_book <- annotate(s = book_list[[book_index]],
                           f = list(sent_token_annotator,
                                    word_token_annotator,
                                    POS_token_annotator))
  
  # Store the result
  annotated_book_list[[book_index]] <- current_book
}

# --------------------------------------------------
# ----- Acquire Individual Words and their POS -----
# --------------------------------------------------

# Initialize a list of data frames to contain books, with their words and POS tags
word_pos_df_list <- list(book1 = "",
                          book2 = "",
                          book3 = "")

# Define the parts of speech codes we are interested in (all nouns, except proper nouns)
wanted_pos <- c("NN", "NNS", 
                # "NNP", "NNPS", "PRP", "PRP$",
                "PDT", "POS")

# Iterate through annotated_book_list to acquire the three words data frames
for (book_index in 1:length(annotated_book_list)) {
  # Acquire parts of speech for current book
  word_pos <- subset(annotated_book_list[[book_index]], type == "word")
  
  # Get only a vector for the actual parts of speech tags
  pos_tags <- sapply(word_pos$features, "[[", "POS")
  
  # Get the vector of words corresponding with the POS indices
  words_vector <- book_list[[book_index]][word_pos]
  
  # Create a data frame containing both vectors, with which we can use for analyses
  word_pos_df <- data.frame(Token = words_vector,
                            POS = pos_tags,
                            stringsAsFactors = FALSE)
  
  # Subset using the wanted parts of speech vector
  nouns_df <- subset(word_pos_df, POS %in% wanted_pos)
  
  # Convert all noun token to lower case
  nouns_df <- nouns_df %>% 
    mutate(Token = tolower(Token))
  
  # Compute token frequencies
  group_nouns_df <- nouns_df %>% 
    group_by(Token) %>% 
    summarize(Frequency = n()) %>% 
    ungroup() %>% 
    as.data.frame()
  
  # Store the result
  word_pos_df_list[[book_index]] <- group_nouns_df
}

# ------------------------------------------------------------
# ----- Function to Strip Symbols from Words Data Frames -----
# ------------------------------------------------------------

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

# -----------------------------------
# ----- Define %notin% Function -----
# -----------------------------------

`%notin%` <- Negate(`%in%`)

# --------------------------------
# ----- Cleaning Data Frames -----
# --------------------------------

# Define a vector of symbols to remove from the ends of words
bad_symbols <- c("'", "\"", "?", "!", "-", ",", ".", "’", "—")

# Initialize a list to contain cleaned data frames
cleaned_word_df_list <- list(book1 = "",
                             book2 = "",
                             book3 = "")

# Iterate through list and clean words
for (book_index in 1:length(word_pos_df_list)) {
  # Use function to remove symbols
  cleaned_df <- word_pos_df_list[[book_index]] %>% 
    mutate(Token = lapply(X = Token, FUN = strip_symbols, bad_symbols = bad_symbols))
  
  # Subset to remove "words" that are a single letter, empty string
  cleaned_df <- subset(cleaned_df, Token %notin% c(letters, ""))
  
  # Aggregate according to new cleaned words (some repeat)
  cleaned_df <- cleaned_df %>% 
    group_by(Token) %>% 
    summarize(Frequency = sum(Frequency)) %>% 
    ungroup() %>% 
    as.data.frame()
  
  cleaned_word_df_list[[book_index]] <- cleaned_df
}

# --------------------------------
# ----- Create ggplot2 graph -----
# --------------------------------

# Book 1
hist1 <- ggplot(data = cleaned_word_df_list$book1, mapping = aes(x = Frequency)) +
  geom_histogram(fill = "#166dfc", color = "black", bins = 60) +
  theme_minimal() +
  labs(x = "",
       y = "Count",
       title = "Distribution of Noun Word Frequencies (Book 1)")

# Book 2
hist2 <- ggplot(data = cleaned_word_df_list$book2, mapping = aes(x = Frequency)) +
  geom_histogram(fill = "#166dfc", color = "black", bins = 60) +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Distribution of Noun Word Frequencies (Book 2)")

# Book 3
hist3 <- ggplot(data = cleaned_word_df_list$book3, mapping = aes(x = Frequency)) +
  geom_histogram(fill = "#166dfc", color = "black", bins = 60) +
  theme_minimal() +
  labs(x = "Word Frequency",
       y = "Count",
       title = "Distribution of Noun Word Frequencies (Book 3)")

# Bind the three data sets and plot histogram for all books
all_books_words <- rbind(cleaned_word_df_list$book1,
                         cleaned_word_df_list$book2, 
                         cleaned_word_df_list$book3)

# Aggregate data set
all_books_words <- all_books_words %>% 
  group_by(Token) %>% 
  summarize(Frequency = sum(Frequency)) %>% 
  ungroup() %>% 
  as.data.frame()

# All books
hist_all <- ggplot(data = all_books_words, mapping = aes(x = Frequency)) +
  geom_histogram(fill = "#166dfc", color = "black", bins = 60) +
  theme_minimal() +
  labs(x = "Word Frequency",
       y = "",
       title = "Distribution of Noun Word Frequencies (All Books)")

# Display the plots in a grid
grid.arrange(hist1, hist2, hist3, hist_all, ncol = 2)

# Unique Nouns [1] 3069
subset(all_books_words, Frequency == 1) %>% 
  nrow()

# Total Nouns [1] 6928
nrow(all_books_words)

# Proportion of Nouns that are Unique [1] 0.442985 = 44.29 %
3069 / 6928

# Unique words divided by total words used
nrow(all_books_words) / sum(all_books_words$Frequency)

# -------------------------------------------------
# ----- Write the full data sets to csv files -----
# -------------------------------------------------

csv_df <- data.frame(Token = all_books_words$Token %>% unlist(),
                     Frequency = all_books_words$Frequency %>% unlist())

write.csv(x = csv_df, file = "data/all_books_nouns_data", row.names = FALSE)
