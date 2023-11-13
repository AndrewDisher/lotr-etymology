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
  scales[...]
)

# ---------------------------
# ----- Import the Data -----
# ---------------------------

book_data_freq <- read.csv(file = "data/cleaned_data/word_frequencies.csv")
word_lexicon <- read.csv(file = "data/cleaned_data/word_lexicon.csv")
language_data <- read.csv(file = "data/scraped_data/language-data.csv")

# -----------------------------------------------------------
# ----- Explanation of and Correction for Problem Words -----
# -----------------------------------------------------------

# Some words in our lexicon are spelled the same AND have the same part of speech.
# An example would be the word `bank`. There are two nouns with this spelling, 
# with one referring to a financial institution and the other referring to a sloped
# piece of land. 

# While words that are spelled the same BUT do NOT have the same part of speech are
# not a problem when joining our lexicon to our frequency data, the aforementioned 
# problem words do pose serious hurdles. At the moment, there is no way for us to 
# distinguish between the two nouns spelled as `bank` (or the three verbs spelled
# in the same way!!!) in our word frequency data set, so we need to remove them from 
# our analysis.

# To do this, we'll compile a data frame containing words that follow this pattern,
# and use it to remove them from our lexicon. 

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

joined_data <- left_join(book_data_freq,
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


# ---------------------------------------------
# ----- Write Language Counts Data to csv -----
# ---------------------------------------------

write.csv(x = language_counts, file = "data/cleaned_data/language_counts.csv",
          row.names = FALSE)

