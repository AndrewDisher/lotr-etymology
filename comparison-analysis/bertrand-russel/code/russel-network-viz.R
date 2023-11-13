# Author: Andrew Disher
# Date: 10/31/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  descriptio[weighted.table],
  dplyr[`%>%`, case_when, distinct, filter, group_by, join_by, left_join, mutate, 
        n, select, summarize, ungroup],
  ggplot2[...],
  scales[...],
  data.table[data.table],
  tidyr[gather],
  visNetwork[...],
  htmltools[HTML],
  igraph[layout_as_star]
)

# ---------------------------
# ----- Import the Data -----
# ---------------------------

essay_data_freq <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/essay_data_freq.csv")
word_lexicon <- read.csv(file = "data/cleaned_data/word_lexicon.csv")
language_data <- read.csv(file = "data/scraped_data/language-data.csv")

# ------------------------------------------
# ----- Replicating Some Data Cleaning -----
# ------------------------------------------

# --- Create Problem Words Filter DF ---
prob_words_df <- word_lexicon %>% 
  group_by(Word_Only, POS_Only) %>% 
  filter(n() > 1) %>% 
  ungroup()

filter_df <- prob_words_df %>% 
  select(Word_Only, POS_Only) %>%  
  distinct() 

# --- Create a New Lexicon ---
new_lexicon <- word_lexicon %>% 
  filter(!(Word_Only %in% filter_df$Word_Only & POS_Only %in% filter_df$POS_Only))

# --- Join the Relevant Data ---
joined_data <- left_join(essay_data_freq,
                         new_lexicon,
                         by = join_by(lemma == Word_Only,
                                      custom_pos == POS_Only)
)

joined_data <- joined_data %>% 
  filter(!is.na(Definitions)) %>% 
  select(lemma, custom_pos, Frequency, Definitions)

# ---------------------------------------------------------------
# ----- Create a Co-Occurrance Data Table For The Languages -----
# ---------------------------------------------------------------

# Create new column to contain language list for each word
lang_data_table <- data.frame()

# Extract languages present in Definitions column using language data frame
for (row in 1:nrow(joined_data)) {
  # Compare against current definition string
  string_to_check <- joined_data$Definitions[row]
  
  # Define/redefine empty list of languages
  language_list <- c()
  
  # For each language in the language list
  for (lang in 1:nrow(language_data)) {
    # If definition contains a mention to the current language
    if (grepl(pattern = language_data$Language[lang],
              x = string_to_check)) {
      
      language_list <- c(language_list, language_data[lang, 2]) 
    }
  }
  
  # Generate a data frame to contain individual language rows
  temp_df <- data.frame(Identifier = paste(joined_data[row, 1], joined_data[row, 2], sep = "_") %>% 
                          rep(times = length(language_list)),
                        Frequency = rep(joined_data[row, 3], times = length(language_list)),
                        Languages = language_list)
  
  # Bind the data frame to lang_data_table
  lang_data_table <- lang_data_table %>% 
    rbind(temp_df)
}

# Calculate cross product of identifier column and the languages column
cooccurance_df <- crossprod(table(lang_data_table[c(1, 3)]))
diag(cooccurance_df) <- 0
cooccurance_df <- as.data.frame(cooccurance_df)

# --------------------------------------------
# ----- Create Node And Edge Data Frames -----
# --------------------------------------------

# Node df
node_df <- cooccurance_df %>% 
  mutate(id = rownames(.),
         label = rownames(.),
         Shared_Words = rowSums(.)) %>% 
  select(id, label, Shared_Words)

rownames(node_df) <- NULL

# Edge df
edge_df <- cooccurance_df %>% 
  mutate(from = rownames(.)) %>% 
  gather(to, Frequency, `Ancient Greek`:Yiddish) %>% 
  mutate(Frequency = ifelse(Frequency == 0, NA, Frequency))

# Filter the edge df so we remove rows where connections aren't present
edge_df <- edge_df %>% 
  filter(!is.na(Frequency))

# -------------------------------------
# ----- Create a visNetwork Graph -----
# -------------------------------------

etymology_network <- visNetwork(nodes = node_df,
                                edges = edge_df,
                                main = list(text = "<div style = 'margin-bottom:10px;'>Network of English Etymologies</div>"),
                                submain = list(text = HTML("<div style = 'margin-bottom:10px;'>Which Languages have commonly influenced English Words?</div>"))) %>% 
  visIgraphLayout(layout = "layout_as_star", center = "Old English") %>% 
  visOptions(highlightNearest = list(enabled = TRUE,
                                     degree = 0,
                                     hover = TRUE),
             nodesIdSelection = TRUE) %>% 
  visNodes(font = list(size = 40))

# -------------------------------------------------------
# ----- Exploring Number of Connections by Language -----
# -------------------------------------------------------

# Group edge_df by from and aggregate the number of rows (connections) by language
connect_count <- edge_df %>% 
  group_by(from) %>% 
  summarize(Connections = n())

# Sort the list by connections in descending order
connect_count <- connect_count[order(connect_count$Connections, decreasing = TRUE),]

# Rename from column to language
colnames(connect_count)[1] <- "Language"

# ----------------------------------------
# ----- Creating a New Network Graph -----
# ----------------------------------------

# First, add two more rows to the connection count data frame for Cherokee and Indic (0 connections)
connect_count <- connect_count %>% 
  rbind(data.frame(Language = c("Mende"),
                   Connections = c(0)))

# Second, bind the language connections counts to the node data frame
new_node_df <- left_join(x = node_df,
                         y = connect_count,
                         by = join_by(id == Language))

# Create some new columns for network options
new_node_df <- new_node_df %>% 
  mutate(title = paste0(label, ": ", Connections),
         color.highlight.background = "yellow",
         color.highlight.border = "black")

# Rename Connections column to value. This is for resizing nodes in the network
colnames(new_node_df)[4] <- "value"

# Recreate the network
network_2 <- visNetwork(nodes = new_node_df,
                        edges = edge_df,
                        main = list(text = "<div style = 'margin-bottom:10px;'>Network of English Etymologies</div>"),
                        submain = list(text = HTML("<div style = 'margin-bottom:10px;'>Which Languages have commonly influenced English Words?</div>"))) %>% 
  visIgraphLayout(layout = "layout_as_star", center = "German") %>% 
  visOptions(highlightNearest = list(enabled = TRUE,
                                     degree = 0,
                                     hover = TRUE),
             nodesIdSelection = TRUE) %>% 
  visNodes(font = list(size = 40))

# ---------------------------------
# ----- Trying Something Else -----
# ---------------------------------

# Group lang_data_table by Identifier and aggregate
complex_df <- lang_data_table %>% 
  group_by(Identifier) %>% 
  summarize(Count = n()) %>% 
  ungroup()

# Reorder the data frame in increasing order
complex_df <- complex_df[order(complex_df$Count, decreasing = TRUE),]

# Examine the distribution of Count
ggplot(data = complex_df, mapping = aes(x = Count)) +
  geom_histogram(fill = "#0099f9", color = "black", binwidth = 1) +
  labs(x = "Count of Languages (n)",
       y = "Frequency",
       title = "Distribution of Language 'Complexes'") +
  theme_minimal()

# ---------------------------------------------
# ----- Finding Simplical Complex Weights -----
# ---------------------------------------------

# Use descriptio weighted.table() function to find a weighted contingency table
weights_table <- weighted.table(x = lang_data_table$Identifier,
                                y = lang_data_table$Languages,
                                weights = lang_data_table$Frequency)

# Convert weights table to a data frame and transpose it
weights_table_df <- weights_table %>% 
  unclass() %>% 
  as.data.frame() %>% 
  t()

# ----- Testing -----

# Word: above_prep
colnames(weights_table_df)[13]

# Languages that have influence on the word
weights_table_df[, 13][weights_table_df[, 13] != 0] # There are 5

# Cross product of the vector
weights_table_df[, 13][weights_table_df[, 13] != 0] %>% 
  tcrossprod()

# Note: Cross product clearly produces numbers far too large. But, if we divide
# by the maximum value of the vector: 260
weights_table_df[, 13][weights_table_df[, 13] != 0] %>% 
  max()

# We get accurate numbers for each pair of languages
tcrossprod(weights_table_df[, 13][weights_table_df[, 13] != 0]) / max(weights_table_df[, 13][weights_table_df[, 13] != 0])

# This does not include all languages though, since we filtered out the ones that don't 
# influence the word. We also don't get the names of our languages unfortunately.

# This might be a solution:
test <- tcrossprod(weights_table_df[, 13]) / max(weights_table_df[, 13])

# We get the full matrix, without names, but we can insert names. If we iterate through
# the columns of the weights_table_df for each word, we can iteratively sum up the 
# matrices produced.

# Initialize a matrix of zeros, where m = 123, n = 123
result_matrix <- matrix(rep(0, times = 59^2), nrow = 59, ncol = 59)

for (column in 1:ncol(weights_table_df)) {
  # Compute the temp matrix
  temp_matrix <- tcrossprod(weights_table_df[, column]) / max(weights_table_df[, column])
  
  # Add to results matrix
  result_matrix <- result_matrix + temp_matrix
}

# Rename column and rows
colnames(result_matrix) <- colnames(cooccurance_df)
rownames(result_matrix) <- rownames(cooccurance_df)

# -------------------------------------
# ----- Write the Matrices to csv -----
# -------------------------------------

write.csv(cooccurance_df, file = "comparison-analysis/bertrand-russel/data/cleaned_data/unweighted_matrix.csv", row.names = TRUE)
write.csv(result_matrix, file = "comparison-analysis/bertrand-russel/data/cleaned_data/token_weighted_matrix.csv", row.names = TRUE)

write.csv(lang_data_table, file = "comparison-analysis/bertrand-russel/data/cleaned_data/lang_data_table.csv", row.names = FALSE)
write.csv(complex_df, file = "comparison-analysis/bertrand-russel/data/cleaned_data/complex_df.csv", row.names = FALSE)

