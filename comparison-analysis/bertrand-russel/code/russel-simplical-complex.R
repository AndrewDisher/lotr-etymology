# Author: Andrew Disher
# Date: 10/31/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`, arrange, filter, group_by, group_by_at, join_by, left_join, mutate_at, 
        summarize, summarize_at, ungroup]
)

# ---------------------------
# ----- Import the Data -----
# ---------------------------

lang_data_table <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/lang_data_table.csv")
complex_df <- read.csv(file = "comparison-analysis/bertrand-russel/data/cleaned_data/complex_df.csv")

# Something
compact_languages_df <- lang_data_table %>% 
  group_by(Identifier) %>% 
  summarize(Token_Freq = mean(Frequency),
            All_Languages = paste0(Languages, collapse = "_"))

# Join complex df and compact_languages_df
languages_all_data <- left_join(x = complex_df,
                                y = compact_languages_df,
                                by = join_by(Identifier == Identifier))

colnames(languages_all_data)[2] <- "Lang_Count"

# Acquire list of the data frame split on language Count
split_df <- split(languages_all_data, languages_all_data$Lang_Count)

# ----------------------------------------------------------------------
# ----- Create 21 data frames to store Data for Teams of Languages ----- 
# ----------------------------------------------------------------------

# Instantiate list to contain data frames
df_list <- list()

# Our split_df has splits 1 thru 21, but index 21 corresponds to the value 23.
# We need to account for this in the for loop. So, create 23 data frames, but have
# data frames 22 and 21 be empty when we populate them. 

# For loop to create data frames
for (split_index in 23:1) {
  # Specify name of df
  df_name <- paste0("split_df_", split_index)
  
  # Create new data frame for current split
  df_list[[split_index]] <- matrix(data = 0, 
                                   nrow = 1, 
                                   ncol = split_index + 1) %>% 
    as.data.frame()
                                   
}

# ---------------------------------------------------------------------
# ----- Populating the New Data Structure with Existing Simplices -----
# ---------------------------------------------------------------------

# Define which data frames in df_list should remain empty
empty_df_vector <- c(22, 21)

# For loop to store language teams data in newly created data frames
for (df_list_index in length(df_list):1) {
  
  # Condition checking to account for data frames of simplex dimensions that don't exist in the data
  if (df_list_index == 23) {
    split_index <- 21
  } else if (df_list_index %in% empty_df_vector) {
    split_index <- 0
  } else {
    split_index <- df_list_index
  }
  
  # Only iterate on non-zero split indices
  if (split_index != 0) {
    # Store current data frame
    temp_df <- split_df[[split_index]]
    
    # Iterate over its rows
    for (row in 1:nrow(temp_df)) {
      # Acquire language vector
      lang_list <- temp_df[row, 4] %>% 
        strsplit(split = "_", fixed = TRUE) %>% 
        unlist() %>% 
        sort()
      
      # Acquire value for token frequency
      token_freq <- temp_df[row, 3]
      
      # Bind language list to appropriate `high level` data frame
      df_list[[df_list_index]] <- rbind(df_list[[df_list_index]],
                                      c(lang_list, token_freq))
    }
    
    # Remove placeholder binding row of zeros in first row of each data frame
    df_list[[df_list_index]] <- df_list[[df_list_index]][-1, ]
    
    # # Convert token frequency column to numeric
    df_list[[df_list_index]][, df_list_index + 1] <- df_list[[df_list_index]][, df_list_index + 1] %>% 
      as.numeric()
  }
}

# -------------------------------------------------------------------------
# ----- Aggregate token frequency weights according to language teams -----
# -------------------------------------------------------------------------

for (df_index in length(df_list):1) {
  # Define grouping column names 
  cols2group <- colnames(df_list[[df_index]])[1:df_index]
  
  # Define column to summarize (the frequency column)
  col2summarize <- colnames(df_list[[df_index]])[df_index + 1]
  
  # Summarize the current data frame
  df_list[[df_index]] <- df_list[[df_index]] %>% 
    group_by_at(cols2group) %>% 
    summarize_at(.vars = col2summarize, .funs = sum) %>% 
    ungroup()
}

# ------------------------------------------------------------
# ----- Generating Unaccounted for Lower Order Simplices -----
# ------------------------------------------------------------

# Define df_list indices to iterate over. Data frames 21 and 22 should be omitted,
# because they have no data to contribute. We start at 2 because simplices with 
# dimension 1 can't be broken down further.
df_list_indices <- c(2:20, 23)

# Iterate to generate lower order simplices
for (df_index in df_list_indices) {
# for (df_index in 2:20) {
  # Store current data frame and its number of columns in variables
  current_df <- df_list[[df_index]]
  num_cols <- ncol(current_df)
  
  for (simplex_order in (num_cols - 2):1) {
    # Define temporary data frame for storage of generated sub-simplices
    storage_df <- matrix(data = 0, 
                         nrow = 1, 
                         ncol = simplex_order + 1) %>% 
      as.data.frame()
    
    for (row in 1:nrow(df_list[[df_index]])) {
      # Generate simplex subset
      simplex_subset <- combn(current_df[row, 1:(num_cols - 1)], m = simplex_order) %>%
        t()
      
      # Cbind vector of frequencies to simplex_subset. Vector of frequencies is obtained
      # by taking the frequency of current row (column V4) and replicating it a number of times. 
      simplex_subset <- cbind(simplex_subset, 
                              current_df[row, num_cols] %>% 
                                unlist() %>% 
                                unname() %>% 
                                rep(times = nrow(simplex_subset)))
      
      # Store simplex_subset in storage_df
      storage_df <- storage_df %>% 
        rbind(simplex_subset)
    }
    # Remove placeholder binding row of zeros in first row of storage_df
    storage_df <- storage_df[-1, ]
    
    # Coerce storage_df to be a data frame
    storage_df <- storage_df %>% 
      as.data.frame()
    
    # Append end result of storage_df to its corresponding lower order simplex data frame in df_list
    df_list[[simplex_order]] <- df_list[[simplex_order]] %>%
      rbind(storage_df) %>% 
      as.data.frame()
  }
}

# --------------------------------
# ----- Remove Rows of Zeros -----
# --------------------------------

# The placeholder rows in data frames 22 and 21 still exist. We need to remove them
# manually. 
df_list[[22]] <- df_list[[22]][-1, ]
df_list[[21]] <- df_list[[21]][-1, ]

# -------------------------------------------
# ----- Treating data frames in df_list -----
# -------------------------------------------

# Function to apply in dplyr verb functions
treat_df <- function(column) {
  new_column <- column %>% 
    unlist() %>% 
    unname()
  
  return(new_column)
}

# Treatment of data frames
for (df_index in 1:length(df_list)) {
  # Define column to summarize (the frequency column)
  cols2mutate <- colnames(df_list[[df_index]])
  
  # Treat data frame's columns
  df_list[[df_index]] <- df_list[[df_index]] %>% 
    mutate_at(.vars = cols2mutate, .funs = treat_df)
}

# -----------------------------------------------
# ----- Aggregate newly generated simplices -----
# -----------------------------------------------

for (df_index in length(df_list):1) {
  # Define grouping column names 
  cols2group <- colnames(df_list[[df_index]])[1:df_index]
  
  # Define column to summarize (the frequency column)
  col2summarize <- colnames(df_list[[df_index]])[df_index + 1]
  
  # Summarize the current data frame
  df_list[[df_index]] <- df_list[[df_index]] %>% 
    group_by_at(cols2group) %>% 
    summarize_at(.vars = col2summarize, .funs = sum) %>% 
    ungroup()
}

# ------------------------------------------
# ----- Write Data Frames to csv files -----
# ------------------------------------------

# Define data folder directory
directory <- "comparison-analysis/bertrand-russel/data/cleaned_data/simplicial_complex_data/"

# For loop to write data sets to csv
for (df_index in 1:length(df_list)) {
  write.csv(x = df_list[[df_index]],
            file = paste0(directory, "simplex-data-", df_index, ".csv"),
            row.names = FALSE)
} 

