# Author: Andrew Disher
# Date: 8/28/2023
# Affiliation: UMASS Dartmouth

# --------------------
# ----- Packages ----- 
# --------------------

box::use(
  dplyr[`%>%`],
  polite[bow, scrape],
  purrr[map, pmap_df],
  rvest[html_nodes, html_text2, read_html]
)

# ----------------------------------------------------------------------
# ----- Generating Link Parameters to scrape data from Etymonline  -----
# ----------------------------------------------------------------------

# Vector of number of pages to scrape (each element is number of pages for a letter)
page_number_totals <- c(338, 303, 471, 270, 189, 212, 176, 206, 217, 56, 55, 187, 300, # M
                        112, 121, 438, 29, 254, 591, 232, 119, 75, 109, 4, 18, 14)

# Vector of actual page numbers
page_numbers <- c(1:338, 1:303, 1:471, 1:270, 1:189, 1:212, 1:176, 1:206, 1:217, 1:56, 1:55, 1:187, 1:300, # M
                  1:112, 1:121, 1:438, 1:29, 1:254, 1:591, 1:232, 1:119, 1:75, 1:109, 1:4, 1:18, 1:14)

# Create a vector of letters
letters_vector <- c()

# Loop through each letter and through their corresponding number of total pages
for (number in 1:26) {
  letters_vector <- c(letters_vector, rep(letters[number], 
                                          times = page_number_totals[number]))
}

# Instantiate data frame to house URLs
url_df <- data.frame(Letter = letters_vector,
                     Page = page_numbers)


# ---------------------------------------
# ----- Polite Web Scraper Function -----
# ---------------------------------------

# Start a session
session <- bow(url = "https://www.etymonline.com/search")

# Helper function to scrape words and definitions
scrape_data <- function(Letter, Page) {
  # Scrape pages
  responses <- scrape(session, query = list(page = Page, q = Letter, type = 0))
  
  # Acquire words
  words <- responses %>% 
    html_nodes("a.word__name--TTbAA") %>% 
    html_text2()
  
  # Acquire definitions
  definitions <- responses %>% 
    html_nodes("section.word__defination--2q7ZH") %>% 
    html_text2() %>% 
    paste0()
  
  # Construct a temporary data frame
  return_df <- data.frame(Letters = Letter,
                          Words = words,
                          Definitions = definitions)
  
  return(return_df)
}

# ----------------------------
# ----- Use the function -----
# ----------------------------

# Create a list of numeric vectors. This will be used to generate data frame
# partitions, which will be bound together after.
partition_list <- list(c(1:500), c(501:1000), 
                       c(1001:1500), c(1501:2000),
                       c(2001:2500), c(2501:3000),
                       c(3001:3500), c(3501:4000),
                       c(4001:4500), c(4501:5000),
                       c(5001:5096))

# Instantiate data frame to store results of web scraping.
final_data_df <- data.frame()

t0 <- Sys.time()
# Loop through the partitions to scrape all of the data politely.
for (partition in 1:11) {
  etymology_partition <- url_df[partition_list[[partition]], ] %>% 
    pmap_df(scrape_data)
  
  # rbind the etymology data to the instantiated data frame
  final_data_df <- rbind(final_data_df, etymology_partition)
}
t1 <- Sys.time()

# Write the results to csv file
write.csv(final_data_df, file = "data/scraped_data/word-etymologies.csv", row.names = FALSE)


# -------------------------------------------------------------------------
# ---------------- Nationsonline.org Language Webscraping -----------------
# -------------------------------------------------------------------------

language_html <- read_html("https://www.nationsonline.org/oneworld/language_code.htm")

session <- bow(url = "https://www.nationsonline.org/oneworld/language_code.htm")

# -----------------------------------
# ----- Function to Scrape Data -----
# -----------------------------------

scrape_languages <- function(session) {
  response <- scrape(session)
  
  language_names <- response %>% 
    html_nodes("tr.border1>td:first-child") %>% 
    html_text2()
}

language_data <- scrape_languages(session = session)

write.csv(language_data, file = "data/scraped_data/language-data.csv", row.names = TRUE)
