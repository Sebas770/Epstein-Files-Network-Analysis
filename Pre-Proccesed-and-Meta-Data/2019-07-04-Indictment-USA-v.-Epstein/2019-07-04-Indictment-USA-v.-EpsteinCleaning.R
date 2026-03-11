##Cleaning Data 2019-07-04-Indictment-USA-v.-Epstein

# 0. Load libraries
library(tidyverse)
library(readr)

# 1. Read the text file
# Read document as one long text string
raw_text <- read_file("2019-07-04-Indictment-USA-v.-Epstein.txt")

# 2. Clean the text using Regex (Regular Expressions)
text_clean <- raw_text %>%
  
  # Remove page headers (e.g., "--- PAGE 1 ---")
  str_remove_all("--- PAGE [0-9]+ ---") %>% 
  
  # Replace line breaks (Enters) with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra spaces
  str_squish()

# 3. Save as a .txt
write_file(text_clean, "2019-07-04-Indictment-USA-v.-Epstein-C.txt")




