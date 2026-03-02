##Cleaning Data 2016-04-26-Complaint-Johnson-v.-Trump-and-Epstein

# 0. Load libraries
library(tidyverse)
library(readr)

# 1. Read the text file
# Read document as one long text string
raw_text <- read_file("2016-04-26-Complaint-Johnson-v.-Trump-and-Epstein.txt")

# 2. Clean the text using Regex (Regular Expressions)
text_clean <- raw_text %>%
  
  # Replace line breaks (Enters) with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra spaces
  str_squish()

# 3. Save as a .txt
write_file(text_clean, "2016-04-26-Complaint-Johnson-v.-Trump-and-Epstein-C.txt")
