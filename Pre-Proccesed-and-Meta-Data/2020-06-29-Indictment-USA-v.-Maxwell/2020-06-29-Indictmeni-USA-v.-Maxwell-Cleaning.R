  ##Cleaning Data 2020-06-29-Indictmeni-USA-v.-Maxwell
  # 0. Load libraries
  library(tidyverse)
  library(readr)
  library(pdftools)
  

  # 1. Load the txt file.
  
  raw_text <- read_file("2020-06-29-Indictmeni-USA-v.-Maxwell.txt")
  
  
  
  # 2. Clean the text using Regex (Regular Expressions)
  text_clean <- raw_text %>%
    
    # Replace line breaks (Enters) with a space
    str_replace_all("\n", " ") %>%
    
    # Remove extra spaces
    str_squish()
  
  
  
  # 3. Save as a .txt
  write_file(text_clean, "2020-06-29-Indictmeni-USA-v.-Maxwell-C.txt")

  