  ##Cleaning Data 2022-11-24-Complaint-Doe-v.-Deutsche-Bank
  # 0. Load libraries
  library(tidyverse)
  library(readr)
  library(pdftools)
  
  # 1. Read the pdf file
  
  raw_text <- pdf_text("gov.uscourts.nysd.590047.1.0.pdf")
  
  # 2. Delete last two pages
  
  raw_text <- raw_text[1:102]
  
  # 3. Joint all the text
  
  raw_text <- paste(raw_text, collapse = "\n")
  
  # 4. Save as a file txt
  
  write_file(raw_text, "2022-11-24-Complaint-Doe-v.-Deutsche-Bank.txt")
  
  
  # 5. Load the txt file.
  
  raw_text <- read_file("2022-11-24-Complaint-Doe-v.-Deutsche-Bank.txt")
  
  
  
  # 6. Clean the text using Regex (Regular Expressions)
  text_clean <- raw_text %>%
    
    # Remove page headers (e.g., "Case 1:21-cv-06702-LAK Document 1 Filed 08/09/21 Page 2 of 15")
    str_remove_all("Case 1:22-cv-10018 Document 1 Filed 11/24/22 Page [0-9]+ of 104") %>% 
    
    # Replace line breaks (Enters) with a space
    str_replace_all("\n", " ") %>%
    
    # Remove extra spaces
    str_squish()
  
  
  
  # 7. Save as a .txt
  write_file(text_clean, "2022-11-24-Complaint-Doe-v.-Deutsche-Bank-C.txt")
