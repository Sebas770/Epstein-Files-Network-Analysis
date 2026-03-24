  ##Cleaning Data 2022-11-28-Complaint-Pierson-v.-Black
  # 0. Load libraries
  library(tidyverse)
  library(readr)
  library(pdftools)
  
  # 1. Read the pdf file
  
  raw_text <- pdf_text("952002_2022_Cheri_Pierson_v_Leon_Black_et_al_SUMMONS___COMPLAINT_1.pdf")
  
  # 2. Delete Summons and last page
  
  raw_text <- raw_text[3:29]
  
  # 3. Joint all the text
  
  raw_text <- paste(raw_text, collapse = "\n")
  
  # 4. Save as a file txt
  
  write_file(raw_text, "2022-11-28-Complaint-Pierson-v.-Black.txt")
  
  
  # 5. Load the txt file.
  
  raw_text <- read_file("2022-11-28-Complaint-Pierson-v.-Black.txt")
  
  
  
  # 6. Clean the text using Regex (Regular Expressions)
  text_clean <- raw_text %>%
    #Delete all of the headers

    str_remove_all("[0-9]+ of 30") %>% 
    
    str_remove_all("FILED: NEW YORK COUNTY CLERK 11/28/2022 11:29 AM") %>%
    
    str_remove_all("NYSCEF DOC. NO. 1") %>%
    
    str_remove_all("INDEX NO. 952002/2022") %>%
    
    str_remove_all("RECEIVED NYSCEF: 11/28/2022") %>%
    
    
    # Replace line breaks (Enters) with a space
    str_replace_all("\n", " ") %>%
    
    # Remove extra spaces
    str_squish()
  
  
  
  # 7. Save as a .txt
  write_file(text_clean, "2022-11-28-Complaint-Pierson-v.-Black-C.txt")
