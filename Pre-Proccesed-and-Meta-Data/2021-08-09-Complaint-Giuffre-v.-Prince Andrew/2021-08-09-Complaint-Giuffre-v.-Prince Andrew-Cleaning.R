##Cleaning Data 2021-08-09-Complaint-Giuffre-v.-Prince Andrew
#set
# 0. Load libraries
library(tidyverse)
library(readr)
library(pdftools)

# 1. Read the pdf file

raw_text <- pdf_text("gov.uscourts.nysd.564713.1.0_5.pdf")

# 2. Delete first and last page

raw_text <- raw_text[2:14]

# 3. Joint all the text

raw_text <- paste(raw_text, collapse = "\n")

# 4. Save as a file txt

write_file(raw_text, "2021-08-09-Complaint-Giuffre-v.-Prince Andrew.txt")


# 5. Load the txt file.

raw_text <- read_file("2021-08-09-Complaint-Giuffre-v.-Prince Andrew.txt")



# 6. Clean the text using Regex (Regular Expressions)
text_clean <- raw_text %>%
  
  # Remove page headers (e.g., "Case 1:21-cv-06702-LAK Document 1 Filed 08/09/21 Page 2 of 15")
  str_remove_all("Case 1:21-cv-06702-LAK Document 1 Filed 08/09/21 Page [0-9]+ of 15") %>% 
  
  # Replace line breaks (Enters) with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra spaces
  str_squish()



# 7. Save as a .txt
write_file(text_clean, "2021-08-09-Complaint-Giuffre-v.-Prince Andrew-C.txt")
