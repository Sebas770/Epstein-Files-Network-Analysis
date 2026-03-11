##Cleaning Data 2019-04-16-Complaint-Giuffre-v.-Dershowitz
# 0. Load libraries
library(tidyverse)
library(readr)
library(pdftools)

# 1. Read the pdf file

raw_text <- pdf_text("gov.uscourts.nysd.513818.1.0_2.pdf")

# 2. Delete first and last page

raw_text <- raw_text[2:27]

# 3. Joint all the text

raw_text <- paste(raw_text, collapse = "\n")

# 4. Save as a file txt

write_file(raw_text, "2019-04-16-Complaint-Giuffre-v.-Dershowitz.txt")


# 5. Load the txt file.

raw_text <- read_file("2019-04-16-Complaint-Giuffre-v.-Dershowitz.txt")



# 6. Clean the text using Regex (Regular Expressions)
text_clean <- raw_text %>%
  
  # Remove page headers
  str_remove_all("Case 1:19-cv-03377-LAP Document 1 Filed 04/16/19 Page [0-9]+ of 28") %>% 
  
  # Replace line breaks (Enters) with a space
  str_replace_all("\n", " ") %>%
  
  # Remove extra spaces
  str_squish()



# 7. Save as a .txt
write_file(text_clean, "2019-04-16-Complaint-Giuffre-v.-Dershowitz-C.txt")
