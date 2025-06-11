# Load necessary libraries

install.packages("readxl")
library(readxl) 
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("readr")
library(readr)

# Load all data from GitHub

GDP_df <- read_xlsx(path = "tec00115_page_spreadsheet.xlsx", "Sheet 1") 
Unemploymentlang <- read_xlsx(path = "lfsa_ugad$defaultview_spreadsheet.xlsx", "Sheet 1") 


# Clean data Unemployment 

Unemploymentlang <- Unemploymentlang[12:49, ]
Unemploymentlang <- Unemploymentlang[, -c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)]
rownames(Unemploymentlang) <- NULL
Unemploymentlang <- Unemploymentlang[-c(1, 2), ]

# Clean data GDP

GDP_df <- GDP_df[10:52, ]
rownames(GDP_df) <- NULL
GDP_df <- GDP_df[, colSums(!is.na(GDP_df)) > 0]
GDP_df <- GDP_df[, -c(5, 12, 14, 16, 18)]
colnames(GDP_df)[2:13] <- as.character(2013:2024)
GDP_df <- GDP_df[-c(1,2,3), ]

# add variables in GDP_df (GDP_before_covid and GDP_after_covid)

GDP_df <- GDP_df %>%
  mutate(
    across(`2015`:`2024`, ~ na_if(., ":"))
  ) %>%
  mutate(
    across(`2015`:`2024`, ~ parse_number(as.character(.)))
  ) %>%
  rowwise() %>%
  mutate(
    GDP_before_covid = round((prod(1 + c_across(`2015`:`2019`)/100, na.rm = TRUE) - 1) * 100, 2),
    GDP_after_covid  = round((prod(1 + c_across(`2020`:`2024`)/100, na.rm = TRUE) - 1) * 100, 2)
  ) %>%
  ungroup()



