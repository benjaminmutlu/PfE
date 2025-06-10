install.packages("readxl")
library(readxl) 

# Load all data form GitHub

GDB_df <- read_xlsx(path = "tec00115_page_spreadsheet.xlsx", "Sheet 1") 
Unemploymentlang <- read_xlsx(path = "lfsa_ugad$defaultview_spreadsheet.xlsx", "Sheet 1") 

# Clean data Unemployment 

Unemploymentlang <- Unemploymentlang[12:49, ]
Unemploymentlang <- Unemploymentlang[, -c( 3, 5, 7 ,9 ,11 ,13 ,15 ,17 ,19 ,21)]
rownames(Unemploymentlang) <- NULL

# Clean data GDP

GDB_df <- GDB_df[10:52, ]
rownames(GDB_df) <- NULL
GDB_df <- GDB_df[, colSums(!is.na(GDB_df)) > 0]
GDB_df <- GDB_df[, -c( 5, 12, 14, 16, 18)]
colnames(GDB_df)[2:13] <- as.character(2013:2024)
GDB_df <- GDB_df[-c(1,2,3),]
Unemploymentlang <- Unemploymentlang[-c(1, 2), ]


