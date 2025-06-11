#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl) 
#install.packages("readxl", dependencies = TRUE)

# Load all data form GitHub

GDB_df <- read_xlsx(path = "tec00115_page_spreadsheet.xlsx", "Sheet 1") 
Unemploymentlang <- read_xlsx(path = "lfsa_ugad$defaultview_spreadsheet.xlsx", "Sheet 1")
View(Unemploymentlang)
unemp_men <- readxl::read_xlsx("lfsa_ugad$defaultview_spreadsheet.xlsx", sheet = "Sheet 6")
unemp_women <- readxl::read_xlsx("lfsa_ugad$defaultview_spreadsheet.xlsx", sheet = "Sheet 11")

# clean data unemp_women
unemp_women <- unemp_women[14:49, ]
unemp_women <- unemp_women[, -c( 3, 5, 7 ,9 ,11 ,13 ,15 ,17 ,19 ,21)]
rownames(unemp_women) <- NULL
colnames(unemp_women)[2:11] <- as.character(2015:2024)
unemp_women[, 2:11] <- lapply(unemp_women[, 2:11], function(x) as.numeric(as.character(x)))
unemp_women$avg_women <- rowMeans(unemp_women[, 2:11], na.rm = TRUE)

# clean data unemp_men
unemp_men <- unemp_men[14:49, ]
unemp_men <- unemp_men[, -c( 3, 5, 7 ,9 ,11 ,13 ,15 ,17 ,19 ,21)]
rownames(unemp_men) <- NULL
colnames(unemp_men)[2:11] <- as.character(2015:2024)
unemp_men[, 2:11] <- lapply(unemp_men[, 2:11], function(x) as.numeric(as.character(x)))
unemp_men$avg_men <- rowMeans(unemp_men[, 2:11], na.rm = TRUE)


# Clean data Unemployment 
Unemploymentlang <- Unemploymentlang[14:49, ]
Unemploymentlang <- Unemploymentlang[, -c( 3, 5, 7 ,9 ,11 ,13 ,15 ,17 ,19 ,21)]
rownames(Unemploymentlang) <- NULL
colnames(Unemploymentlang)[2:11] <- as.character(2015:2024)

# Creating a new variable in the Unemploymentlang dataset
Unemploymentlang$avg_women <- unemp_women$avg_women
Unemploymentlang$avg_men <- unemp_men$avg_men


# Clean data GDP

GDB_df <- GDB_df[10:52, ]
rownames(GDB_df) <- NULL
GDB_df <- GDB_df[, colSums(!is.na(GDB_df)) > 0]
GDB_df <- GDB_df[, -c( 5, 12, 14, 16, 18)]
colnames(GDB_df)[2:13] <- as.character(2013:2024)
GDB_df[ , as.character(2013:2024)] <- lapply(GDB_df[ , as.character(2013:2024)], as.numeric)
GDB_df <- GDB_df[-c(1,2,3),]

# Creating a new variable in the GDP dataset

GDB_df <- GDB_df %>%
  rowwise() %>%
  mutate(
    GDP_before_covid = round((prod(1 + c_across(`2015`:`2019`) / 100, na.rm = TRUE) - 1) * 100, 2),
    GDP_after_covid  = round((prod(1 + c_across(`2020`:`2024`) / 100, na.rm = TRUE) - 1) * 100, 2)
  ) %>%
  ungroup()
View(GDB_df)

# Temporal variation plot: EU GDP growth from 2015 to 2024

gdp_growth <- GDB_df %>%
  select(all_of(as.character(2013:2024)))


avg_growth_per_year <- colMeans(gdp_growth, na.rm = TRUE) / 100  # omzetting naar factor


growth_df <- data.frame(
  Year = 2013:2024,
  GrowthRate = avg_growth_per_year
)


growth_df <- growth_df %>%
  mutate(
    TotalGDP = cumprod(1 + GrowthRate) * 100  # basisjaar = 100
  )


ggplot(growth_df, aes(x = Year, y = TotalGDP)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue") +
  geom_vline(xintercept = 2020, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = 2013:2024) +  # elk jaar tonen
  labs(
    title = "Cumulative EU GDP Growth (2013–2024)",
    subtitle = "Base year = 100, red dashed line = COVID-19 start",
    x = "Year",
    y = "Cumulative GDP Index"
  ) +
  theme_minimal ()


























library(tidyverse)

# Pivot both datasets to long form and add a Gender column
men_long <- unemp_men %>%
  select(Country = 1, `2015`:`2024`) %>%
  pivot_longer(`2015`:`2024`, names_to = "Year", values_to = "Unemployment") %>%
  mutate(Gender = "Men")

women_long <- unemp_women %>%
  select(Country = 1, `2015`:`2024`) %>%
  pivot_longer(`2015`:`2024`, names_to = "Year", values_to = "Unemployment") %>%
  mutate(Gender = "Women")

# Combine and factor Year
unemp_gender <- bind_rows(men_long, women_long) %>%
  mutate(Year = factor(Year, levels = as.character(2015:2024)))

# Plot the boxplot with a shorter y–axis
ggplot(unemp_gender, aes(x = Year, y = Unemployment, fill = Gender)) +
  geom_boxplot(
    position      = position_dodge(width = 0.8),
    outlier.shape = NA
  ) +
  scale_y_continuous(
    limits = c(0, 30),         # set the lower & upper bounds
    expand = expansion(0, 0)   # remove padding at axis ends
  ) +
  labs(
    title = "Unemployment Rates: Men vs Women (EU Countries, 2015–2024)",
    x     = "Year",
    y     = "Unemployment (%)",
    fill  = "Gender"
  ) +
  theme_minimal()



