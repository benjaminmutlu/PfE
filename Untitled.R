#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("readxl", dependencies = TRUE)
#install.packages("rworldmap")
#install.packages("dplyr")

library(ggplot2)
library(rworldmap)
library(dplyr)
library(readxl) 
library(tidyverse)

# Load all data form GitHub

Total_Work_population <- read_xlsx(path = "tipslm16_page_spreadsheet.xlsx", "Sheet 1") 
GDP_df <- read_xlsx(path = "tec00115_page_spreadsheet.xlsx", "Sheet 1") 
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

GDP_df <- GDP_df[10:52, ]
rownames(GDP_df) <- NULL
GDP_df <- GDP_df[, colSums(!is.na(GDP_df)) > 0]
GDP_df <- GDP_df[, -c( 5, 12, 14, 16, 18)]
colnames(GDP_df)[2:13] <- as.character(2013:2024)
GDP_df[ , as.character(2013:2024)] <- lapply(GDP_df[ , as.character(2013:2024)], as.numeric)
GDP_df <- GDP_df[-c(1,2,3),]

# Creating a new variable in the GDP dataset

GDP_df <- GDP_df %>%
  rowwise() %>%
  mutate(
    GDP_before_covid = round((prod(1 + c_across(`2015`:`2019`) / 100, na.rm = TRUE) - 1) * 100, 2),
    GDP_after_covid  = round((prod(1 + c_across(`2020`:`2024`) / 100, na.rm = TRUE) - 1) * 100, 2)
  ) %>%
  ungroup()

gdp_growth <- GDP_df %>%
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



# Clean data Total_Work_population
Total_Work_population <- Total_Work_population[13:39, ]
Total_Work_population <- Total_Work_population[, -c(2:41)]
Total_Work_population <- Total_Work_population[, -c( 3, 5, 7 ,9 ,11 ,13 ,15 ,17 ,19 ,21)]
rownames(Total_Work_population) <- NULL
colnames(Total_Work_population)[2:11] <- as.character(2015:2024)
Total_Work_population[, 2:11] <- lapply(Total_Work_population[, 2:11], function(x) as.numeric(as.character(x)))

# nieuwe data set werkloosheid in procenten 

colnames(Total_Work_population)[1]  <- "Country"
colnames(Unemploymentlang)[1]   <- "Country"

merged_df <- inner_join(Total_Work_population, Unemploymentlang, by = "Country")

years <- 2015:2024

# Turn all “.x” and “.y” year‑columns into numeric
num_x <- paste0(years, ".x")
num_y <- paste0(years, ".y")

merged_df[ num_x ] <- lapply(merged_df[ num_x ], as.numeric)
merged_df[ num_y ] <- lapply(merged_df[ num_y ], as.numeric)


# 1) Start met alleen de landnaam-kolom
unemployment_percent_df <- merged_df["Country"]

# 2) Loop wél de berekening per jaar áán binnen de loop
for (year in years) {
  pop_col  <- paste0(year, ".x")      # bv. "2015.x"
  unemp_col<- paste0(year, ".y")      # bv. "2015.y"
  perc_col <- paste0(year, "_percent")# bv. "2015_percent"
  
  # de berekening en toewijzing *in* de loop
  unemployment_percent_df[[perc_col]] <-
    merged_df[[unemp_col]] / merged_df[[pop_col]] * 100
}

# 3) Gemiddelde over alle jaar‑percentages
unemployment_percent_df$avg_unemp_percent <-
  round(
    rowMeans(
      unemployment_percent_df[ , grep("_percent$", names(unemployment_percent_df))],
      na.rm = TRUE
    ),
    1
  )


# //Event Analysis Plot//Plotting the GDP growth from 2013 to 2024-//////

ggplot(growth_df, aes(x = Year, y = TotalGDP)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue") +
  geom_vline(xintercept = 2020, color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = 2013:2024) +  # elk jaar tonen
  labs(
    title = "Cumulative EU GDP Growth (2013–2024)",
    subtitle = "Base year = 100, ",
    x = "Year",
    y = "Cumulative GDP Index"
  ) +
  theme_minimal()


#///Sub-population Plot///

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


# ///Temporal Variation Plot/EU Economic Development: GDP Index and Average Unemployment (2015–2024)///
# 📊 1. Maak je samengestelde GDP-index klaar (2015–2024)
gdp_growth <- GDP_df %>%
  select(all_of(as.character(2015:2024)))

avg_growth_per_year <- colMeans(gdp_growth, na.rm = TRUE) / 100

growth_df <- data.frame(
  Year = 2015:2024,
  GrowthRate = avg_growth_per_year
) %>%
  mutate(TotalGDP = cumprod(1 + GrowthRate) * 100)

# 📊 2. Maak gemiddelde werkloosheid klaar (2015–2024)
unemp_df <- Unemploymentlang %>%
  summarise(across(all_of(as.character(2015:2024)), ~mean(as.numeric(.), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Unemployment") %>%
  mutate(Year = as.integer(Year))

# 🔗 3. Combineer de twee datasets
combo_df <- left_join(growth_df, unemp_df, by = "Year")

# 🎨 4. Plot met dubbele y-as en mooie styling

ggplot(combo_df, aes(x = Year)) +
  # Blauwe GDP-lijn (links)
  geom_line(aes(y = TotalGDP), color = "#0072B2", size = 1.3) +
  geom_point(aes(y = TotalGDP), color = "#0072B2", size = 2) +
  
  # Oranje werkloosheidslijn (rechts, herschaald)
  geom_line(aes(y = Unemployment / 7.5), color = "#E69F00", size = 1.3) +
  geom_point(aes(y = Unemployment / 7.5), color = "#E69F00", size = 2) +
  
  # Y-assen
  scale_y_continuous(
    limits = c(0, 135),
    name = "Cumulative GDP Index (Base year = 100)",
    sec.axis = sec_axis(~.*7.5, name = "Average Unemployment (in thousands)", breaks = seq(0, 1000, 250))
  ) +
  
  # X-as per jaar tonen
  scale_x_continuous(breaks = 2015:2024) +
  
  # Titel & opmaak
  labs(
    title = "EU GDP Growth vs. Average Unemployment (2015–2024)",
    subtitle = "Left: GDP Index (blue), Right: Avg. Unemployment (orange)",
    x = "Year"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y.left = element_text(color = "#0072B2", face = "bold"),
    axis.title.y.right = element_text(color = "#E69F00", face = "bold"),
    axis.text.y.left = element_text(color = "#0072B2"),
    axis.text.y.right = element_text(color = "#E69F00")
  )


    #// European Unemployment Map (2015–2024) ////

               
  
  # Stap 1: Zorg dat de landnaamkolom goed heet
  colnames(unemployment_percent_df)[1] <- "Country"
  
  # Stap 2: Koppel je data aan de wereldkaart
  mapped_data_percent <- joinCountryData2Map(unemployment_percent_df,
                                             joinCode = "NAME",
                                             nameJoinColumn = "Country")
  
  # Stap 3: Maak de kaart op basis van procentuele werkloosheid
  mapCountryData(
    mapped_data_percent,
    nameColumnToPlot = "avg_unemp_percent",
    catMethod = "quantiles",
    numCats = 3,
    mapTitle = "Average Unemployment in Europe (2015–2024)",
    colourPalette = c("green", "yellow", "red"),
    mapRegion = "Europe",
    addLegend = FALSE  # hide default color bar
  )
  
  legend(
    "topleft",
    legend = c("Low", "Medium", "High"),
    fill = c("green", "yellow", "red"),
    title = "Unemployment level",
    cex = 0.8                     
               
  )
  
  

   
  
  
  