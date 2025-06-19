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


# Creating a new variable in the Unemploymentlang dataset
Unemploymentlang$avg_women <- unemp_women$avg_women
Unemploymentlang$avg_men <- unemp_men$avg_men

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