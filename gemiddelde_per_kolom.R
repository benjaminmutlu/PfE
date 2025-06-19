Low_growth_df <- allmerged_df %>%
  filter(EconClass == "Low growth")

High_growth_df <- allmerged_df %>%
  filter(EconClass == "High growth")

Middle_growth_df <- allmerged_df %>%
  filter(EconClass == "Middle growth")

library(tidyverse)

# Functie om gemiddelde rij toe te voegen aan een subset
voeg_gemiddelde_toe <- function(df) {
  gemiddelde_rij <- df %>%
    select(-Country) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
    mutate(Country = "Gemiddelde") %>%
    select(Country, everything())
  
  bind_rows(df, gemiddelde_rij)
}

# Filter per groep en selecteer alleen relevante kolommen
low_growth_df <- allmerged_df %>%
  filter(EconClass == "Low growth") %>%
  select(Country, ends_with("_percent"))
low_growth_df <- voeg_gemiddelde_toe(low_growth_df)

middle_growth_df <- allmerged_df %>%
  filter(EconClass == "Middle growth") %>%
  select(Country, ends_with("_percent"))
middle_growth_df <- voeg_gemiddelde_toe(middle_growth_df)

high_growth_df <- allmerged_df %>%
  filter(EconClass == "High growth") %>%
  select(Country, ends_with("_percent"))
high_growth_df <- voeg_gemiddelde_toe(high_growth_df)





long_df <- allmerged_df %>%
  pivot_longer(
    cols = ends_with("_percent"),
    names_to = "year",
    values_to = "unemployment"
  ) %>%
  mutate(
    year = str_remove(year, "_percent"),
    year = as.integer(year)
  )


Boxplot_unemployment_to_economic_class <- ggplot(long_df, aes(x = factor(year), y = unemployment, fill = EconClass)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Unemployment by GDP growth class",
    x = "Year",
    y = "Unemployment Rate (%)",
    fill = "Economic Class"
  ) +
  theme_minimal()

print(Boxplot_unemployment_to_economic_class)




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


#boxplot avarage unemployment
Boxplot_unemp_avg <- ggplot(long_df, aes(x = EconClass, y = unemployment, fill = EconClass)) +
  geom_boxplot() +
  labs(
    title = "Unemployment by GDP growth class",
    x = "Economic Class",
    y = "Unemployment Rate (%)",
    fill = "Economic Class"
  ) +
  theme_minimal()


print(Boxplot_unemp_avg)
