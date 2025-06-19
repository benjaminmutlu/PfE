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
