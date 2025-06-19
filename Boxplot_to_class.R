GDP_df <- GDP_df %>%
  rowwise() %>%
  mutate(
    GDP_growth_total = round((prod(1 + c_across(`2015`:`2024`) / 100, na.rm = TRUE) - 1) * 100, 2)
  ) %>%
  ungroup()

colnames(GDP_df)[1]             <- "Country"
colnames(Unemploymentlang)[1]   <- "Country"


newx_df <- inner_join(
  inner_join(GDP_df, Unemploymentlang, by = "Country"),
  unemployment_percent_df,
  by = "Country" 
)


breaks27 <- quantile(
  newx_df$GDP_growth_total,
  probs = c(0, 1/3, 2/3, 1),
  na.rm = TRUE
)

newxx_df <- newx_df %>%
  mutate(
    EconClass = cut(
      GDP_growth_total,
      breaks = breaks27,
      labels = c("Low growth", "Middle growth", "High growth"),
      include.lowest = TRUE
    )
  )


allmerged_df <- newxx_df %>%
  select(1,29,30,31,32,33,34,35,36,37,38,40)


Boxplot_unemp_2024 <- ggplot(newxx_df, aes(x = EconClass, y = `2024_percent`, fill = EconClass)) +
  geom_boxplot() +
  labs(
    title = "Unemployment Rate in 2024 by Economic Class",
    x = "Economic Class",
    y = "Unemployment Rate (%) in 2024",
    fill = "Economic Class"
  ) +
  theme_minimal()

print(Boxplot_unemp_2024)






