GDP_df <- GDP_df %>%
  rowwise() %>%
  mutate(
    GDP_growth_total = round((prod(1 + c_across(`2015`:`2024`) / 100, na.rm = TRUE) - 1) * 100, 2)
  ) %>%
  ungroup()




# NEW BOXPLOT 
#make groups 
breaks <- quantile(GDP_df$GDP_growth_total,
                   probs = c(0, 1/3, 2/3, 1),
                   na.rm = TRUE)

GDP_df <- GDP_df %>%
  mutate(
    EconClass = cut(
      GDP_growth_total,
      breaks = breaks,
      labels = c("Low growth", "Middle growth", "High growth"),
      include.lowest = TRUE
    )
  )

colnames(GDP_df)[1]             <- "Country"
colnames(Unemploymentlang)[1]   <- "Country"

combined_dfx <- inner_join(
  GDP_df,
  Unemploymentlang,
  by = "Country"
)

new_df <- combined_dfx %>%
  select(1, 16,17)




