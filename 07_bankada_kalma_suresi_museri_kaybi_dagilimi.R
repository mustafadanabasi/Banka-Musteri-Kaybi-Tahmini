# -----------------------------------------------------------------------
# Bankada kalma süresi (yıl) gruplarına Göre Churn Oranı
# -----------------------------------------------------------------------


data$tenure_group <- cut(data$tenure,
                         breaks = c(-1, 2, 5, 8, Inf),
                         labels = c("0-2 Yıl", "3-5 Yıl", "6-8 Yıl", "9+ Yıl"),
                         right = TRUE)

View(data)

tenure_churn <- data %>%
  group_by(tenure_group) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1),
    .groups = 'drop'
  )


ggplot(tenure_churn, aes(x = tenure_group, y = churn_rate, fill = tenure_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Bankada kalma süresi (yıl) gruplarına göre churn oranı",
       x = "Bankada Geçen Süre (Yıl)",
       y = "Churn Oranı (%)") +
  scale_fill_brewer(palette = "Pastel1",name= "Kalma Süresi") +
  theme_minimal()