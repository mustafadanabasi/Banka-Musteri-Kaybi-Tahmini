# -----------------------------------------------------------------------
# Bankadaki hesap bakiyesi gruplarına Göre Churn Oranı
# -----------------------------------------------------------------------

data$balance_group <- cut(data$balance,
                          breaks = c(-0.1, 0, 50000, 100000, 150000, Inf),
                          labels = c("0 TL", "0-50K", "50K-100K", "100K-150K", "150K+"),
                          right = TRUE)

balance_churn <- data %>%
  group_by(balance_group) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1),
    .groups = 'drop'
  )

ggplot(balance_churn, aes(x = balance_group, y = churn_rate, fill = balance_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Balance Gruplarına Göre Churn Oranı",
       x = "Hesap Bakiyesi",
       y = "Churn Oranı (%)") +
  scale_fill_brewer(palette = "YlGnBu",name="Hesap Bakiyesi") +
  theme_minimal()
