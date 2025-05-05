# -----------------------------------------------------------------------
# Kredi Kartı Kullanımına Göre Müşteri Kaybı Oranı
# -----------------------------------------------------------------------

creditcard_churn <- data %>%
  group_by(credit_card) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1),
    .groups = 'drop'
  )

# Verinin gösterilmesi
View(creditcard_churn)

# Etiket düzenlemesi
creditcard_churn$card_status <- ifelse(creditcard_churn$credit_card == 1, "Kredi Kartı Var", "Kredi Kartı Yok")


ggplot(creditcard_churn, aes(x = card_status, y = churn_rate, fill = card_status)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Kredi Kartı Kullanımına Göre Churn Oranı",
       x = "Kredi Kartı Durumu",
       y = "Churn Oranı (%)") +
  scale_fill_manual(
    values = c("Kredi Kartı Var" = "#3F51B5", "Kredi Kartı Yok" = "#FF9800"),
    name = "Kart Durumu"
  ) +
  theme_minimal()
