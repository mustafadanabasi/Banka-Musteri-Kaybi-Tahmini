# -----------------------------------------------------------------------
# Üyelik durumuna (Aktif / Pasif) göre müşteri kaybı dağılımı
# -----------------------------------------------------------------------


active_churn <- data %>%
  group_by(active_member) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1),
    .groups = 'drop'
  )

# Aktif üye bilgisini etiket olarak belirleme
active_churn$active_status <- ifelse(active_churn$active_member == 1, "Aktif", "Pasif")

ggplot(active_churn, aes(x = active_status, y = churn_rate, fill = active_status)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Aktif Üyelik Durumuna Göre Churn Oranı",
       x = "Üyelik Durumu",
       y = "Churn Oranı (%)") +
  scale_fill_manual(values = c("Aktif" = "#4CAF50", "Pasif" = "#F44336")) +
  theme_linedraw()