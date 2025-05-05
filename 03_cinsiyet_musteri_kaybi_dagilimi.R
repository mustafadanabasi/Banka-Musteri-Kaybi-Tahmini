# -----------------------------------------------------------------------
# 1 -  Cinsiyet Bazında müşteri kaybı dağılımı
# -----------------------------------------------------------------------

gender_churn <- data %>%
  group_by(gender) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1)
  )

ggplot(gender_churn, aes(x = gender, y = churn_rate, fill = gender)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Gender'a Göre Churn Oranı",
       x = "Cinsiyet",
       y = "Churn Oranı (%)") +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#6495ED")) +
  theme_minimal()


# -----------------------------------------------------------------------
# 2 - Ülke ve cinsiyet bazında müşteri kaybı dağılımı
# -----------------------------------------------------------------------

country_gender_churn <- data %>%
  group_by(country, gender) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1),
    .groups = 'drop'
  )


ggplot(country_gender_churn, aes(x = country, y = churn_rate, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(churn_rate, "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.3, size = 4) +
  labs(title = "Ülke ve Cinsiyete Göre Churn Oranı",
       x = "Ülke",
       y = "Churn Oranı (%)",
       fill = "Cinsiyet") +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#6495ED")) +
  theme_minimal()