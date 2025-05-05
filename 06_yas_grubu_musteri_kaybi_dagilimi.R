# -----------------------------------------------------------------------
# Yaş gruplarına göre müşteri kaybı dağılımı
# -----------------------------------------------------------------------


# Yaş grubu adında yeni bir sutun oluşturmak.
# cut(...) > Bu fonksiyon, sayısal bir değişkeni aralık (kategori) gruplarına böler.
#18 ≤ yaş < 30 > 
#30 ≤ yaş < 40
#40 ≤ yaş < 50
#50 ≤ yaş < 60
#60 ≤ yaş

data$age_group <- cut(data$age,
                      breaks = c(18, 30, 40, 50, 60, Inf),
                      labels = c("18-30", "31-40", "41-50", "51-60", "60+"),
                      right = FALSE)

View(data)
age_churn <- data %>%
  group_by(age_group) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1),
    .groups = 'drop'
  )

ggplot(age_churn, aes(x = age_group, y = churn_rate, fill = age_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.5, size = 5) +
  labs(title = "Yaş Gruplarına Göre Churn Oranı",
       x = "Yaş Grubu",
       y = "Churn Oranı (%)") +
  scale_fill_brewer(palette = "Set2",  name = "Yaş Grubu") +
  theme_minimal()