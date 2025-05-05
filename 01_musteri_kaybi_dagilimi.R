# -----------------------------------------------------------------------
# 1 - Müşteri kaybı  dağılımı sayı bazında
# -----------------------------------------------------------------------


ggplot(data, aes(x = as.factor(churn), fill = as.factor(churn))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    name = "Churn",
                    labels = c("Kalmış", "Gitmiş")) +
  labs(
    title = "Müşteri Kaybı (Churn) Dağılımı",
    x = "Churn (0 = Kalmış, 1 = Gitmiş)",
    y = "Kişi Sayısı"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------
# 2 - Müşteri kaybı dağılımı % lik bazında
# -----------------------------------------------------------------------


churn_dagilimi <- data %>%
  group_by(churn) %>%
  summarise(count = n()) %>%
  mutate(
    percentage = count / sum(count),
    label = paste0(round(percentage * 100, 1), "%")
  )

ggplot(churn_dagilimi, aes(x = "", y = percentage, fill = as.factor(churn))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("steelblue", "tomato"), 
                    name = "Churn", 
                    labels = c("Kalmış", "Gitmiş")) +
  labs(title = "Müşteri Kaybı (Churn) Dağılımı", x = NULL, y = NULL) +
  theme_void()
