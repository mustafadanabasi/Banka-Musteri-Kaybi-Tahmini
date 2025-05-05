# -----------------------------------------------------------------------
# 1 -  Ülkeye göre müşteri kaybı oranı
# -----------------------------------------------------------------------

ulke_dagilimi_data <- data %>% 
  filter(!is.na(country), !is.na(churn))

ggplot(ulke_dagilimi_data, aes(x = country, fill = as.factor(churn))) +
  geom_bar(position = "fill") +
  labs(title = "Ülkeye Göre Churn Oranı", x = "Ülke", y = "Oran") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Churn", labels = c("Kalmış", "Gitmiş"))


# -----------------------------------------------------------------------
# 2 - Harita üzerinde ülkeye göre müşteri kaybı gösterimi
# -----------------------------------------------------------------------

# Ülke koordinatlarını oluştur
ulke_koordinatlari <- data.frame(
  country = c("France", "Germany", "Spain"),
  lat = c(46.603354, 51.165691, 40.463667),
  lng = c(1.888334, 10.451526, -3.74922)
)

ulke_ozet_dagilim_data <- ulke_dagilimi_data %>%
  group_by(country) %>%
  summarise(
    total_customers = n(),
    churned = sum(churn == 1),
    churn_rate = round(100 * churned / total_customers, 1)
  ) %>%
  left_join(ulke_koordinatlari, by = "country")

View(ulke_ozet_dagilim_data)

leaflet(ulke_ozet_dagilim_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lng, ~lat,
    radius = ~churn_rate / 2,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    label = ~paste0(
      country, ": ", 
      churned, " kişi churn etmiş (", 
      churn_rate, "%)"
    )
  ) %>%
  addLegend("bottomright", 
            colors = "red", 
            labels = "Churn Oranı (%)",
            title = "Ülke Bazlı Müşteri Kaybı")
