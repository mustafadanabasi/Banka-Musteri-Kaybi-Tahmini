# -----------------------------------------------------------------------
# Müşteri kaybı tahmini (Lojistik regresyon modeli)
# -----------------------------------------------------------------------

# -----------------------------
# 1. Gerekli kütüphaneler
# -----------------------------
library(caret)   # Veri ön işleme (özellikle dummy değişken oluşturma) için kullanılır
library(dplyr)   # Veri çerçevesi işlemleri için kullanılır (select, mutate, vb.)

# -----------------------------
# 2. Cinsiyet bilgisi sayısallaştırılıyor
# -----------------------------
# "Male" ise 1, değilse (örneğin "Female") 0 atanır
data$gender <- ifelse(data$gender == "Male", 1, 0)

# -----------------------------
# 3. Ülke bilgisi sayısal koda çevriliyor
# -----------------------------
# France = 0, Germany = 1, Spain = 2 olacak şekilde kodlanır
data$country <- recode(data$country,
                       "France" = 0,
                       "Germany" = 1,
                       "Spain" = 2)

# -----------------------------
# 4. churn sütunu hariç tüm değişkenler dummy değişkenlere dönüştürülüyor
# -----------------------------
# dummyVars, kategorik değişkenleri modelin anlayabileceği sayısal forma getirir
dmy <- dummyVars(~ ., data = data %>% select(-churn))

# Veri dönüştürülüyor (dummy değişkenlere çevriliyor)
data_transformed <- predict(dmy, newdata = data) %>% as.data.frame()

# churn sütunu geri ekleniyor (bu sütun hedef değişkenimiz)
data_transformed$churn <- data$churn

# -----------------------------
# 5. Lojistik regresyon modeli eğitiliyor
# -----------------------------
# churn (bağımlı değişken) diğer tüm değişkenlerle modellenir
model <- glm(churn ~ ., data = data_transformed, family = "binomial")

# -----------------------------
# 6. Yeni müşteri verisi tanımlanıyor
# -----------------------------
yeni_musteri <- data.frame(
  customer_id = 5,
  credit_score = 690,
  country = 1,        # Germany'nin sayısal kodu
  gender = 1,         # Male = 1
  age = 45,
  tenure = 6,
  balance = 50000,
  products_number = 2,
  credit_card = 0,
  active_member = 1,
  estimated_salary = 55000
)

# -----------------------------
# 7. Yeni müşteri verisine dummy dönüşüm uygulanıyor
# -----------------------------
# Modelle uyumlu hale getirmek için yeni müşteri verisi de aynı şekilde dönüştürülür
yeni_musteri_transformed <- predict(dmy, newdata = yeni_musteri) %>% as.data.frame()

# -----------------------------
# 8. Tahmin yapılır
# -----------------------------
# Model yeni müşteri için churn (ayrılma) olasılığını hesaplar
churn_olasilik <- predict(model, newdata = yeni_musteri_transformed, type = "response")

# Eğer olasılık 0.5'ten büyükse churn = 1 (yani müşteri gidebilir), değilse 0
churn_tahmini <- ifelse(churn_olasilik > 0.5, 1, 0)

# -----------------------------
# 9. Sonuçlar ekrana yazdırılır
# -----------------------------
cat("Tahmin:", churn_tahmini, "\n")
cat("Churn Olasılığı:", round(churn_olasilik * 100, 2), "%\n")
