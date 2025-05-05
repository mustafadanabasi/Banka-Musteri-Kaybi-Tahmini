# -----------------------------------------------------------------------
# Kullanılan kütüphaneler
# -----------------------------------------------------------------------

#install.packages(c("tidyverse", "ggplot2",  "dplyr", "readr", "corrplot","leaflet", "caret"))

# Veri analizi ve görselleştirme için birçok paketi (ggplot2, dplyr, readr, vb.) bir arada sunar
library(tidyverse)

# Veri görselleştirme için bir araç (ör: barplot, scatter plot, histogram)
library(ggplot2)

# Veri çerçeveleri üzerinde filtreleme, gruplama, özetleme gibi işlemler sağlar
library(dplyr)

# CSV ve benzeri dosyaları hızlı ve güvenli şekilde okuma işlemleri için kullanılır
library(readr)

# Korelasyon matrisini renkli ve görsel olarak gösterir (ör: değişkenler arası ilişki analizi)
library(corrplot)

# İnteraktif haritalar oluşturmak için kullanılır (özellikle coğrafi veri analizinde)
library(leaflet)

# Makine öğrenmesi için model oluşturma, çapraz doğrulama, metrik hesaplama gibi işlemleri yapar
library(caret)

# -----------------------------------------------------------------------
# Veri inceleme
# -----------------------------------------------------------------------

# Veri Kaynağı
# https://www.kaggle.com/datasets/gauravtopre/bank-customer-churn-dataset

# Veriyi yükle
data <- read_csv("Bank Customer Churn Prediction.csv")


#İlk 5 satırı gösterme
head(data, 5)

# Veri seti hakkında bilgi .
str(data)

# Satır ve sütun sayısı -  10,000 row, 12 column
dim(data)

# Veri seti özet istatistikleri
summary(data) 

# Eksik veri kontrolü
colSums(is.na(data))


#customer_id: Müşteri ID
#credit_score: Kredi skoru
#country: Ülke
#gender: Cinsiyet
#age: Yaş
#tenure: Bankada kalma süresi (yıl)
#balance: Hesap bakiyesi
#products_number: Alınan ürün sayısı
#credit_card: Kredi kartı var mı? (0 = hayır, 1 = evet)
#active_member: Aktif üye mi? (0 = hayır, 1 = evet)
#estimated_salary: Tahmini maaş
#churn: Müşteri kaybı (1 = terk etmiş, 0 = hâlâ müşteri)

# korelasyon grafiği - Sadece sayısal değişkenleri seçiyoruz (korelasyon sadece sayısal verilerle yapılabilir)

#Cinsiyeti de sayısal değişkene çeviriyoruz. Male = 1, Female = 0
data$gender_code <- ifelse(data$gender == "Male", 1, 0)

# -----------------------------------------------------------------------
# Yaş histogramı - kaç müşterinin hangi yaş aralığında
# -----------------------------------------------------------------------
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(title = "Yaş Dağılımı", x = "Yaş", y = "Frekans")

# -----------------------------------------------------------------------
# Ortalama değerlere göre müşteri kaybı sayıları
# -----------------------------------------------------------------------

data %>%
  group_by(churn) %>%
  summarise(
    Ortalama_Yas = mean(age),
    Ortalama_Bakiye = mean(balance),
    Ortalama_KrediSkoru = mean(credit_score),
    Ortalama_Maas = mean(estimated_salary),
    Musteri_Sayisi = n()
  )

# -----------------------------------------------------------------------
# Korelasyon incelemesi
# -----------------------------------------------------------------------

# Hangi sayısal değişkenler birbiriyle ne kadar güçlü ilişkilidir? Pozitif mi, negatif mi?
numeric_data <- select(data, credit_score, age, tenure, balance, products_number, estimated_salary,gender_code, churn)
#Korelasyon matrisini hesaplıyoruz
cor_matrix <- cor(numeric_data)
print(cor_matrix)

# Korelasyon matrisinin sütun ve satır isimlerini Türkçeleştir
colnames(cor_matrix) <- c("Kredi Skoru", "Yaş", "Bankada Kalma Süresi", 
                          "Hesap Bakiyesi", "Ürün Sayısı", "Tahmini Maaş","Cinsiyet", "Müşteri Kaybı")
rownames(cor_matrix) <- colnames(cor_matrix)
print(cor_matrix)

# renk paleti (negatif - nötr - pozitif)
colors <- colorRampPalette(c("red", "yellow", "green"))(200)

# Korelasyon grafiği özel renklerle
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         col = colors,   
         tl.cex = 0.8,       
         addCoef.col = "black") 

#corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)




