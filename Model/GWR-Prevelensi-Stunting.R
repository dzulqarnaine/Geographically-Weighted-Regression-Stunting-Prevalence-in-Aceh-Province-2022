library(spdep)
library(skedastic)
library(sf)
library(mapview)
library(GWmodel)
library(corrplot)
library(spgwr)
library(spatialreg)
library(gwrr)
library(readxl)
library(nortest)
library(car)
library(lmtest)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(easyGgplot2)



path = "C:/KULIAH STATISTIKA UNIVERSITAS SYIAH KUALA/SEMESTER 6/MBKM DISKOMINSA/Artikel/Stunting2022.xlsx"
data = read_xlsx(path)
data



Y = data$`Prevelensi Stanting`
X1 = data$`IMD`
X2 = data$`JumlahTenaga Gizi Rumah Sakit`
X3 = data$`Jumlah Ibu Hamil Mengonsumsi Tablet Tambah Darah (TTD) (Jiwa)`
X4 = data$`Jumlah Posyandu`
X5 = data$`jumlah tenaga bidan`
X6 = data$`Persentase Penduduk Miskin`
X7 = data$`Tenaga kesehatan Masyarakat`
lon = data$`X`
lat = data$`Y`



# data$Y = log(data$`Prevelensi Stanting` + 1)
data$X1_log = log(data$`IMD` + 1)
data$X2_log = log(data$`JumlahTenaga Gizi Rumah Sakit` + 1)
data$X3_log = log(data$`Jumlah Ibu Hamil Mengonsumsi Tablet Tambah Darah (TTD) (Jiwa)` + 1)
data$X4_log = log(data$`Jumlah Posyandu` + 1)
data$X5_log = log(data$`jumlah tenaga bidan` + 1)
data$X6_log = log(data$`Persentase Penduduk Miskin` + 1)
data$X7_log = log(data$`Tenaga kesehatan Masyarakat` + 1)



model_linier = lm(Y ~ X1_log +  X2_log + X3_log + X4_log + X5_log + X6_log + X7_log , data = data)
summary(model_linier)



### Uji Normalitas
# Heterogenitas
bptest(model_linier) 

# Normalitas
ks_test = ks.test(residuals(model_linier), "pnorm", mean(residuals(model_linier)), sd(residuals(model_linier)))
cat("Kolmogorov-Smirnov Test p-value =", ks_test$p.value, "\n")

# AutoKorelasi
dwt(model_linier)

#Mutikolinearitas
vif(model_linier)


#### KONVERSI DATA DALAM BENTUK SPASIAL
df = as_tibble(data)
df_spasial = st_as_sf(df, coords = c("X", "Y"), crs = 4326)
df_spasial_sp = as(df_spasial, "Spatial")
df_spasial_sp
df_spasial_sp$y = data$`Prevelensi Stanting`

df_spasial_sp$X1_log = data$X1_log
df_spasial_sp$X2_log = data$X2_log
df_spasial_sp$X3_log = data$X3_log
df_spasial_sp$X4_log = data$X4_log
df_spasial_sp$X5_log = data$X5_log
df_spasial_sp$X6_log = data$X6_log
df_spasial_sp$X7_log = data$X7_log

coords = coordinates(df_spasial_sp)
bobot = nb2listw(knn2nb(knearneigh(coords, k = 5)))



#### MORAN'S TEST
moran.test(df_spasial_sp$y, bobot, alternative = "greater")



#### JARAK EUCLIDIAN
euclidean = gw.dist(coords)
head(euclidean)



#### MENENTUKAN BANDWIDTH OPTIMAL
gwr_band = gwr.sel( Y ~ X1_log +  X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, df_spasial_sp, gweight = gwr.bisquare)
gwr_band

# gwr_band_bisquare = gwr.sel( Y ~ X1_log + X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, df_spasial_sp, gweight = gwr.bisquare)
# gwr_band_bisquare



#### Matriks Pembobot Lokasi
bobot = knn2nb(knearneigh(coords,k=4))
matriks_bobot = nb2mat(bobot, zero.policy=TRUE)
rownames(matriks_bobot) = df$Kabupaten_Kota
colnames(matriks_bobot) = df$Kabupaten_Kota



#### MODEL GWR
gwr.fit = gwr(Y ~ X1_log + X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, data = df_spasial_sp, bandwidth = gwr_band, gweight = gwr.bisquare, hatmatrix = TRUE, se.fit = TRUE)
gwr.fit

#gwr.fit = gwr(Y ~ X1_log + X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, data = df_spasial_sp, bandwidth = gwr_band, gweight = gwr.bisquare, hatmatrix = TRUE, se.fit = TRUE)
#gwr.fit



#### EVALUASI MODEL GWR
R2_OLS = summary(model_linier)$r.squared 
R2_GWR = 0.7395151 
AIC_OLS = AIC(model_linier) 
AIC_GWR = gwr.fit$results$AICh 
evaluasi = data.frame(model = c("OLS", "GWR"), R2 = c(R2_OLS, R2_GWR), AIC = c(AIC_OLS, AIC_GWR))
print(evaluasi)



#### OUTPUT MODEL GWR
df_gwr = as.data.frame(gwr.fit$SDF)
rownames(df_gwr) = df_spasial$Kabupaten_Kota
df_gwr
View(df_gwr)



#### MENENTUKAN VARIABEL BERPENGARUH BERDASARKAN LOKASI
df_gwr$t_X1_log = df_gwr$X1_log / df_gwr$X1_log_se
df_gwr$t_X2_log = df_gwr$X2_log / df_gwr$X2_log_se
df_gwr$t_X3_log = df_gwr$X3_log / df_gwr$X3_log_se
df_gwr$t_X4_log = df_gwr$X4_log / df_gwr$X4_log_se
df_gwr$t_X5_log = df_gwr$X5_log / df_gwr$X5_log_se
df_gwr$t_X6_log = df_gwr$X6_log / df_gwr$X6_log_se
df_gwr$t_X7_log = df_gwr$X7_log / df_gwr$X7_log_se


df_gwr[, c("t_X1_log", "t_X2_log", "t_X3_log", "t_X4_log", "t_X5_log", "t_X6_log","t_X7_log")]
df_gwr
View(df_gwr)


# Prediksi model GWR dalam skala log
prediksi_log = gwr.fit$SDF$pred  # Hasil prediksi dalam skala log

# Mengembalikan prediksi ke skala asli dengan eksponensial
prediksi_asli = exp(prediksi_log)

# Tampilkan hasil prediksi dalam skala asli
print(prediksi_asli)


# write.csv(df_gwr, "C:/KULIAH STATISTIKA UNIVERSITAS SYIAH KUALA/SEMESTER 6/Artikel/Data Hasil Pembahasan/hasil2.csv", row.names = TRUE)
# names(df_gwr)



  
  
  
  
  
  
  