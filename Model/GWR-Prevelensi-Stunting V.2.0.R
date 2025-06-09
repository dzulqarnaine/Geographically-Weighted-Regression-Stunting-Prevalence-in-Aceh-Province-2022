library(spgwr) # Mengaktifkan paket spgwr
library(sf) # Untuk manipulasi data spasial
library(spdep) # Untuk analisis spasial
library(spatialreg) # Untuk analisis spasial tambahan

# Membaca data yang telah disediakan
path <- "C:/KULIAH STATISTIKA UNIVERSITAS SYIAH KUALA/SEMESTER 6/MBKM DISKOMINSA/Artikel/Stunting2022.xlsx"
data <- read_xlsx(path)

# Mendefinisikan variabel-variabel yang relevan
Y1 <- data$`Prevelensi Stanting`
X1 <- data$`IMD`
X2 <- data$`JumlahTenaga Gizi Rumah Sakit`
X3 <- data$`Jumlah Ibu Hamil Mengonsumsi Tablet Tambah Darah (TTD) (Jiwa)`
X4 <- data$`Jumlah Posyandu`
X5 <- data$`jumlah tenaga bidan`
X6 <- data$`Persentase Penduduk Miskin`
X7 <- data$`Tenaga kesehatan Masyarakat`
#X8 <- data$`jumlah tenaga bidan`
lon <- data$`X`
lat <- data$`Y`

# Menggunakan log transformasi pada beberapa variabel
data$Y1 <- log(data$`Prevelensi Stanting` + 1)
data$X1_log <- log(data$`IMD` + 1)
data$X2_log <- log(data$`JumlahTenaga Gizi Rumah Sakit` + 1)
data$X3_log <- log(data$`Jumlah Ibu Hamil Mengonsumsi Tablet Tambah Darah (TTD) (Jiwa)` + 1)
data$X4_log <- log(data$`Jumlah Posyandu` + 1)
data$X5_log <- log(data$`jumlah tenaga bidan` + 1)
data$X6_log <- log(data$`Persentase Penduduk Miskin` + 1)
data$X7_log <- log(data$`Tenaga kesehatan Masyarakat` + 1)
#data$X8_log <- log(data$`jumlah tenaga bidan` + 1)

# Konversi data menjadi format spasial
df <- as_tibble(data)
df_spasial <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)
df_spasial_sp <- as(df_spasial, "Spatial")
df_spasial_sp$y <- data$`Prevelensi Stanting`

# Menambahkan variabel log ke dalam data spasial
df_spasial_sp$X1_log <- data$X1_log
df_spasial_sp$X2_log <- data$X2_log
df_spasial_sp$X3_log <- data$X3_log
df_spasial_sp$X4_log <- data$X4_log
df_spasial_sp$X5_log <- data$X5_log
df_spasial_sp$X6_log <- data$X6_log
df_spasial_sp$X7_log <- data$X7_log


model_linier <- lm(Y1 ~ X1_log +  X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, data = data)
summary(model_linier)
bptest(model_linier) 
ks_test <- ks.test(residuals(model_linier), "pnorm", mean(residuals(model_linier)), sd(residuals(model_linier)))
cat("Kolmogorov-Smirnov Test p-value =", ks_test$p.value, "\n")
dwt(model_linier)
vif(model_linier)
moran.test(data$Y, bobot, alternative = "greater")

# Mendefinisikan koordinat
coords <- coordinates(df_spasial_sp)

# Menyesuaikan dengan metode GWR seperti yang diinginkan
# Menjalankan model GWR menggunakan fungsi gwr.sel untuk memilih bandwidth
b <- gwr.sel(Y1 ~ X1_log + X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, 
             data = df_spasial_sp, adapt = TRUE, gweight = gwr.Gauss)


# Estimasi Parameter GWR
gwr1 <- gwr(Y1 ~ X1_log + X2_log + X3_log + X4_log + X5_log + X6_log + X7_log, 
            data = df_spasial_sp, adapt = b, hatmatrix = TRUE, gweight = gwr.Gauss)

# Menampilkan hasil estimasi
gwr1

# Melihat nama-nama elemen dalam hasil GWR
names(gwr1)

# Menampilkan hasil koefisien beta dari model GWR
gwr1$SDF$"(Intercept)"
gwr1$SDF$X1_log
gwr1$SDF$X2_log
gwr1$SDF$X3_log
gwr1$SDF$X4_log
gwr1$SDF$X5_log
gwr1$SDF$X6_log
gwr1$SDF$X7_log

# Uji Kecocokan Model
BFC02.gwr.test(gwr1)

# Uji Pengaruh Geografis terhadap setiap prediktor
LMZ.F3GWR.test(gwr1)

# Menampilkan nilai bandwidth
gwr1$bandwidth

# Menampilkan nilai koefisien dan nilai prediksi dari hasil GWR
gwr1$SDF[, 2:4]
gwr1$SDF[, c(2:4, 9, 11)]

names(gwr1$SDF)

View(data)

###############################################################################################################
###############################################################################################################
h <- gwr.sel(data$'Y1' ~ data$X1_log + data$X2_log + data$X3_log+ data$X4_log+ data$X5_log+ data$X6_log+ data$X7_log, 
             coords=cbind(data$X,data$Y),
             data=data, adapt=FALSE,gweight=gwr.Gauss)

gwr2 <- gwr(data$'Y1' ~ data$X1_log + data$X2_log + data$X3_log+ data$X4_log+ data$X5_log+ data$X6_log+ data$X7_log,
            coords=cbind(data$X,data$Y),bandwidth=h,
            data=data,hatmatrix=TRUE,gweight=gwr.bisquare)
gwr2
names(gwr2)
names(gwr2$SDF)

# Menampilkan nilai koefisien beta
gwr2$SDF$"(Intercept)"
gwr2$SDF$"data$X1_log"
gwr2$SDF$"data$X2_log"
gwr2$SDF$"data$X3_log"
gwr2$SDF$"data$X4_log"
gwr2$SDF$"data$X5_log"
gwr2$SDF$"data$X6_log"
gwr2$SDF$"data$X7_log"

# Uji Kecocokan Model
BFC02.gwr.test(gwr2)

# Uji Pengaruh Geografis terhadap setiap prediktor
LMZ.F3GWR.test(gwr2)

# Melihat nilai bandwidth
gwr2$bandwidth

# Menampilkan Nilai koefisien dan nilai prediksi
gwr2$SDF[, c("(Intercept)", "data$X1_log", "data$X2_log", "data$X3_log")]  # Koefisien estimasi untuk (Intercept), X1_log, X2_log, X3_log
gwr2$SDF[, c("(Intercept)", "data$X1_log", "data$X2_log", "data$X3_log", "pred")]  # Menampilkan koefisien dan nilai prediksi

names(gwr2$SDF)

coefficients_df <- as.data.frame(gwr2$SDF[, c("(Intercept)", "data$X1_log", "data$X2_log", "data$X3_log")])
coefficients_df


# Menghitung nilai t dan p untuk variabel tertentu (misalnya X1_log)
gwr2$SDF$t_X1_log <- gwr2$SDF$"data$X1_log" / gwr2$SDF$"data$X1_log_se"
gwr2$SDF$p_X1_log <- 2 * pt(abs(gwr2$SDF$t_X1_log), df = nrow(gwr2$SDF) - 1, lower.tail = FALSE)

# Lakukan hal yang sama untuk variabel lainnya (misalnya X2_log, X3_log, dst.)
gwr2$SDF$t_X2_log <- gwr2$SDF$"data$X2_log" / gwr2$SDF$"data$X2_log_se"
gwr2$SDF$p_X2_log <- 2 * pt(abs(gwr2$SDF$t_X2_log), df = nrow(gwr2$SDF) - 1, lower.tail = FALSE)

# Menampilkan kabupaten/kota yang memiliki X1_log signifikan
significant_X1_log <- gwr2$SDF[gwr2$SDF$p_X1_log < 0.05, ]
print(significant_X1_log)

# Lakukan hal yang sama untuk X2_log, X3_log, dst.
significant_X2_log <- gwr2$SDF[gwr2$SDF$p_X2_log < 0.05, ]
print(significant_X2_log)

# Membuat tabel dengan koefisien dan nilai p
significance_table <- data.frame(
  Kabupaten_Kota = gwr2$SDF$region_id,  # Misalnya, jika ada kolom 'region_id' untuk kabupaten/kota
  X1_log_coef = gwr2$SDF$"data$X1_log",
  X1_log_p = gwr2$SDF$p_X1_log,
  X2_log_coef = gwr2$SDF$"data$X2_log",
  X2_log_p = gwr2$SDF$p_X2_log,
  # Tambahkan kolom lainnya untuk X3_log, X4_log, dst.
)
print(significance_table)
