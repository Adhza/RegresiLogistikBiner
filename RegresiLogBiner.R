# memanggil data
data = read.csv(file.choose(), header = T, ";")
data

# eksplorasi data menjadi data frame
str(data)

# variabel Y : penderita hipertensi                    
# kategorik : 1. Tidak hipertensi     0
#             2. Hipertensi           1

# variabel X1 : Umur
# kategorik : 1. 10-18    0    3. 31-50    2
#             2. 19-30    1    4. > 50     3

# variabel X8 : IMT (Indeks Massa Tubuh)
# kategorik : 1. Kurus    0    3. Berlebih   2        
#             2. Normal   1    4. Obesitas   3

# variabel X13 : Frekuensi Makanan Dibakar
# kategorik : 1. 1 kali/bulan      0     3. 1-2 kali/minggu    2    5. 1 kali/hari   4     
#             2. 3-6 kali/minggu   1     4. < 3 kali/bulan     3

# variabel X15 : Frekuensi Makanan Berlemak
# kategorik : 1. 1 kali/bulan      0     3. 3-6 kali/minggu   2    5. > 3 kali/bulan   4     
#             2. 1 kali/hari       1     4. 1-2 kali/bulan    3    6. > 1 kali hari    5


# factor karena data kategorik
data$Penderita.Hipertensi= as.factor(data$Penderita.Hipertensi)
data$Umur= as.factor(data$Umur)
data$Indeks.Masa.tubuh= as.factor(data$Indeks.Masa.tubuh)
data$Frekuensi.Makanan.Dibakar= as.factor(data$Frekuensi.Makanan.Dibakar)
data$Frekuensi.Makanan.berlemak= as.factor(data$Frekuensi.Makanan.berlemak)

str(data)

# melihat kelas pada tabel yang akan diprediksi

table(data$Penderita.Hipertensi)
table(data$Umur)
table(data$Indeks.Masa.tubuh)
table(data$Frekuensi.Makanan.Dibakar)
table(data$Frekuensi.Makanan.berlemak)


# regresi logistik
model.logistik<-glm(Penderita.Hipertensi~.,data=data, family="binomial")
model.logistik

summary(model.logistik)

zvalues =  summary(model.logistik)$coefficients
zvalues
coefs = coef(model.logistik)
coefs
pt(abs(zvalues), 50-4,lower.tail = FALSE)*2
(ctabel<- cbind(coefs,"p value" = p[,1]))


# uji individu
model.logistik1= glm(Penderita.Hipertensi~data[,1],data, family = "binomial")
model.logistik2= glm(Penderita.Hipertensi~data[,2],data, family = "binomial")
model.logistik3= glm(Penderita.Hipertensi~data[,3],data, family = "binomial")
model.logistik4= glm(Penderita.Hipertensi~data[,4],data, family = "binomial")

# odd rasio
coefs = coef(model.logistik)
coefs
exp(coefs)



#prediksi = round(predict(model.logistik,type = "response"))
#factor(prediksi)
#table(prediksi,data$Penderita.Hipertensi)

prob.prediksi<-predict(model.logistik, data, type="response")
prediksi<-ifelse(prob.prediksi<0.5,"0","1")
prediksi
pred.aktual<-data.frame("Prediksi"=prediksi,"Aktual"=data$Penderita.Hipertensi)

pred.aktual
# Akurasi
library(caret)
confusionMatrix(factor(prediksi),data$Penderita.Hipertensi)
