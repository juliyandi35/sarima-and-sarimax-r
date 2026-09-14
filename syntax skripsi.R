#fungsi2 dalam analisis times series,misal untuk uji ADF
#install.packages("tseries") 
library(tseries)
#untuk peramalan
#install.packages("forecast") 
library(forecast)
#analisis time series
#install.packages("TSA") 
library(TSA)
#untuk membantu uji signifikansi parameter model deret waktu 
#install.packages("lmtest") 
library(lmtest)
#estimasi parameter model MLE
#install.packages("astsa")
library(astsa) 


#memanggil data
library(readxl)
data_curah_hujan <- read_excel("data curah hujan.xlsx")
View(data_curah_hujan)
head(data_curah_hujan)
tail(data_curah_hujan)

#plot data time series
Tahun <- data_curah_hujan$Tahun
Bulan <- data_curah_hujan$Bulan
Curah_hujan <- data_curah_hujan$`Curah Hujan`
Tekanan_udara <- data_curah_hujan$`Tekanan Udara`

curah.ts = ts(Curah_hujan, start= c(2018,1), frequency = 12) #1=dimulai dr bln jan
autoplot(curah.ts, main = "Plot Data Curah Hujan")
tekanan.ts = ts(Tekanan_udara, start = c(2018,1), frequency = 12)
autoplot(tekanan.ts, main = "Plot Data Tekanan Udara")

#ADF dari data curah hujan
adf.test(curah.ts)

#plot acf dan pacf 
par(mfrow =c(1,2))#partisi 1 baris 2 kolom
acf(Curah_hujan, lag.max = 24, main="plot ACF Curah Hujan")
pacf(Curah_hujan, lag.max = 24, main="plot PACF Curah Hujan")

#uji signifikansi parameter
printstasarima <- function(x, digits =4, se=TRUE){
  if (length(x$coef) > 0){
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits) 
    if (se && nrow(x$var.coef))
    ses <- rep(0, length(coef))
    ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
    coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
    coef <- rbind(coef, s.e.= ses)
    statt <- coef[1,]/ses
    pval <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
    coef <- rbind(coef, t=round(statt, digits=digits),sign.=round(pval, digits=digits))
    coef <- t(coef)}
    print.default(coef, print.gap= 2)}


#Estimasi Model SARIMA
model1 = Arima(curah.ts, order = c(4,1,4), seasonal = list(order = c(0,1,1), period=12), 
               include.mean = FALSE)
checkresiduals(model1)
summary(model1) 
printstasarima(model1) #melihat signifikansi koefisien

model2 = Arima(curah.ts, order = c(4,1,0), seasonal = list(order = c(0,1,1), period=12), 
               include.mean = FALSE)
checkresiduals(model2)
summary(model2) #melihat signifikansi koefisien
printstasarima(model2) #melihat signifikansi koefisien

model3 = Arima(curah.ts, order = c(0,1,4), seasonal = list(order = c(0,1,1), period=12), 
               include.mean = FALSE)
checkresiduals(model3)
summary(model3) 
printstasarima(model3) #melihat signifikansi koefisien

model4 = Arima(curah.ts, order = c(3,1,0), seasonal = list(order = c(0,1,1), period=12), 
               include.mean = FALSE)
checkresiduals(model4)
summary(model4) #melihat signifikansi koefisien
printstasarima(model4) #melihat signifikansi koefisien

model5 = Arima(curah.ts, order = c(2,1,0), seasonal = list(order = c(0,1,1), period=12), 
               include.mean = FALSE)
checkresiduals(model5)
summary(model5) 
printstasarima(model5) #melihat signifikansi koefisien

## Jika melihat nilai RMSE dan MAPE terkecil, maka dipilih model 1 
# dengan MAPE terkecil sebesar 261.8058 dan RMSE terkecil sebesar 53.3395


#Uji Asumsi White Noise
Box.test(model1$residuals, type = "Ljung-Box")
Box.test(model2$residuals, type = "Ljung-Box")
Box.test(model3$residuals, type = "Ljung-Box")
Box.test(model4$residuals, type = "Ljung-Box")
Box.test(model5$residuals, type = "Ljung-Box")

#Uji Asumsi Normalitas
library(dgof)
ks.test(curah.ts, model1$residuals)
ks.test(curah.ts, model2$residuals)
ks.test(curah.ts, model3$residuals)
ks.test(curah.ts, model4$residuals)
ks.test(curah.ts, model5$residuals)

#prediksi
prediksi_sarima <- predict(model1,n.ahead = 5) 
prediksi_sarima

#Estimasi Model SARIMAX
sarimax= Arima(curah.ts, order = c(4,1,4), seasonal = list(order = c(0,1,1), period=12) ,xreg = tekanan.ts)
checkresiduals(sarimax)
summary(sarimax)
printstasarima(sarimax)

#Uji Asumsi White Noise 
Box.test(sarimax$residuals, type = "Ljung-Box")
    
#Uji Asumsi Normalitas
library(dgof)
ks.test(curah.ts, sarimax$residuals)

#prediksi
# Kita misalkan variabel tekan sebagai tekanan udara 5 bulan ke depan sebagai xreg yang baru.
new_tekan <- c(924.5,924.3,924.1,923.9,923.5) 
prediksi_sarimax <- predict(sarimax, newxreg = new_tekan,n.ahead = 5)
prediksi_sarimax
