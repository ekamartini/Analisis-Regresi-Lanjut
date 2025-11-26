

#================================SETTING WORKING DIRECTORY================================
## Mengeset working directory
setwd("C:/Users/ekama/OneDrive/Documents/2022 - MATEMATIKA/SEMESTER 7/2. ANALISIS REGRESI LANJUT")
## Melihat direktori kerja saat ini
getwd()
## Melihat objek apa saja yang sudah ada pada direktori kerja
ls()
## Menghapus semua objek yang tidak diperlukan
rm(list=ls(all=TRUE))

#============================================SOAL NOMOR 1===============================================
## Memasukkan data
tbc = read.csv("dataarl.csv",header=T)
tbc
## Perintah head dapat digunakan untuk memberikan nama header data
head(tbc)
tail(tbc)

## 1(b) Membuat plot pasangan (pairs plot)
tbc.edit = cbind(tbc$cnr,tbc$kepadatan,tbc$miskin,tbc$sanitasi)
colnames(tbc.edit) = c("CNR","kepadatan","miskin","sanitasi")
pairs(tbc.edit)

#1(c) Melakukan Regresi Linear Berganda. Objek dengan nama tbc.lm
tbc.lm <- lm(cnr ~ kepadatan + miskin + sanitasi , data = tbc)
summary(tbc.lm) 

## 1(d) Melakukan seleksi Model
library(MASS)
## Seleksi maju
step.tbc.forward = stepAIC(tbc.lm,direction="forward")
step.tbc.forward$anova

## Seleksi mundur
step.tbc.backward = stepAIC(tbc.lm,direction="backward")
step.tbc.backward$anova

## Seleksi dua arah
step.tbc = stepAIC(tbc.lm,direction="both") # seleksi stepwise dua arah
step.tbc$anova

## Regresi subhimpunan
library(leaps)
#install.packages("leaps")
subset.tbc = regsubsets(cnr~kepadatan+miskin+sanitasi,data=tbc)
summary.tbc = summary(subset.tbc)
summary.tbc
summary.tbc$which # matriks logika
summary.tbc$rsq # R^2 masing-masing model
summary.tbc$rss # Jumlah kuadrat sisaan
summary.tbc$cp #Cp Mallows
summary.tbc$adjr2 # R^2 yang disesuaikan
summary.tbc$bic # BIC
summary.tbc$outmat

## Model terpilih (tanpa variabel miskin)
tbc.lm.new = lm(cnr~kepadatan+sanitasi,data=tbc)
summary(tbc.lm.new)

## 1(e) Model diagnostik
plot(tbc.lm.new)

## Plot suaian versus sisaan
fit.tbc = fitted(tbc.lm.new) # menghitungg fitted value (Y topi)
res.tbc = residuals(tbc.lm.new) # menghitung sisaan
plot(fit.tbc,res.tbc,xlab="Suaian",ylab="Sisaan",col="blue",
     ylim=c(-10,10))
abline(h=0,col="red")

## Uji kenormalan sisaan (res.savings)
shapiro.test(res.tbc)
qqnorm(res.tbc)  
qqline(res.tbc)

#============================================SOAL NOMOR 2=================================================
### Membaca data
L9 <- read.table("L9.txt",header=T)
L9
t <- L9$t
Y <- L9$Y

#2(a) plot pencar antara t dan Y
plot(t,Y,col="blue")

#2 (b) Melakukan estimasi parameter
#Melakukan Transformasi Data
#Dipilh alpha = 20 (yang lebih besar dari Y)
alpha = 10000
Y.star = log (1-Y/teta1)
reg.L9 = lm(Y.star~t-1) #t-1 dikarenakan tidak ada intersep
summary (reg.L9)

teta2 = -(-0.7130)


#Melakukan regresi non linear
nlreg.L9 <- nls(Y ~ teta1 *(1 - exp(-teta2 * t)),start = list(teta1 = 10000, teta2 = teta2))
summary (nlreg.L9)


#varians galat
summary(nlreg.L9)$sigma^2 #untuk model regresi linear
summary(reg.L9)$sigma^2 #untuk model regresi non linear

#2 (c) Plot Kurva Regresi
plot (t,Y)
x = t
curve(1.053e+04 *(1 - exp(-5.694e-01 * x)),x,type="l",add=T,col="red")

#2 (d) Selang kepercayaan 95%
confint(nlreg.L9)

# 2(e) bootstrap dengan replikasi sebanyak 5000
library(nlstools)
nlreg.L9 <- nls(Y ~ teta1 * (1 - exp(-teta2 * t)),data = L9, start = list(teta1 = 10000, teta2 = 0.8240))
boot.L9 <- nlsBoot(nlreg.L9,niter=5000)
summary(boot.L9)

#=======================================================SOAL NOMOR 3===========================================================
### Membaca data
orange.tree <- read.table("orangetree.txt",header=T)
orange.tree
t <- orange.tree$t
w <- orange.tree$w1
#data frame yang berisi t dan w1
data.orangetree <- data.frame(t, w)
data.orangetree

#3(a) Plot pencar Antara t dan w1
plot(t,w,col="Magenta")

#3(b) melakukan estimasi parameter 

#Untuk model 3(d) atau model yang dipilih (model gompetz)
#Melakukan Transformasi Data

#Dipilh alpha = 200
alpa = 200
w.star = log(log(alpa/w))
reg.gompetz= lm(w.star~t)
summary (reg.gompetz)

beta0 = exp(7.479e-01) 
k0 = -( -1.245e-03) 

#Untuk model 3(b) atau model sesuai NIM (Model Autokalitik)
#Melakukan Transformasi Data
#Dipilh alpha = 300
alpha = 300
Y.star = log ((alpha/w)-1)
reg.autokalitik= lm(Y.star~t)
summary (reg.autokalitik)

beta.0 = exp(2.1236691)
k.0 = -(-0.0014392) 

#Melakukan regresi non linear
#Untuk Model Gompetz
nlreg.gompetz <- nls(w ~ alpa*exp(-beta * exp(-k * t)),start = list(alpa = alpa, beta = beta0, k = k0))
summary(nlreg.gompetz)
#Untuk Model autokalitik
nlreg.autokalitik <- nls (w~alpha/(1+beta*exp(-k*t)),start = list(alpha = alpha, beta = beta.0, k = k.0))
summary(nlreg.autokalitik)

#varians galat
summary(nlreg.gompetz)$sigma^2 #Untuk model gompetz
summary(nlreg.autokalitik)$sigma^2 #Untuk Model autokalitik

#3 (c) plot pencar untuk model sesuai NIM (Model Autokalitik)
#Plot Kurva Regresi
plot (t,w)
x = t
curve(1.542e+02 / (1 + 5.640e+00 * exp(-2.758e-03 * x)),x,type="l",add=T,col="red")

#3 (d) Selang kepercayaan 95%
library(nlstools)
confint2(nlreg.gompetz)
confint2(nlreg.autokalitik)

#3 (e) bootstrap dengan replikasi sebanyak 5000
nlreg.gompetz <- nls(w ~ alpa * exp(-beta * exp(-k * t)),data = data.orangetree,start = list(alpa = alpa, beta = beta0, k = k0))
nlreg.autokalitik <- nls(w ~ alpha / (1 + beta * exp(-k * t)),data = data.orangetree, start = list(alpha = alpha, beta = beta.0, k = k.0))
library(nlstools)
boot.autokalitik <- nlsBoot(nlreg.autokalitik,niter=5000)
summary(boot.autokalitik)
boot.gompetz <- nlsBoot(nlreg.gompetz,niter=5000)
summary(boot.gompetz)

#==============================================SOAL NOMOR 4==============================
#Membaca Data
kumbang <- read.table("kumbang.dat",header=T)
kumbang
X <- kumbang$x
Y <- kumbang$y

#4(a)  plot prediktor versus respons.
par(mfrow=c(1,2))
plot(X,Y,xlab="Karbon Disulfida X",ylab="Keberadaan Kumbang")


# 4(b) melakukan regresi logistik
kumbang.glm <- glm(Y~X,family=binomial,data=kumbang)
summary(kumbang.glm)

plot(X,Y,xlab="Karbon Disulfida X",ylab="Keberadaan Kumbang",main="Regresi Logistik")
x <- X
curve(1/(1+exp(60.740-34.286*x)),add=TRUE,col="blue")


data.kumbang <- c(x)
pred.kumbang <- predict(kumbang.glm)

#4(d) diagnostik model
# Residual ini didefinisikan sebagai e/(1 - h_i)^0.5 dengan h_i addalah 
# hat matriks
matriks.topi <- hatvalues(kumbang.glm)

# Kita akan mengakses residual Pearson (Pearson residual) katakanlah
# namanya PR.glm (PR = Pearson residual).
PR.kumbang.glm <- residuals(kumbang.glm,"pearson")
PR.kumbang.glm

## leverage
leverage <- matriks.topi
leverage
Std.kumbang.glm.02 <- rstandard(kumbang.glm,type="pearson")
Std.kumbang.glm.02

## residual deviance
devres.kumbang.glm <- residuals(kumbang.glm,"deviance")
devres.kumbang.glm

# Plot nilai suaian (fit values) versus sisaan
## residual deviance
devres.kumbang.glm <- residuals(kumbang.glm,"deviance")
devres.kumbang.glm
par(mfrow=c(1,2))
fit.kumbang.glm <- fitted.values(kumbang.glm)
plot(fit.kumbang.glm,Std.kumbang.glm.02,main="Suaian vs Sisaan Pearson terstandar")
plot(fit.kumbang.glm,devres.kumbang.glm,main="Suaian vs Sisaan Devians")


## Secara teoretis residual ini harusnya mendekati normal
qqnorm(Std.kumbang.glm.02)
qqplot(Std.kumbang.glm.02)
qqline(Std.kumbang.glm.02)

## Menurut Agresti nilai sisaan mutlak yang lebih dari 2 atau 3 
## memberi bukti lack of fit
Abs.std.kumbang.glm <- abs(Std.kumbang.glm.02)
Abs.std.kumbang.glm
Abs.std.kumbang.glm > 2
Abs.std.kumbang.glm > 3


## Plot kuantil-kuantil
par(mfrow=c(1,2))
qqnorm(devres.kumbang.glm)
qqline(devres.kumbang.glm)
qqnorm(Std.kumbang.glm.02)
qqline(Std.kumbang.glm.02)

## Plot fitted values versus deviance
fit.kumbang.glm <- fitted.values(kumbang.glm)
plot(fit.kumbang.glm,devres.kumbang.glm)

## Plot 

## delta beta
delta.beta <- (PR.kumbang.glm^2)*(matriks.topi)/(1-matriks.topi)^2
delta.beta
delta.beta > 4

## delta chi-square
delta.chisqr <- PR.kumbang.glm^2/(1-matriks.topi)
delta.chisqr
delta.chisqr > 4

## delta deviance
delta.deviance <- devres.kumbang.glm^2+ (PR.kumbang.glm^2*matriks.topi)/(1-matriks.topi)
delta.deviance
delta.deviance > 4

par(mfrow=c(1,3))
plot(fit.kumbang.glm,delta.chisqr,main="Suaian vs Delta Khi-kuadrat")
plot(fit.kumbang.glm,delta.deviance,main="Suaian vs Delta Deviance")
plot(fit.kumbang.glm,delta.beta,main="Suaian vs Delta Beta")

# plot leverage vs 
par(mfrow=c(1,3))
plot(leverage,delta.chisqr,main="Leverage vs Delta Khi-kuadrat")
plot(leverage,delta.deviance,main="Leverage vs Delta Deviance")
plot(leverage,delta.beta,main="Leverage vs Delta Beta")

## Kuantitas diagnostik
Diag.kumbang.glm <- cbind(X,Y,fit.kumbang.glm,PR.kumbang.glm,Std.kumbang.glm.02,devres.kumbang.glm,delta.chisqr,delta.deviance,delta.beta,leverage)
Diag.kumbang.glm

