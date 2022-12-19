### PONTIFICIA UNIVERSIDAD CATÓLICA DE CHILE 
### FACULTAD DE MATEMÁTICAS / DEPARTAMENTO DE ESTADÍSTICA
### SANTIAGO URQUIZO ROMO

# MODELO ESTADÍSTICO PARA LA FIJACIÓN DE PRECIOS EN LA EMPRESA DIT SPA

# Librerías ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(rio)
library(PerformanceAnalytics)
library(GGally)
library(MASS)

# Datos -------------------------------------------------------------------

dit <- import("../Proyecto UC/bd_dit.xlsx")

dit$FECHA <- as.Date(dit$FECHA)
dit$TEMP <- as.factor(dit$TEMP)

head(dit)

# Limpieza ----------------------------------------------------------------

# Rango Intercuartil e idintificación de datos atipicos
summary(data1$PRECIO)
liminf <- quantile(data1$PRECIO)[2] - 1.5*IQR(data1$PRECIO)
limsup <- quantile(data1$PRECIO)[4] + 3*IQR(data1$PRECIO)


# Análisis descriptivo ----------------------------------------------------

summary(dit)
names(dit)

# Demanda vs Precio (Zirconio)
summary(dit$DZ)
summary(dit$PZ)

par(mar = c(5.25, 4.25, 4.25, 4.25))
plot(x = dit$FECHA, y = dit$DZ, type = "l", xlab = "Fecha", xaxt = "n",
     ylab = "Demanda", ylim = c(0,1400),
     main = "Unidades vendidas vs Precio promedio (Estructuras de Zirconio)")
axis(1, dit$FECHA, format(dit$FECHA, "%b %y"), cex.axis = .7)
par(new = TRUE)
plot(x = dit$FECHA, y = dit$PZ, type = "l", col = "blue", axes = FALSE,
     bty = "n", xlab = "", ylab = "", ylim = c(16000,24000))
axis(4,col.axis = "blue")
mtext("Precio", side = 4, line = 3, col = "blue")


# Demanda vs Precio (Metal)
summary(dit$DM)
summary(dit$PM)

par(mar = c(5.25, 4.25, 4.25, 4.25))
plot(x = dit$FECHA, y = dit$DM, type = "l", xlab = "Fecha", xaxt = "n",
     ylab = "Demanda", ylim = c(0,1800),
     main = "Unidades vendidas vs Precio promedio (Estructuras de metal)")
axis(1, dit$FECHA, format(dit$FECHA, "%b %y"), cex.axis = .7)
par(new = TRUE)
plot(x = dit$FECHA, y = dit$PM, type = "l", col = "blue", axes = FALSE,
     bty = "n", xlab = "", ylab = "", ylim = c(0,28000))
axis(4,col.axis = "blue")
mtext("Precio", side = 4, line = 3, col = "blue")


# variación de precios en el tiempo 

par(mfrow = c(1, 3))

plot(x = dit$FECHA, y = dit$PS, type = "l", xlab = "Fecha", xaxt = "n",
     ylab = "Precio de servicios", ylim = c(0,15000))
axis(1, dit$FECHA, format(dit$FECHA, "%b %y"), cex.axis = .7)

plot(x = dit$FECHA, y = dit$PP, type = "l", xlab = "Fecha", xaxt = "n",
     ylab = "Precio de estructuras provisorios", ylim = c(0,15000))
axis(1, dit$FECHA, format(dit$FECHA, "%b %y"), cex.axis = .7)

plot(x = dit$FECHA, y = dit$PC, type = "l", xlab = "Fecha", xaxt = "n",
     ylab = "Precio de componentes dentales", ylim = c(0,15000))
axis(1, dit$FECHA, format(dit$FECHA, "%b %y"), cex.axis = .7)

par(mfrow = c(1, 1))


# Análisis de correlación

dit_cor <- dit %>% dplyr::select(-FECHA,-TEMP)
chart.Correlation(dit_cor, histogram = F, pch = 19)


chart.Correlation(log(dit_cor), histogram = F, pch = 19)

ggpairs(dit_cor,upper = list(continuous = wrap("cor", size = 3)))


# Modelamiento ------------------------------------------------------------

names(dit)
dit_log <- log(dit[,-c(1,9)])
dit_log$TEMP <- dit$TEMP

n=nrow(dit_log)

#Modelo Demanda de Metal
full.model.mt <- lm(DM ~., data = dit_log)
step.model.mt <- stepAIC(full.model.mt, direction = "both", trace = T)
summary(step.model.mt)
summary(full.model.mt)

AIC(full.model.mt)
AIC(step.model.mt)

performance::check_model(step.model.mt)

shapiro.test(step.model.mt$residuals)
lmtest::bptest(step.model.mt)


#validación modelo metal
p=length(coef(step.model.mt))

hii<-ls.diag(step.model.mt)$hat
i<-seq(1,n,1)
i[hii>=2*p/n]

t.bc<-ls.diag(step.model.mt)$stud.res
y.gorro.bc<-step.model.mt$fitted
a<-max(abs(t.bc),qt(0.975,(n - p - 1)))
i<-seq(1,n,1)
i[abs(t.bc)>=qt(0.975,(n - p - 1))]



### Modelo Demanda de Zirconio

full.model.zr <- lm(DZ ~., data = dit_log)
step.model.zr <- stepAIC(full.model.zr, direction = "both", trace = FALSE)
summary(step.model.zr)

AIC(full.model.zr)
AIC(step.model.zr)

performance::check_model(step.model.zr)

# Análisis de colinealidad
X=cbind(dit_log$DM,dit_log$PP,dit_log$PZ,dit_log$TEMP)
V=solve(cor(X))
VIFs=diag(V)
VIFs

# Probamos con otro modelo

modelo0_zr =lm(DZ ~ 1, data = dit_log)
add1(modelo0_zr, ~ DM + PS + PM + PP + PZ + PC + TEMP, test="F")

modelo1_zr =lm(DZ ~ TEMP, data = dit_log)
add1(modelo1_zr, ~ DM + PS + PM + PP + PZ + PC + TEMP, test="F")

modelo2_zr =lm(DZ ~ TEMP + PS, data = dit_log)
add1(modelo2_zr, ~ DM + PS + PM + PP + PZ + PC + TEMP, test="F")

modelo3_zr =lm(DZ ~ TEMP + PS + PP, data = dit_log)
add1(modelo3_zr, ~ DM + PS + PM + PP + PZ + PC + TEMP, test="F")

summary(modelo3_zr)

performance::check_model(modelo3_zr)

# Análisis de residuos

## Análisis de colinealidad
X=cbind(dit_log$DZ, dit_log$PP, dit_log$PS, dit_log$TEMP)
V=solve(cor(X))
VIFs=diag(V)
VIFs

## Test de Kolmogorov-Smirnov
r <- ls.diag(modelo3_zr)$std.res
ks.test(r, "pnorm")

#Homocedasticidad
lmtest::bptest(modelo3_zr)

# Coeficientes hii
names(dit)

#Distancia de cook
D<-ls.diag(step.model.zr)$cooks
i[D>=4/(n - p - 1)]


t.bc<-ls.diag(step.model.zr)$stud.res
y.gorro.bc<-step.model.zr$fitted
a<-max(abs(t.bc),qt(0.975,(n - p - 1)))
i<-seq(1,n,1)
i[abs(t.bc)>=qt(0.975,(n - p - 1))]



exp(8.06545)
exp(75.4602)



cor(dit$DZ, dit$PS)
plot(dit$DZ, dit$PS)

cor(dit_log$DZ, dit_log$PS)
