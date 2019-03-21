library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ascii)
library(lubridate)
library(splines)
library(mgcv)


## Импортировать наборы данных, необходимых для главы 4
## PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')
PSDS_PATH <- file.path('~', 'r_projects/statistics-for-data-scientists')


lung <- read.csv(file.path(PSDS_PATH, 'data', 'LungDisease.csv'))
zhvi <- read.csv(file.path(PSDS_PATH, 'data', 'County_Zhvi_AllHomes.csv'))
zhvi <- unlist(zhvi[13,-(1:5)])
dates <- parse_date_time(paste(substr(names(zhvi), start=2, stop=8), "01", sep="."), "Ymd")
zhvi <- data.frame(ym=dates, zhvi_px=zhvi, row.names = NULL) %>%
        mutate(zhvi_idx=zhvi/last(zhvi))
house <- read.csv(file.path(PSDS_PATH, 'data', 'house_sales.csv'), sep='\t')


## Фрагмент кода для рисунка 1
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0401.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
plot(lung$Exposure, lung$PEFR, xlab="воздействие", ylab="PEFR")
dev.off()


## Фрагмент кода 4.1
model <- lm(PEFR ~ Exposure, data=lung)
model


## Фрагмент кода для рисунка 2
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0402.png'), width = 350, height = 350)
par(mar=c(4,4,0,0)+.1)

plot(lung$Exposure, lung$PEFR, xlab="воздействие", ylab="PEFR", ylim=c(300,450), type="n", xaxs="i")
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=2)
text(x=.3, y=model$coefficients[1], labels=expression("b"[0]),  adj=0, cex=1.5)
x <- c(7.5, 17.5)
y <- predict(model, newdata=data.frame(Exposure=x))
segments(x[1], y[2], x[2], y[2] , col="red", lwd=2, lty=2)
segments(x[1], y[1], x[1], y[2] , col="red", lwd=2, lty=2)
text(x[1], mean(y), labels=expression(Delta~Y), pos=2, cex=1.5)
text(mean(x), y[2], labels=expression(Delta~X), pos=1, cex=1.5)
text(mean(x), 400, labels=expression(b[1] == frac(Delta ~ Y, Delta ~ X)), cex=1.5)
dev.off()


## Фрагмент кода 4.2
fitted <- predict(model)
resid <- residuals(model)


## Фрагмент кода для рисунка 3
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0403.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
lung1 <- lung %>%
  mutate(Fitted=fitted,
         positive = PEFR>Fitted) %>%
  group_by(Exposure, positive) %>%
  summarize(PEFR_max = max(PEFR), 
            PEFR_min = min(PEFR),
            Fitted = first(Fitted)) %>%
  ungroup() %>%
  mutate(PEFR = ifelse(positive, PEFR_max, PEFR_min)) %>%
  arrange(Exposure)

plot(lung$Exposure, lung$PEFR, xlab="воздействие", ylab="PEFR")
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=2)
segments(lung1$Exposure, lung1$PEFR, lung1$Exposure, lung1$Fitted, col="red", lty=3)
dev.off()


## Фрагмент кода 4.3
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

			   
## Фрагмент кода 4.4
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,  
               data=house, na.action=na.omit)

			   
## Фрагмент кода 4.5
house_lm


## Фрагмент кода 4.6
summary(house_lm)


## Фрагмент кода 4.7
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)

				 
## Фрагмент кода 4.8
step_lm <- stepAIC(house_full, direction="both")
step_lm

lm(AdjSalePrice ~  Bedrooms, data=house)


## Фрагмент кода 4.9: взвешенная регрессия
house$Year = year(house$DocumentDate)
house$Weight = house$Year - 2005


## Фрагмент кода 4.10
house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,
               data=house, weight=Weight, na.action=na.omit)
round(cbind(house_lm=house_lm$coefficients, 
            house_wt=house_wt$coefficients), digits=3)


## Фрагмент кода 4.11: факторные переменные
head(house[, 'PropertyType'])


## Фрагмент кода 4.12
prop_type_dummies <- model.matrix(~PropertyType -1, data=house)
head(prop_type_dummies)


## Фрагмент кода 4.13
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms +  BldgGrade + PropertyType, data=house)

	 
## Фрагмент кода 4.14
table(house$ZipCode)


## Фрагмент кода 4.15
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  # сортировать почтовые индексы по медианному остатку
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))
house <- house %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')


# Фрагмент кода 4.15: коррелированные переменные
step_lm$coefficients


# Фрагмент кода 4.16
update(step_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)


## Фрагмент кода 4.17: искажающие переменные
lm(AdjSalePrice ~  SqFtTotLiving + SqFtLot + 
     Bathrooms + Bedrooms + 
     BldgGrade + PropertyType + ZipGroup,
   data=house, na.action=na.omit)


## Фрагмент кода 4.18: взаимодействия
lm(AdjSalePrice ~  SqFtTotLiving*ZipGroup + SqFtLot + 
     Bathrooms + Bedrooms + 
     BldgGrade + PropertyType,
   data=house, na.action=na.omit)

head(model.matrix(~C(PropertyType, sum) , data=house))


## Фрагмент кода 4.19: анализ выбросов
house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, data=house_98105)

				 
## Фрагмент кода 4.20
sresid <- rstandard(lm_98105)
idx <- order(sresid, decreasing=FALSE)
sresid[idx[1]]
resid(lm_98105)[idx[1]]


## Фрагмент кода 4.21
house_98105[idx[1], c('AdjSalePrice', 'SqFtTotLiving', 'SqFtLot',
                      'Bathrooms', 'Bedrooms', 'BldgGrade')]

					  
# Фрагмент кода для рисунка 4-5: влиятельные точки данных в регрессии
seed <- 11
set.seed(seed)
x <- rnorm(25)
y <- -x/5 + rnorm(25)
x[1] <- 8
y[1] <- 8

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0405.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(3,3,0,0)+.1)
plot(x, y, xlab='', ylab='', pch=16)
model <- lm(y~x)
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=3)
model <- lm(y[-1]~x[-1])
abline(a=model$coefficients[1], b=model$coefficients[2], col="red", lwd=3, lty=2)
dev.off()


## Фрагмент кода 4.22: влиятельные наблюдения
std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)


## Фрагмент кода для рисунка 4-6
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0406.png'), width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)
dev.off()


## Фрагмент кода для таблицы 4-2
lm_98105_inf <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + 
                     Bathrooms +  Bedrooms + BldgGrade,
                   subset=cooks_D<.08, data=house_98105)

df <- data.frame(lm_98105$coefficients,
                 lm_98105_inf$coefficients)
names(df) <- c('Оригинал', 'Влиятельные убраны')
ascii((df),
      include.rownames=TRUE, include.colnames=TRUE, header=TRUE,
      digits=rep(0, 3), align=c("l", "r", "r") ,
      caption="Сравнение коэффициентов регрессии с полными данными и убранными влиятельными данными")

	  
## Фрагмент кода 4.23: гетероскедастичность
df <- data.frame(
  resid = residuals(lm_98105),
  pred = predict(lm_98105))
ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth() 

  
## Фрагмент кода для рисунка 4-7
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0407.png'), width = 4, height=4, units='in', res=300)
ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth() +
  theme_bw() 
dev.off()


## Фрагмент кода для рисунка 4-8
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0408.png'), width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
hist(std_resid, ylab='частота', main='')
dev.off()


## Фрагмент кода 4.24: график частных остатков
terms <- predict(lm_98105, type='terms')
partial_resid <- resid(lm_98105) + terms


## Фрагмент кода 4.25
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 'SqFtTotLiving'],
                 PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))  

  
## Фрагмент кода для рисунка 4-9
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0409.png'),  width = 4, height=4, units='in', res=300)
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 'SqFtTotLiving'],
                 PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  theme_bw() +
  geom_line(aes(SqFtTotLiving, Terms)) 
dev.off()


## Фрагмент кода 4.26
lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot +
   BldgGrade + Bathrooms +  Bedrooms, 
   data=house_98105)


lm_poly <- lm(AdjSalePrice ~  poly(SqFtTotLiving, 2) + SqFtLot + 
                BldgGrade +  Bathrooms +  Bedrooms,
              data=house_98105)
terms <- predict(lm_poly, type='terms')
partial_resid <- resid(lm_poly) + terms


## Фрагмент кода для рисунка 4-10
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0410.png'), width = 4, height=4, units='in', res=300)
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 1],
                 PartialResid = partial_resid[, 1])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))+
  theme_bw()
dev.off()


## Фрагмент кода 4.27
knots <- quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots=knots, degree=3) +  SqFtLot +  
                  Bathrooms + Bedrooms + BldgGrade,  data=house_98105)


terms <- predict(lm_spline, type='terms')
partial_resid <- resid(lm_spline) + terms


## Фрагмент кода для рисункаe 4-12
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0412.png'), width = 4, height=4, units='in', res=300)
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 1],
                 PartialResid = partial_resid[, 1])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))+
  theme_bw()
dev.off()


## Фрагмент кода 4.27
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot + 
                Bathrooms +  Bedrooms + BldgGrade, 
              data=house_98105)
terms <- predict.gam(lm_gam, type='terms')
partial_resid <- resid(lm_gam) + terms


## Фрагмент кода для рисунка 4-13
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0413.png'), width = 4, height=4, units='in', res=300)
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 5],
                 PartialResid = partial_resid[, 5])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))  +
  theme_bw()
dev.off()



