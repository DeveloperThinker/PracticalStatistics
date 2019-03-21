# Программные пакеты, необходимые для главы 1

# загрузить отсутствующую библиотеку командой install.packages('имя_пакета')

library(dplyr)
library(tidyr)
library(ggplot2)
library(vioplot)
library(ascii)
library(corrplot)
library(descr)

# Импортировать наборы данных, необходимые для главы 1
# Сначала следует определиться с местом хранения данных.
# В Windows 10, к примеру, следует создать:
#     C:\Users\[имя_пользователя]\r_projects\statistics-for-data-scientists\data
#PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')
PSDS_PATH <- file.path('~', 'r_projects/statistics-for-data-scientists')

state <- read.csv(file.path(PSDS_PATH, 'data', 'state.csv'))
dfw <- read.csv(file.path(PSDS_PATH, 'data', 'dfw_airline.csv'))
sp500_px <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'))
sp500_sym <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_sym.csv'))
kc_tax <- read.csv(file.path(PSDS_PATH, 'data', 'kc_tax.csv'))
lc_loans <- read.csv(file.path(PSDS_PATH, 'data', 'lc_loans.csv'))


## Код для создания таблицы штатов
state_asc <- state
state_asc[["Population"]] <- formatC(state_asc[["Population"]], format="d", digits=0, big.mark=",")
ascii(state_asc[1:8,], digits=c(0, 0,1), align=c("l", "l", "r", "r"), caption="Несколько строк из data.frame state с количеством населения и убийств по штатам.")


## Фрагмент кода 1.1
mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])


## Фрагмент кода 1.2
mean(state[["Murder.Rate"]])
library("matrixStats")
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])


## Фрагмент кода 1.3
sd(state[["Population"]])
IQR(state[["Population"]])
mad(state[["Population"]])


## Фрагмент кода 1.4
quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))


## Код для создания перцентильной таблицы PercentileTable
ascii(
  quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Percentiles of murder rate by state.")

  
## Фрагмент кода 1.5
boxplot(state[["Population"]]/1000000, ylab="население (млн. чел.)")


## Фрагмент кода для рисунка 2
png(filename=file.path(PSDS_PATH, "figures", "psds_0102.png"), width = 3, height=4, units='in', res=300)
par(mar=c(0,4,0,0)+.1)
boxplot(state[["Population"]]/1000000, ylab="население (млн. чел.)")
dev.off()


## Фрагмент кода 1.6
breaks <- seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
state['PopFreq'] <- pop_freq
table(pop_freq)


## Фрагмент кода для частотной таблицы FreqTable
state_abb <- state %>%
  arrange(Population) %>%
  group_by(PopFreq) %>%
  summarize(state = paste(Abbreviation, collapse=","), .drop=FALSE) %>%
  complete(PopFreq, fill=list(state='')) %>%
  select(state) 
unlist(state_abb)

lower_br <- formatC(breaks[1:10], format="d", digits=0, big.mark=",")
upper_br <- formatC(c(breaks[2:10]-1, breaks[11]), format="d", digits=0, big.mark=",")

pop_table <- data.frame("BinNumber"=1:10,
                        "BinRange"=paste(lower_br, upper_br, sep="-"),
                        "Count"=as.numeric(table(pop_freq)),
                        "States"=state_abb)
ascii(pop_table, include.rownames=FALSE, digits=c(0, 0, 0, 0), align=c("l", "r", "r", "l"), 
      caption="Частотная таблица численности населения по штатам.")

	  
## Фрагмент кода 1.7
hist(state[["Population"]], breaks=breaks)


## Фрагмент кода для рисунка 3
png(filename=file.path(PSDS_PATH, "figures", "psds_0103.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
pop_hist <- hist(state[["Population"]], breaks=breaks,
                 xlab="население", ylab="частота", main="")
dev.off()


## Фрагмент кода 1.8
hist(state[["Murder.Rate"]], freq=FALSE )
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")


## Фрагмент кода для рисунка 4
png(filename=file.path(PSDS_PATH, "figures", "psds_0104.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
hist(state[["Murder.Rate"]], freq=FALSE, xlab="уровень убийств (на 100 тыс)", ylab="плотность", main="" )
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")
dev.off()


## Фрагмент кода для задержек авиарейсов 
ascii(
  100*as.matrix(dfw/sum(dfw)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Процент задержек из-за причин в аэропорту Даллас/Форт-Уэрт.")


## Фрагмент кода для рисунка 5
png(filename=file.path(PSDS_PATH, "figures", "psds_0105.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4, 4, 0, 1) + .1)
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)
dev.off()


## Фрагмент кода для корреляционной таблицы CorrTable
telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
telecom_cor <- cor(telecom)
ascii(telecom_cor, digits=c( 3,3,3,3,3), align=c("l", "r", "r", "r", "r", "r"), caption="Корреляция между доходностями акций телекоммуникационных компаний.",
      include.rownames = TRUE, include.colnames = TRUE)

	  
## Фрагмент кода 1.10
etfs <- sp500_px[row.names(sp500_px)>"2012-07-01", 
                 sp500_sym[sp500_sym$sector=="etf", 'symbol']]
corrplot(cor(etfs), method = "ellipse")


## Фрагмент кода для рисунка 6
png(filename=file.path(PSDS_PATH, "figures", "psds_0106.png"), width = 4, height=4, units='in', res=300)
etfs <- sp500_px[row.names(sp500_px)>"2012-07-01", sp500_sym[sp500_sym$sector=="etf", 'symbol']]
library(corrplot)
corrplot(cor(etfs), method = "ellipse")
dev.off()


## Фрагмент кода 1.11
plot(telecom$HBAN, telecom$CCL, xlab="T", ylab="VZ")     ## T -> HBAN, VZ -> CCL


## Фрагмент кода для рисунка 7
png(filename=file.path(PSDS_PATH, "figures", "psds_0107.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,1)+.1)
plot(telecom$HBAN, telecom$CCL, xlab="T", ylab="VZ", cex=.8)    ## T -> HBAN, VZ -> CCL
abline(h=0, v=0, col="grey")
dev.off()


## Фрагмент кода 1.12
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving>100 &
                  SqFtTotLiving<3500)
nrow(kc_tax0)


## Фрагмент кода 1.13
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="black") +
  labs(x="общая площадь, кв. футы", y="налогооблагаемая стоимость")

  
## Фрагмент кода для рисунка 8
png(filename=file.path(PSDS_PATH, "figures", "psds_0108.png"),  width = 4, height=4, units='in', res=300)
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="black") +
  labs(x="общая площадь, кв. футы", y="налогооблагаемая стоимость")
dev.off()


## Фрагмент кода 1.14
ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() + 
  geom_point( alpha=0.1) + 
  geom_density2d(colour="white") + 
  labs(x="общая площадь, кв. футы", y="налогооблагаемая стоимость")

  
## Фрагмент кода для рисунка 9
png(filename=file.path(PSDS_PATH, "figures", "psds_0109.png"),  width = 4, height=4, units='in', res=300)
ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() + 
  geom_point(colour="blue", alpha=0.1) + 
  geom_density2d(colour="white") + 
  labs(x="общая площадь, кв. футы", y="налогооблагаемая стоимость")
dev.off()


## Фрагмент кода 1.15: перекрестная таблица CrossTabs
x_tab <- CrossTable(lc_loans$grade, lc_loans$status, 
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

tots <- cbind(row.names(x_tab$tab), format(cbind(x_tab$tab, x_tab$rs)))
props <- cbind("", format(cbind(x_tab$prop.row, x_tab$rs/x_tab$gt), digits=1))
c_tot <- c("Total", format(c(x_tab$cs, x_tab$gt)))

asc_tab <- matrix(nrow=nrow(tots)*2+1, ncol=ncol(tots))
colnames(asc_tab) <- c("Grade", colnames(x_tab$tab), "Total")
idx <- seq(1, nrow(asc_tab)-1, by=2)
asc_tab[idx,] <- tots
asc_tab[idx+1,] <- props
asc_tab[nrow(asc_tab), ] <- c_tot

ascii(asc_tab,  align=c("l", "r", "r", "r", "r"), include.rownames = FALSE, include.colnames = TRUE)


#########################################################################################

##download_from_google_drive(id="0B98qpkK5EJemc3YzUTBoelpjaUU", fname='dfw_airline2.csv', path=PSDS_PATH)
airplanes <- read.csv(file.path(PSDS_PATH, 'data', 'dfw_airline2.csv'), stringsAsFactors = FALSE)
airline_stats <- airplanes %>%
  filter(year>2009,  carrier %in% c("AA", "AS", "B6", "DL", "UA", "WN")) %>%
  transmute(pct_carrier_delay = 100*carrier_ct/arr_flights, 
            pct_atc_delay = 100*nas_ct/arr_flights,
            pct_weather_delay = 100*X.weather_ct/arr_flights,
            airline=factor(carrier, levels = c("AA", "AS", "B6", "DL", "UA", "WN"),
                           labels = c("American", "Alaska", "Jet Blue", "Delta", "United", "Southwest")))
##write.csv(airline_stats, file=file.path(PSDS_PATH, 'data', 'airline_stats.csv'), row.names = FALSE)



## Фрагмент кода 1.16   dfw
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50))


## Фрагмент кода для рисунка 10
png(filename=file.path(PSDS_PATH, "figures", "psds_0110.png"), width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50), cex.axis=.6,
        ylab="ежедневный % задержанных рейсов")
dev.off()


## Фрагмент кода 1.17
ggplot(data=airline_stats, aes(airline, pct_carrier_delay))  + 
  ylim(0, 50) + 
  geom_violin() +
  labs(x="", y="ежедневный % задержанных рейсов")

  
## Фрагмент кода для рисунка 11
png(filename=file.path(PSDS_PATH, "figures", "psds_0111.png"), width = 4, height=4, units='in', res=300)
ggplot(data=airline_stats, aes(airline, pct_carrier_delay)) + 
  ylim(0, 50) + 
  geom_violin(draw_quantiles = c(.25, .5, .75), linetype=2) +
  geom_violin(fill=NA, size=1.1) +
  theme_bw() + 
  labs(x="", y="% задержанных авиарейсов")
dev.off()


## Фрагмент кода 1.18
ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
         aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient( low="white", high="black") +
  labs(x="общая площадь, кв. футы", y="налогооблагаемая стоимость")
  facet_wrap("ZipCode")

  
## Фрагмент кода для рисунка 12
png(filename=file.path(PSDS_PATH, "figures", "psds_0112.png"), width = 5, height=4, units='in', res=300)
ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient( low="gray95", high="blue") +
  labs(x="общая площадь, кв. футы", y="налогооблагаемая стоимость")
  facet_wrap("ZipCode")
dev.off()
