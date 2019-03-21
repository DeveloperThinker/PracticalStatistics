library(ggplot2)
library(dplyr)
library(lmPerm)

## Импортировать наборы данных, необходимых для главы 4
## PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')
PSDS_PATH <- file.path('~', 'r_projects/statistics-for-data-scientists')

session_times <- read.csv(file.path(PSDS_PATH, 'data', 'web_page_data.csv'))
session_times[,2] <- session_times[,2] * 100
four_sessions  <- read.csv(file.path(PSDS_PATH, 'data', 'four_sessions.csv'))
click_rate <-  read.csv(file.path(PSDS_PATH, 'data', 'click_rates.csv'))
imanishi <-  read.csv(file.path(PSDS_PATH, 'data', 'imanishi_data.csv'))


## Фрагмент кода 3.1
ggplot(session_times, aes(x=Page, y=Time)) + 
  geom_boxplot()

  
## Фрагмент кода для рисунка 3
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0303.png'),  width = 4, height=4, units='in', res=300)
ggplot(session_times, aes(x=Page, y=Time)) + 
  geom_boxplot() +
  labs(y='время (сек.)', x='страница') + 
  theme_bw()
dev.off()

mean_a <- mean(session_times[session_times['Page']=='Page A', 'Time'])
mean_b <- mean(session_times[session_times['Page']=='Page B', 'Time'])
mean_b - mean_a


## Пример перестановочнго теста с прилипчивостью веб-страниц
perm_fun <- function(x, n1, n2)
{
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}


## Фрагмент кода для рисунка 4
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0304.png'),  width = 4, height=4, units='in', res=300)
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = perm_fun(session_times[,'Time'], 21, 15)
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='разницы во времени сессий (сек.)', ylab='частота', main='')
abline(v = mean_b - mean_a)
dev.off()

mean(perm_diffs > (mean_b - mean_a))


## Фрагмент кода для рисунка 5
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0305.png'),  width = 4, height=4, units='in', res=300)
obs_pct_diff <- 100*(200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion, 23739, 22588 )
hist(perm_diffs, xlab='уровень конверсии (%)', ylab='частота', main='')
abline(v = obs_pct_diff, lty=2, lwd=1.5)
text("  наблюдаемая\n  разница", x=obs_pct_diff,  y=par()$usr[4]-20, adj=0)
dev.off()

mean(perm_diffs > obs_pct_diff)

prop.test(x=c(200,182), n=c(23739,22588), alternative="greater")
## Гистограмма повторной выборки
## проверка на основе t-статистики
t.test(Time ~ Page, data=session_times, alternative='less' )


## времена сеансев

## четырехгрупповая ANOVA

## Фрагмент кода для рисунка 6
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0306.png'),  width = 4, height=4, units='in', res=300)
ggplot(four_sessions, aes(x=Page, y=Time)) + 
  geom_boxplot() +
  labs(y='время (сек.)', x='страница') +
  theme_bw()
dev.off()


summary(aovp(Time ~ Page, data=four_sessions))
summary(aov(Time ~ Page, data=four_sessions))


## Проверка на основе статистики хи-квадрат
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)
dimnames(clicks) <- list(unique(click_rate$Headline), unique(click_rate$Click))

chisq.test(clicks, simulate.p.value=TRUE)

chisq.test(clicks, simulate.p.value=FALSE)


## Фрагмент кода для рисунка 7
x <- seq(1, 30, length=100)
chi <- data.frame(df = factor(rep(c(1, 2, 5, 10), rep(100, 4))),
                  x = rep(x, 4),
                  p = c(dchisq(x, 1), dchisq(x, 2), dchisq(x, 5), dchisq(x, 20)))

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0307.png'),  width = 5, height=3, units='in', res=300)
ggplot(chi, aes(x=x, y=p)) +
  geom_line(aes(linetype=df)) +
  theme_bw() +
  labs(x='', y='')
dev.off()


## Точная проверка Фишера
fisher.test(clicks)
## пример Тафтса


## Фрагмент кода для рисунка 8
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0308.png'),  width = 4, height=4, units='in', res=300)
imanishi$Digit <- factor(imanishi$Digit)
ggplot(imanishi, aes(x=Digit, y=Frequency)) +
  geom_bar(stat="identity") +
  theme_bw() +
  labs(x='цифра', y='частота')
dev.off()
