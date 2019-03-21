library(dplyr)
library(tidyr)
library(ggplot2)
library(ascii)
library(lubridate)
library(ellipse)
library(mclust)
library(cluster)


###############################################################
## Импортировать наборы данных, необходимых для главы 6
## PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')
PSDS_PATH <- file.path('~', 'r_projects/statistics-for-data-scientists')

sp500_px <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'))
sp500_sym <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_sym.csv'))
loan_data <- read.csv(file.path(PSDS_PATH, 'data', 'loan_data.csv'))


###############################################################
## PCA для данных об нефтяных акций
oil_px = as.data.frame(scale(oil_px, scale=FALSE))
oil_px <- sp500_px[, c('CVX', 'XOM')]
pca <- princomp(oil_px)
pca$loadings


## Фрагмент кода для рисунка 7-1: главные компоненты для данных нефтяных акций
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0701.png'), width = 4, height=4, units='in', res=300)
loadings <- pca$loadings
ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values=c(46)) +
  stat_ellipse(type='norm', level=.99, color='grey25') +
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1], color='grey25', linetype=2) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2],  color='grey25', linetype=2) +
  scale_x_continuous(expand=c(0,0), lim=c(-3, 3)) + 
  scale_y_continuous(expand=c(0,0), lim=c(-3, 3)) +
  theme_bw()
dev.off()


## Фрагмент кода для рисунка 7-2: график каменистой осыпи 
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0702.png'), width = 4, height=4, units='in', res=300)
syms <- c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP',
           'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_cons <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]
sp_pca <- princomp(top_cons)
par(mar=c(6,3,0,0)+.1, las=2)
screeplot(sp_pca, main='')
dev.off()


## Loadings for stock data
loadings = sp_pca$loadings[,1:5]
loadings <- as.data.frame(loadings)
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings, "Component", "Weight", -Symbol)
head(loadings)


## Фрагмент кода для рисунка 7-3: график нагрузок компонент
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0703.png'), width = 4, height=4, units='in', res=300)
loadings$Color = loadings$Weight > 0
ggplot(loadings, aes(x=Symbol, y=Weight, fill=Color)) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(Component ~ ., scales='free_y') +
  guides(fill=FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5)) +
  labs(y="нагрузки компонент")
dev.off()


###############################################################
## Раздел K-средних

set.seed(1010103)
df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]
km <- kmeans(df, centers=4, nstart=10)

df$cluster <- factor(km$cluster)
head(df)

centers <- data.frame(cluster=factor(1:4), km$centers)
centers


## Фрагмент кода для рисунка 7-4: кластеры K-средних для двую акций
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0704.png'), width = 4, height=3, units='in', res=300)
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = 1:4,
                     guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=XOM, y=CVX), size=2, stroke=2)  +
  theme_bw() +
  scale_x_continuous(expand=c(0,0), lim=c(-2, 2)) + 
  scale_y_continuous(expand=c(0,0), lim=c(-2.5, 2.5)) 
dev.off()


## Алгоритм центров кластеров / центроидов
syms <- c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP',
           'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
df <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]

set.seed(10010)
km <- kmeans(df, centers=5, nstart=10)
km$size
centers <- km$centers


#centers <- scale(scale(centers, center=FALSE, scale=1/attr(df, 'scaled:scale')),
#                 center=-attr(df, 'scaled:center'), scale=FALSE)


## Фрагмент кода для рисунка 7-5: интепретация кластеров
centers <- as.data.frame(t(centers))
names(centers) <- paste("Cluster", 1:5)
centers$Symbol <- row.names(centers)
centers <- gather(centers, "Cluster", "Mean", -Symbol)

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0705.png'), width = 4, height=5, units='in', res=300)
centers$Color = centers$Mean > 0
ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(Cluster ~ ., scales='free_y') +
  guides(fill=FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5)) +
  labs(y="нагрузки компонент")
dev.off()


## Фрагмент кода для рисунка 7-6: выбор количества кластеров (точка локтя)
pct_var <- data.frame(pct_var = 0,
                      num_clusters=2:14)
totalss <- kmeans(df, centers=14, nstart=50, iter.max = 100)$totss
for(i in 2:14){
  pct_var[i-1, 'pct_var'] <- kmeans(df, centers=i, nstart=50, iter.max = 100)$betweenss/totalss
}

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0706.png'), width = 4, height=3, units='in', res=300)
ggplot(pct_var, aes(x=num_clusters, y=pct_var)) +
  geom_line() +
  geom_point() +
  labs(y='% объясненной дисперсии', x='количество кластеров') +
  scale_x_continuous(breaks=seq(2, 14, by=2))   +
  theme_bw()
dev.off()


################################################################
## Раздел о hclust

syms1 <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 
           'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP',
           'WMT', 'TGT', 'HD', 'COST')

df <- sp500_px[row.names(sp500_px)>='2011-01-01', syms1]
d <- dist(t(df))
hcl <- hclust(d)


## Фрагмент кода для рисунка 7-7: дендограмма данных об акциях
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0707.png'), width = 4, height=4, units='in', res=300)
par(cex=.75, mar=c(0, 5, 0, 0)+.1)
plot(hcl, ylab='расстояние', xlab='', sub='', main='')
dev.off()


## Фрагмент кода для рисунка 7-8: сравнение разных мер различия 
cluster_fun <- function(df, method)
{
  d <- dist(df)
  hcl <- hclust(d, method=method)
  tree <- cutree(hcl, k=4)
  df$cluster <- factor(tree)
  df$method <- method
  return(df)
}

df0 <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]
df <- rbind(cluster_fun(df0, method='single'),
            cluster_fun(df0, method='average'),
            cluster_fun(df0, method='complete'),
            cluster_fun(df0, method='ward.D'))
df$method <- ordered(df$method, c('single', 'average', 'complete', 'ward.D'))


png(filename=file.path(PSDS_PATH, 'figures', 'psds_0708.png'), width = 5.5, height=4, units='in', res=300)
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = c(46, 3, 1,  4),
                     guide = guide_legend(override.aes=aes(size=2))) +
  facet_wrap( ~ method) +
  theme_bw()
dev.off()


###############################################################
# Модельно-ориентированная кластеризация
# Многомерное нормальное распределеине 

mu <- c(.5, -.5)
sigma <- matrix(c(1, 1, 1, 2), nrow=2)
prob <- c(.5, .75, .95, .99) ## либо любые другие по желанию
names(prob) <- prob ## чтобы получить ИД столбца в результате
x <- NULL
for (p in prob){
  x <- rbind(x,  ellipse(x=sigma, centre=mu, level=p))
}
df <- data.frame(x, prob=factor(rep(prob, rep(100, length(prob)))))
names(df) <- c("X", "Y", "Prob")


## Фрагмент кода для рисунка 7-9: многомерные нормальные эллипсы
dfmu <- data.frame(X=mu[1], Y=mu[2])
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0709.png'), width = 4, height=4, units='in', res=300)
ggplot(df, aes(X, Y)) + 
  geom_path(aes(linetype=Prob)) +
  geom_point(data=dfmu, aes(X, Y), size=3) +
  theme_bw()
dev.off()


## Фрагмент кода для рисунка 7-10: mclust применительно к XOM и CVX
df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]
mcl <- Mclust(df)
summary(mcl)

cluster <- factor(predict(mcl)$classification)

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0710.png'), width = 5, height=4, units='in', res=300)
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) +
  geom_point(alpha=.8) +
  theme_bw() +
  scale_shape_manual(values = c(46, 30, 20, 10, 3),    ## UPD c(46, 3)
                     guide = guide_legend(override.aes=aes(size=2))) 
dev.off()

summary(mcl, parameters=TRUE)$mean
summary(mcl, parameters=TRUE)$variance


## Фрагмент кода для рисунка 7-11: BIC-оценки для разных моделей, подогнанных при помощи mclust
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0711.png'), width = 4, height=4, units='in', res=300)
par(mar=c(4, 5, 0, 0)+.1)
plot(mcl, what='BIC', ask=FALSE, ylab='BIC', xlab='число компонент', cex=.75)
dev.off()
#


#######################################################################
# Раздел о шкалировании

defaults <- loan_data[loan_data$outcome=='default',]
df <- defaults[, c('loan_amnt', 'annual_inc', 'revol_bal', 'open_acc', 'dti', 'revol_util')]
km <- kmeans(df, centers=4, nstart=10)
centers <- data.frame(size=km$size, km$centers) 
round(centers, digits=2)

df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart=10)
centers0 <- scale(km0$centers, center=FALSE, scale=1/attr(df0, 'scaled:scale'))
centers0 <- scale(centers0, center=-attr(df0, 'scaled:center'), scale=FALSE)
centers0 <- data.frame(size=km0$size, centers0) 
round(centers, digits=2)

km <- kmeans(df, centers=4, nstart=10)
centers <- data.frame(size=km$size, km$centers) 
round(centers, digits=2)


## Фрагмент кода для рисунка 7-12: график каменистой осыпи для данных с доминантными переменными
syms <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 
          'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_15 <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]
sp_pca1 <- princomp(top_15)

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0712.png'), width = 4, height=4, units='in', res=300)
par(mar=c(6,3,0,0)+.1, las=2)
screeplot(sp_pca1, main='')
dev.off()

round(sp_pca1$loadings[,1:2], 3)


#
###############################################################
## Фрагмент кода для рисунка 7-13: категориальные данные и расстояние Говера

x <- loan_data[1:5, c('dti', 'payment_inc_ratio', 'home_', 'purpose_')]
x

daisy(x, metric='gower')

set.seed(301)
df <- loan_data[sample(nrow(loan_data), 250),
                c('dti', 'payment_inc_ratio', 'home_', 'purpose_')]
d = daisy(df, metric='gower')
hcl <- hclust(d)
dnd <- as.dendrogram(hcl)

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0713.png'), width = 4, height=4, units='in', res=300)
par(mar=c(0,5,0,0)+.1)
plot(dnd, leaflab='none', ylab='расстояние')
dev.off()

dnd_cut <- cut(dnd, h=0.5)
df[labels(dnd_cut$lower[[1]]),]


## Проблемы в кластеризации со смешанными типами данных
df <- model.matrix(~ -1 + dti + payment_inc_ratio + home_ + pub_rec_zero, data=defaults)
df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart=10)
centers0 <- scale(km0$centers, center=FALSE, scale=1/attr(df0, 'scaled:scale'))
round(scale(centers0, center=-attr(df0, 'scaled:center'), scale=FALSE), 2)
