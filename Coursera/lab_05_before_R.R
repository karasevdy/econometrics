# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики


h <- read.table("flats_moscow.txt", header = TRUE)
head (h)
tail(h)

qplot(data = h, x=totsp, y=price)
qplot(data = h, totsp, price)

model <- lm(data=h, price~totsp)
summary(model)

coeftest(model) # с точечными оценками порядок
confint(model) # оценка доверительного интервала (в в данной случае не верна из-за гомоскедас)

vcov(model) # оценка ковариационной матрицы (в данной случае не верна из-за гомоскедас)

h <- augment(model, h)
glimpse(h)

# добавились новые переменные
# .resid - остатки
# .fitted - прогнозные значения стоимости квартиры

qplot(data=h, totsp, abs(.resid)) # построили график по X- регрессор, 
# а по Y -  модули (можно взять квадраты) остатков (эпсилан с крышко)

vcov(model) # было
vcovHC(model) # стало (ковариационная матрица устойчивая к гомоскедас)

vcovHC(model, type = "HC2")

# протестируем гипотезы о равенстве 0 коэффициентов моедли с импользованием верных стандартных ошибок

coeftest(model) # было
coeftest(model, vcov. = vcovHC(model)) # стало

# построим доверительные интервалы устойчивые к гетероскедастичность

conftable <- coeftest(model, vcov. = vcovHC(model))
conftable
ci <- data.frame(estimate=conftable[,1],
                 se_hc=conftable[,2])
ci
ci <- mutate(ci,left_ci=estimate-1.96*se_hc,
             right_ci=estimate+1.96*se_hc)
ci # стал доверительный интервал устойчивый к гомоскедаст (шире)
confint(model) # был доверительный интервал (уже)

# Тесты на гетераскедастичность (Тест Уайта = Тест Бройша-Пагана)

bptest(model)
#H_0 - гипотезая о гомоскедастичности (принимается, если p-value > 0.05); в противном случае отвергается.

# Классическую формулу Теста Уайта надо описать чуть подробнее (зависимость не только от пер, но и от квадра)
bptest(model, data=h, varformula = ~ totsp + I(totsp^2) )
# альтернативное написание формулы выше (через "полином" poly)
bptest(model, data = h, varformula = ~ poly(totsp,2))

# Тест Голфилда-Кванта

gqtest(model, order.by = ~ totsp, data = h, fraction = 0.2)
#H_0 - гипотезая о гомоскедастичности (принимается, если p-value > 0.05); в противном случае отвергается.

# Важный спосособ избавление от гетероскедастичности - логарифмирование

qplot(data = h, log(totsp), log(price))
model2 <- lm(data=h, log(price)~log(totsp))
gqtest(model2, order.by = ~ totsp, data = h, fraction = 0.2)

tss <- 120/(1-0.4)
ess <- tss*0.4
ess

d_ts <- ChickWeight
help("ChickWeight")
head(d_ts)
na_perc (d_ts)

str(d_ts)

table(d_ts$weight, include = if(d_ts$Time == "10"))

otbor_po_dni <- function(d_ts) {
  if(d_ts$Time == "10") (res <- mean(d_ts$weight))
  return(res)
}

res
head(d_ts)
d_ts_10d <- d_ts %>% filter_all(all_vars(d_ts$Time == "10"))
head (d_ts_10d)
mean(d_ts_10d$weight)

# альтернативный способ (который объясняется только при решении)
w <- mean(d_ts[d_ts$Time==10,]$weight)
round(w,2)


d_ts_21d <- d_ts %>% filter_all(all_vars(d_ts$Time == "21"))
head (d_ts_21d)

summary(d_ts_21d)
str(d_ts_21d)

d_ts_21d_1d<- d_ts_21d %>% filter_all(all_vars(d_ts_21d$Diet == "1"))
d_ts_21d_2d<- d_ts_21d %>% filter_all(all_vars(d_ts_21d$Diet == "2"))
d_ts_21d_3d<- d_ts_21d %>% filter_all(all_vars(d_ts_21d$Diet == "3"))
d_ts_21d_4d<- d_ts_21d %>% filter_all(all_vars(d_ts_21d$Diet == "4"))

summary(d_ts_21d_1d)
summary(d_ts_21d_2d)

mean (d_ts_21d_1d$weight)
mean (d_ts_21d_2d$weight)
mean (d_ts_21d_3d$weight)
mean (d_ts_21d_4d$weight)

#Альтернативный способ
d2 <- d_ts[d_ts$Time==21,]
mean(d2[d2$Diet == 1,]$weight)
mean(d2[d2$Diet == 2,]$weight)
mean(d2[d2$Diet == 3,]$weight)
mean(d2[d2$Diet == 4,]$weight)

d_ts$Diet_1 <- ifelse(d_ts$Diet == "1", 1, 0) #1
d_ts$Diet_2 <- ifelse(d_ts$Diet == "2", 1, 0) #2
d_ts$Diet_3 <- ifelse(d_ts$Diet == "3", 1, 0) #3

tail(d_ts)

m_ts <- lm(data = d_ts, weight~Time+Diet_1+Diet_2+Diet_3)
summary (m_ts)
round(0.7453,2)

qplot(data = diamonds, log(price),fill=cut)+facet_grid(~cut)
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)
help("diamonds")

str(diamonds)
head(diamonds)

m_d <- lm(data = diamonds, price~carat+table+x+y+depth)
summary(m_d)
53940-7

53940-6

t1 <- qt(0.95, df=53934)
t1
t_kr_l <- -102.490 - (t1*3.084)
t_kr_l

(80-40)/2
40/(75-5)
20/0.5714286

1/(1-0.65)
1/(1-0.8)

help
TRUE
library(Ecdat)
library(rrcov)
library(Ecfun)
install.packages("Ecdat")

install.packages("Ecdat", repos="http://R-Forge.R-project.org")
help("BudgetFood")
data("BudgetFood")
help(mtcars)
d <- mtcars
head(d)

m_c <- lm(data = d, mpg ~ disp+hp+wt)
vif(m_c)
round(7.324517,2)

d <- select(d, mpg, disp, hp, wt)
d <- select(d,-cyl)
head(d)
str(d)

car.pca <- prcomp(d,scale=TRUE)
pca1 <- car.pca$x[,1]
pca2 <- car.pca$x[,2]
pca3 <- car.pca$x[,3]
r <- norm(pca1, type = "2")
round(r,2)

m_c1 <- lm(data = d, mpg ~ pca1+pca2)
m_c2 <- lm(data = d, mpg ~ pca1+pca2+pca3)
summary(m_c1)
summary(m_c2)

0.9909-0.8982

(120/(1-0.4))-120

help("select")

