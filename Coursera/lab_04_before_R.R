# ESLI RUSSKIE BUKVI NE VIDNI --->
# File -- Reopen with encoding --- utf8 --- set as default --- ok


library("HSAUR") # из этого пакета возьмем набор данных по семиборью
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тесты для линейных моделей
library("glmnet") # LASSO + ridge
library("ggplot2") # графики
library("car") # vif

h <- cars
qplot(data = h, speed, dist)
model <- lm(data = h, dist~speed)
summary (model)

h <- mutate(h, speed2= speed^2, speed3= speed^3)
model_mk <- lm(data = h, dist~speed + speed2+speed3)

summary(model_mk)

#коэффициенты вздутия дисперсии

vif(model_mk)

#вывести коэффициенты модели без свободного члена в виде матрицы
x0 <- model.matrix(data=h, dist~0 + speed + speed2+speed3)
head(x0)

# поссчтать корреляцию между регрессорами
cor(x0)

nd <- data.frame (speed=10, speed2=100, speed3=1000)

# постороим прогнозный предиктивный интервал для модели с одной
predict(model, newdata = nd, interval = "prediction")

predict(model_mk, newdata = nd, interval = "prediction")

# доверительный интервал для коэффициентов модели
confint(model)

confint(model_mk) # доверительный интервал для модели с мультиколинеарностьсю для переменной speed гораздо шире

# Ридж и Lasso регрессии 
y <- h$dist
x0 <- model.matrix(data=h, dist~0 + speed + speed2+speed3)

#LASSO (alpha = 1 в glmnet)
lambdas <- seq(50,0.1,length=30)
m_lasso <- glmnet(x0,y,alpha = 1, lambda = lambdas)
coef (m_lasso, s=c(0.1,1)) # как выглядят коэффициенты Lasso-регрессии для двух разных лямбд

plot(m_lasso, xvar="lambda", label=TRUE)
plot(m_lasso, xvar="dev", label=TRUE) # по Х доля объясненной дисперсии
plot(m_lasso, xvar="norm", label=TRUE) # по Х норма вектора

# Ридж регрессия (alpha = 0 в glmnet)

m_rr <- glmnet(x0,y,alpha = 1, lambda = lambdas)

# Метод cross validation чтобы выбрать оптимальную лямбду

cv <- cv.glmnet(x0,y,alpha=1)
plot(cv)
cv$lambda.min
cv$lambda.1se

coef(cv,s="lambda.1se")

# PCA Метод главных компонент

h <- heptathlon
help("heptathlon")

glimpse(h)

#удаление переменно
h <- select(h,-score)
describe(h)

cor(h)

#стандартизация переменных (scale=true)
h.pca <- prcomp(h,scale=TRUE)
pca1 <- h.pca$x[,1] # первый столбик новых Х
v1 <- h.pca$rotation[,1]

v1 # веса с которыми старые переменные (виды спорта) входят в новую синтетическую переменную

head(pca1)
summary(h.pca)

cor(heptathlon$score,pca1)
plot(h.pca) # какую долю дисперсии объясняет каждая из главных компонент

biplot(h.pca,xlim=c(-1,1))

1/(1-0.6)

d <- airquality
help(airquality)
str(d)

qplot(data = d, Ozone, Wind)
m_oz <- lm(data = d, Ozone~Solar.R+Wind+Temp)
vif(m_oz)

d <- na.omit(d)
y <- d$Ozone
x0 <- model.matrix(data=d, Ozone~0+Solar.R+Wind+Temp)
lambdas <- seq(50,0.1,length=30)
m_oz_lasso <- glmnet(x0,y,alpha = 1, lambda = lambdas)
coef (m_oz_lasso, s=1)

m_oz_mrr <- glmnet(x0,y,alpha = 0, lambda = lambdas)
coef(m_oz_mrr, s=2)

plot(m_oz_lasso, xvar="norm")
plot(m_oz_lasso, xvar="dev")
plot(m_oz_lasso, xvar="lambda")

oz.pca <- prcomp(x0,scale=TRUE)
pca1 <- oz.pca$x[,1]
pca2 <- oz.pca$x[,2]
pca3 <- oz.pca$x[,3]

qplot(data = d,pca1,pca2)
