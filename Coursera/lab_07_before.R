# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

# lab 07

library("dplyr") # манипуляции с данными
library("erer") # расчет предельных эффектов
library("vcd") # графики для качественных данных
library("ggplot2") # графики
library("reshape2") # манипуляции с данными
library("AUC") # для ROC кривой

# при загрузке файлов R автоматом переделывает все строковые переменные в факторные
# эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по пассажирам Титаника
t <- read.csv("titanic3.csv")
# источник и описание:
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html

glimpse(t)

# Указание того, какие переменные факторные
t <- mutate(t, sex=as.factor(sex), pclass=as.factor(pclass), survived=as.factor(survived))

summary(t)

# когда объясняемая переменная качественная имеет смысл смотреть следующие графики:

# 1) одна качественная переменная объясняет другую качественную

# 2) количественная объясняет качественную

mosaic(data=t,~sex+pclass+survived, shade=TRUE) # много мужчин из третьего класса погибло, практически все женщины из первого и второго класса выжили

qplot(data=t,x=survived,y=age, geom = "violin") # распределение по возрастам среди выживших и не выживших одинаковое

qplot(data=t,x=survived,y=age, geom = "boxplot")

#наложим функции плотности для выживших и не выживших на одном графике
qplot(data=t, x=age, y=..count.., fill = survived, geom = "density", position = "stack")

# график сглаженной условной вероятности
qplot(data=t, x=age, y=..count.., fill = survived, geom = "density", position = "fill")

# Оценко логит- и пробит-моделей

m_logit <- glm(data=t, survived~sex+age+pclass+fare,
               family = binomial(link = "logit"), x=TRUE)

m_probit <- glm(data=t, survived~sex+age+pclass+fare,
               family = binomial(link = "probit"), x=TRUE)

summary(m_logit)
summary(m_probit)

# интерпретация непрерывной пременной (возраст) и прерывной (пол) отличается
# инструкция в видео 7.2.2

# ковариационная матрица
vcov(m_logit)

# прогнозирование и оценка предельных эффектов

# создадим искуственный набор данных для прогнозирование
# например вероятность выжить для мужчин всех возможных возрастов из второго класса
 
new_data <- data.frame(age=seq(from=5, to=100, length=95), sex="male", pclass="2nd", fare=100)

head(new_data)

pr_logit <- predict(m_logit,new_data, se=TRUE)

new_data_pr <- cbind(new_data,pr_logit)

head(new_data_pr)

# fit = предсказанная скрытая переменная (Y*) , se.fit - стандартная ошибка этой переменной

# переход от спрогнозированных скрытых переменных к прогнозу вероятности выжить 

new_data_pr <- mutate(new_data_pr, prob=plogis(fit),
                      left_ci=plogis(fit-1.96*se.fit),
                      right_ci=plogis(fit+1.96*se.fit))

head(new_data_pr)

# визуализация результатов прогнозирования вероятности выжить в зависимости от возраста с доверительным интервалом

qplot(data=new_data_pr, x=age, y=prob, geom="line") + geom_ribbon(aes(ymin=left_ci, ymax=right_ci), alpha=0.2)

t2 <- select (t, sex, age, pclass, survived, fare) %>% na.omit()

m_logit2 <- glm(data = t2, survived ~ sex+age,
                family = binomial(link = "logit"), x=TRUE)

# сравнение более сложной и простой модели с помощью теста отношения правдоподобия 
lrtest(m_logit,m_logit2)

# p-value = 2.2e-16 *** - очень мало, следовательно H:0 - финансовые переменные не оказывают влияния на вероятность выжить отвергаются. 

# Оценка предельных эффектов (надо уточнение для какого)

# для среднестатистического пассажира (средний возраст, пол, класс)
maBina(m_logit)

# усреднить по всем пассажирам
maBina(m_logit, x.mean = FALSE)

# тонкости интерпретации 

# попробуем МНК модель, хотя это не верно

m_ols <- lm(data=t, as.numeric(survived)~sex+age+pclass+fare)
summary (m_ols)

pr_ols <- predict(m_ols, new_data)
head(pr_ols)

# ROC-кривая: выживет конкретный человек или погибнет - можно установить любой порог отсечения
# выбор порогапрогнозирования влияет на вероятность ошибок

pr_t <- predict(m_logit,t,se=TRUE)
t <- cbind(t,pr_t)
t <- mutate (t, prob=plogis(fit))

select(t,age,survived,prob)

# на основании этих данных переберем пороги отсечения (на вероятность неправильной классификации пассажирова)

roc.data <- roc(t$prob, t$survived)
str(roc.data)

# cutoffs - пороги отсечения за которыми мы признаем пассажира выжившим или нет
# fpr - доля неверно классифицированных невыживших пассажиров (т.е. они не выжили, но их рассматривают как выживших)
# tpr - доля верно классифицированных выживших

qplot (x=roc.data$cutoffs, y=roc.data$tpr, geom="line")

qplot (x=roc.data$cutoffs, y=roc.data$fpr, geom="line")

qplot (x=roc.data$fpr, y=roc.data$tpr, geom="line")


# Тестовое задание 

t <- read.csv("titanic3.csv")

t <- mutate(t, sex=as.factor(sex), pclass=as.factor(pclass), survived=as.factor(survived))

m_logit4 <- glm(data=t, survived~sex+age+fare+sibsp,
               family = binomial(link = "logit"), x=TRUE)

summary(m_logit4)

t <- mutate(t, sex=as.numeric(sex))
t <- dplyr::select(t, -sex_sq)
t <- dplyr::select(t, --sex_sq)
str(t)
t <- mutate(t, age_sq = age^2)

m_logit5 <- glm(data=t, survived~sex+age+age_sq+fare+sibsp,
                family = binomial(link = "logit"), x=TRUE)
summary(m_logit5)

x_0 = 0.0663291/(2*0.0007399) 
x_0


t <- mutate(t, parch=as.factor(parch))
m_logit6 <- glm(data=t, survived~sex+age+age_sq+fare+sibsp+parch,
                family = binomial(link = "logit"), x=TRUE)
summary(m_logit6)

t6<-qt(0.975, df=1303)
t6 # 1.96

lft = -0.4333768 - (1.96*0.1046810)
lft

m_logit5 <- glm(data=t, survived~sex+age+age_sq+fare+sibsp,
                family = binomial(link = "logit"), x=TRUE)

summary (m_logit5)

newdata2 <- data.frame(age=50, age_sq=2500, sex="male", fare=200, sibsp=2)

head(newdata2)

pr_logit5 <- predict(m_logit5,newdata2, se=TRUE)
pr_logit5


newdata_pr2 <- cbind(newdata2,pr_logit5)

head(newdata_pr2)

newdata_pr2 <- mutate(newdata_pr2, prob=plogis(fit),
                      left_ci=plogis(fit-1.96*se.fit),
                      right_ci=plogis(fit+1.96*se.fit))
head(newdata_pr2)

# предельный эффект среднестатистического индивида

m_logit6 <- glm(data=t, survived~sex+age+age_sq+fare+sibsp+parch,
                family = binomial(link = "logit"), x=TRUE)

maBina(m_logit6)

d <- dplyr::select(t, sex, age, age_sq, fare, sibsp, parch, survived)
d <- d %>% na.omit()

d <- mutate(d, fare_sq = fare^2)

m_logit6_2 <- glm(data=d, survived~sex+age+age_sq+fare+fare_sq+sibsp,
                  family = binomial(link = "logit"), x=TRUE)


m_logit4_2 <- glm(data=d, survived~sex+age+age_sq+sibsp,
                  family = binomial(link = "logit"), x=TRUE)

lrtest(m_logit6_2,m_logit4_2)

#______

d2 <- dplyr::select(t, sex, age, age_sq, fare, sibsp, survived)
d2 <- d2 %>% na.omit()

m_logit5_2 <- glm(data=d2, survived~sex+age+age_sq+fare+sibsp,
                  family = binomial(link = "logit"), x=TRUE)

pr_d2 <- predict(m_logit5_2,d2,se=TRUE)

d2 <- cbind(d2, pr_d2)
d2 <- mutate(d2,prob=plogis(fit))
d2 <- select(d2,age,survived,prob)
roc.data <- roc(d2$prob, d2$survived)

str(roc.data)

head(roc.data)
# 3

se <- ((-25^-1)*-1)^1/2
se

p <- pnorm(1.5, mean = 0, sd = 1, lower.tail = TRUE)
p

# 12
str(t)

m_logit12 <- glm(data=t, survived~age+sex+fare+sibsp+parch,
               family = binomial(link = "logit"), x=TRUE)

summary (m_logit12)

# 13

t <- mutate(t, age_sq = age^2)

m_logit13 <- glm(data=t, survived~age+age_sq+sex+fare+sibsp+parch,
                 family = binomial(link = "logit"), x=TRUE)

summary (m_logit13)

dead_age <- 0.0712165/(2*0.0007992) 
dead_age

# 14

m_logit14 <- glm(data=t, survived~age+age_sq+sex+fare+sibsp,
                 family = binomial(link = "logit"), x=TRUE)
summary(m_logit14)
t5<-qt(0.975, df=1304)
t5 # 1.96
lft_sbsp = -0.4612073 - (1.96*0.1025660)
lft_sbsp

# 15

head(t)
m_logit15 <- glm(data=t, survived~age+age_sq+sex+fare+sibsp,
                 family = binomial(link = "logit"), x=TRUE)

newdata15 <- data.frame(age=50, age_sq=2500, sex="male", fare=200, sibsp=2)
predict(m_logit15, newdata = newdata15, type = "response", se.fit = TRUE)

# 16

m_logit16 <- glm(data=t, survived~age+age_sq+sex+fare+sibsp,
                 family = binomial(link = "logit"), x=TRUE)

newdata16 <- data.frame(age=30, age_sq=900, sex="male", fare=200, sibsp=2)
predict(m_logit16, newdata = newdata16, type = "response", se.fit = TRUE)

lft_surv_hat = 0.4343837 - (1.96*0.09047134)
lft_surv_hat

# 17 предельный эффект братьев/сестер для среднестатистического индивида

m_logit16 <- glm(data=t, survived~age+age_sq+sex+fare+sibsp+parch,
                 family = binomial(link = "logit"), x=TRUE)

maBina(m_logit16)

# 18

# 19

d <- dplyr::select(t, sex, age, age_sq, fare, sibsp, survived)
d <- d %>% na.omit()

m_logit19 <- glm(data=d, survived~age+age_sq+sex+fare+sibsp,
                 family = binomial(link = "logit"), x=TRUE)

pr_d <- predict(m_logit19,d,se=TRUE)
head (pr_d)
d <- cbind(d,pr_d)
d <- mutate (d, prob=plogis(fit))

d$surv_pr <- ifelse(d$prob >= 0.7, 1, 0)
d$true_prediction <- ifelse(d$surv_pr == d$survived, 1, 0)


d2<-d %>% filter_all(all_vars(d$survived == "1"))
sum(d2$true_prediction)

231/427

# 20

m_logit20 <- glm(data=t, survived~age+age_sq+sex+fare+sibsp,
                 family = binomial(link = "logit"), x=TRUE)

summary(m_logit20)

0.1613292^2
