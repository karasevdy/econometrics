library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
install.packages("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

h <- d_d
glimpse(h)
qplot(data=h, carat, price)
bg  <- qplot(data=h, log(carat), log(price))
bg + geom_hex()

f <- read.csv("flats_moscow.txt", sep = "\t", header = TRUE, dec=".")
glimpse(f)

qplot(data = f, totsp, price)
table(f$brick)
qplot(data=f, log(totsp), log(price))

mosaic(data=f, ~ walk+brick+floor, shade=TRUE)

f<-mutate_each(f,"fctr", walk, brick, floor, code)
f <- mutate_each(f, "factor", walk, brick, floor, code)

str(f)
qplot(data = f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position = position_dodge2(width = 1))
qplot(data=f, log(price), fill=brick, ggeom_density)

help("position_dodge")

g2 <- ggplot(f, aes(x = log(price))) + geom_density(color = 4, fill = 4, alpha = 0.25)

# добавление фарсеток (разъбиение на 4 графика по двум качественным переменным "walk" и "floor")
g2+facet_grid(walk~floor)

ggplot(data =f, aes(x = log(price)) + geom_density())
f %>% ggplot(x = log(price))+ geom_density()

# Модель с дамми переменными
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))
# Альтернативная запись model_2 вместо log(totsp)+brick+brick:log(totsp) - brick*log(totsp)
model_2b <- lm(data=f, log(price)~brick*log(totsp))
mtable(model_2,model_2b)

sjp.lm(model_2)
help(sjPlot)
??sjp
plot_model(model_2)

# Прогнозирование с дами-переменными
nw<- data.frame(totsp=c(60,60), brick =factor(c(1,0)))
nw

predict(model_2, newdata=nw)
# Нужно экспанинцировать,т.к. до этого брали логарифм

exp(predict(model_2, newdata=nw))

# Построим 95% доверительный интервал для среднего (средней квартиры по москве)

predict(model_2, newdata=nw, interval = "confidence")

exp(predict(model_2, newdata=nw, interval = "confidence"))

# Построим 95% предективный доверительный интервал для конкретной квартиры 

predict(model_2, newdata=nw, interval = "prediction")

exp(predict(model_2, newdata=nw, interval = "prediction"))

# Тест Вальда (H_0 model_0 верна, а не model_1)
waldtest(model_0,model_1)
# т.к. p<0,05, то H_0 отвергается 

waldtest(model_1,model_2)
# H_0: model_1 better than model_2 также отвергается, поскольку p<0,05

qq0<- qplot(data=f,log(totsp),log(price))
qq0 + stat_smooth(method = "lm")

# добавили разделение на два графика в зависимости от переменной "walk" пешая доступность до метро
qq0 + stat_smooth(method = "lm") + facet_grid(~walk)

# раскрасим в зависимости от того кирпичный ли дом
qq0 + aes(col=brick) + stat_smooth(method = "lm") + facet_grid(~walk)

structure(f)

f$nonbrick <-memisc::recode(f$brick, 1 <- 0, 0 <- 1)
structure(f)

model_wrong <- lm(data=f, log(price)~log(totsp)+brick+nonbrick)
summary(model_wrong)

# Добавить AIC (критерий Акаики) и BIC (критерий ) и прочие доп статистики в mtable
mtable(model_0, model_1, model_2, summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "p", "Log-likelihood", "Deviance", "AIC","BIC", "N")) 

# Тест Рамсея (на пропущенные переменные в регрессии)

resettest(model_2)


df <- diamonds
describe(df)

str(df)
m_d <- lm(data=df, log(price)~log(carat))
summary(m_d)

m_d1 <- lm(data=df, price~carat)
summary(m_d1)
table(df$clarity)

# дамми-переменные для уровней cut
table(df$cut)

df$cut_Fair <- ifelse(df$cut == "Fair", 1, 0)
df$cut_Good <- ifelse(df$cut == "Good", 1, 0)
df$cut_VeryGood <- ifelse(df$cut == "Very Good", 1, 0)
df$cut_Premium <- ifelse(df$cut == "Premium", 1, 0)

str(df)

df$clarity_I1 <-memisc::recode(df$clarity, I1 <- 1, SI2 <- 0, SI1 <- 0, VS2 <- 0, VS1 <- 0, VVS2 <- 0, VVS1 <- 0, IF <- 0)
str(df)

m_cut <- lm(data = df, price~carat+depth+cut_Fair+cut_Good+cut_VeryGood+cut_Premium)

mtable(m_cut, summary.stats=c("AIC"))

m_d1 <- lm(data=df, price~carat)
m_d2 <- lm(data=df, price~carat+depth)
m_d3 <- lm(data = df, price~carat+depth+cut_Fair+cut_Good+cut_VeryGood+cut_Premium)

mtable(m_d1,m_d2,m_d3, summary.stats=c("R-squared", "adj. R-squared", "AIC", "BIC"))

RSS_r<-deviance(m_d1)
RSS_r
RSS_ur <- deviance(m_d2)
RSS_ur

ch <-  RSS_r-RSS_ur/1
zn <- RSS_ur/(53940-3)
53940-3

ch/zn

resettest(m_d1)

# Работающие графики

qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_wrap(~clarity)

qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_grid(~clarity)

qplot(data = df, log(price), fill=clarity, geom = "density")

df$clarity_I1 <- ifelse(df$clarity == "I1", 1, 0) #1
df$clarity_SI2 <- ifelse(df$clarity == "SI2", 1, 0) #2
df$clarity_SI1 <- ifelse(df$clarity == "SI1", 1, 0) # 3
df$clarity_VS2 <- ifelse(df$clarity == "VS2", 1, 0) # 4
df$clarity_VS1 <- ifelse(df$clarity == "VS1", 1, 0) # 5
df$clarity_VVS2 <- ifelse(df$clarity == "VVS2", 1, 0) # 6
df$clarity_VVS1 <- ifelse(df$clarity == "VVS1", 1, 0) #7

m_clar <- lm(data = df, price~carat+clarity_I1+clarity_SI2+clarity_SI1+clarity_VS2+clarity_VS1+clarity_VVS2+clarity_VVS1)
summary(m_clar)
m_d3 <- lm(data = df, price~carat+depth+clarity_I1+clarity_SI2+clarity_SI1+clarity_VS2+clarity_VS1+clarity_VVS2+clarity_VVS1)

m_d2 <- lm(data=df, price ~ carat+depth)
mtable(m_di, summary.stats=c("AIC"))

resettest(m_d2)

mtable(m_d1, m_d2, m_d3, summary.stats=c("BIC"))

help("mutate")

df <- mutate_each(df, "factor", clarity)
glimpse(df)

demo <- read.csv("demo.csv",header=TRUE,sep=",")
str(demo)
table(demo$podkategoria)

# Как писать функции в R (лекция 5)
f <- function(x) { 
  res <- x^2 
  return(res)
}

f(3)
f(-1)

fs <- function(x, stepen=2) { 
  res <- x^stepen 
  return(res)
}

fs(4)
fs(2,stepen=5)

cars

d <- cars
d[1,2] <- NA
d[3,1] <- NA

d
# функция, которая работае с пропущенными значениям
is.na(d)
sum(is.na(d))

na_perc <- function(d) {
  res <- sum(is.na(d))/nrow(d)/ncol(d)
  return(res)
}

na_perc(d)

na_perc <- function(d) {
  if(!is.data.frame(d)) stop("d should be a data.frame")
  res <- sum(is.na(d))/nrow(d)/ncol(d)
  return(res)
}

x <- c(5,6,7)
na_perc (x)
na_perc(HCI)

# как писать цикл в R

for(i in 5:10) {
  k <- i^2
  cat("i=",i," i^2=",k,"\n")
}

# циклы помогают с чтением данных разделенных на разные файлы с таблицами 
# rbind (старый массив, подклеиваемый с низу новый) - подклейска строчек снизу

all_data <- NULL
for(fname in c("file01.csv","file02.csv")) {
  temp <- read.csv(fname)
  all_data <- rbind(all_data,temp)
}

head(all_data)

