

InTar <- read_excel("IT_bef_aft.xlsx")
InTar <-plm::pdata.frame(InTar, index=c("cn_id","Year"))
InTar
head(InTar)

did1 <- lm(data = InTar, Infl_aft~Infl_bef*IT)
summary(did1)           

q <- qplot(data = InTar, log(Infl_bef), log(Infl_aft))

Intrg <- read_excel("IT.xlsx")
Intrg <- plm::pdata.frame(Intrg, index=c("cn_id","Year"))
head(Intrg)
str(Intrg)
Intrg4 <- Intrg %>% filter(Inflation > 0)
Intrg5 <- mutate(Intrg4, L_Inflation=log(Inflation))

Intrg 


Intrg2 <- dplyr::select(Intrg, Country, cn_id, dev, IT, Year, L_Inflation)
Intrg3 <- Intrg2 %>% filter_all(all_vars(!is.na(.)))

GMM1 <- pgmm(L_Inflation ~ lag(L_Inflation, 1:2) * lag(IT, 1:1) * lag(dev, 1:1)
             |lag(L_Inflation, 2:10), data = Intrg5,
             effect = "twoways",
             index = c ("cn_id","Year"),
             model = "twosteps",
             transformation = "ld",
             fsm = "full",
             subset = sample == 1)

GMM2 <- pgmm (L_Inflation ~ lag(L_Inflation, 1:2) | lag(L_Inflation, 2:10), data = Intrg5,
              effect = "twoways",
              index = c ("cn_id","Year"),
              model = "twosteps")

GMM3 <- pgmm(L_Inflation ~ lag(L_Inflation, 1:2) + lag(IT, 1:1) + lag(dev, 1:1) + lag(L_Inflation, 1:2) : lag(IT, 1:1)
             |lag(L_Inflation, 2:10), data = Intrg5,
             effect = "twoways",
             index = c ("cn_id","Year"),
             model = "twosteps",
             transformation = "ld",
             fsm = "full",
             subset = sample == 1)

GMM5 <- pgmm(L_Inflation ~ lag(L_Inflation, 1:2)
             |lag(L_Inflation, 2:10), data = Intrg5,
             effect = "twoways",
             index = c ("cn_id","Year"),
             model = "twosteps",
             transformation = "ld",
             fsm = "full",
             subset = sample == 1)

GMM5 <- pgmm(L_Inflation ~ lag(L_Inflation, 1:2)
             |lag(L_Inflation, 2:10), data = Intrg5,
             effect = "twoways",
             index = c ("cn_id","Year"),
             model = "twosteps",
             transformation = "d",
             fsm = "full",
             subset = sample == 1)

summary(GMM1)
summary(GMM3)
summary(GMM5)

Intrg6 <- augment(GMM5, Intrg5)
m <- matrix(data=fitted.values(GMM5), nrow=6, ncol=134)
m1 <- matrix(data=fitted.values(GMM5), nrow=10, ncol=134)
m2 <- matrix(data=fitted.values(GMM5), nrow=15, ncol=134)

m3 <- t(m2)
m3

m []
m1

Intrg6 <- mutate(Intrg5, L_Infl_hat=fitted.values(GMM5))

L_Infl_hat<-data.frame(fitted.values(GMM5))
L_Infl_hat2<-data.frame(fitted.values(GMM5))

str(L_Infl_hat)
head (L_Infl_hat)
tail(L_Infl_hat)

m4 <-  t(L_Infl_hat)
m4
str(m4)
exm <- c(m4[1:2,1:2])
exm

c_1.1 <- c(m4[1:19,8])
c_1 <- c(m4[1:19,1:7])
c_2.1 <- c(m4[20:102,8])
c_2 <- c(m4[20:102,1:7])
c_3.1 <- c(m4[103:112,8])
c_3 <- c(m4[103:112, 1:7])
c_4.1 <- c(m4[113-134],8)
c_4 <- c(m4[113-134],1:7)

V_I_h1 <- c(c_1.1,c_1,c_2.1,c_2,c_3.1,c_3,c_4.1,c_4)
V_I_h1
Intrg6 <- mutate(Intrg5, L_Infl_hat=V_I_h1)


v_1.1 <- rep(NA,19)
v_1 <- c(m4[1:19,9:15])
v_2.1 <- rep(NA,83)
v_2 <- c(m4[20:102,9:15])
v_3.1 <- rep(NA,10)
v_3 <- c(m4[103:112,9:15])
v_4.1 <- rep(NA,22)
v_4 <- c(m4[113-134],9:15)

V_I_h2 <- c(v_1.1,v_1,v_2.1,v_2,v_3.1,v_3,v_4.1,v_4)

V_I_h2

IT_be_af <- read_excel("IT_bef_aft.xlsx")
str(IT_be_af)
IT_be_af2 <- IT_be_af %>% filter(Infl_bef > 0, Infl_aft > 0, IT==1)
IT_be_af3 <- mutate(IT_be_af2, L_Infl_bef=log(Infl_bef), L_Infl_aft=log(Infl_aft))
IT_be_af3 <-plm::pdata.frame(IT_be_af3, index=c("cn_id","Year"))

disco1 <- lm(data = IT_be_af3, Infl_aft ~ Infl_bef + I_h_th_av)
summary (disco1)

disco2 <- lm(data = IT_be_af3, Infl_aft ~ Infl_bef + In_time)
summary (disco2)

disco3 <- lm(data = IT_be_af3, Infl_aft ~ Infl_bef + In_time + I_h_th_av)
summary (disco3)

disco4pl <- lm(data = IT_be_af3, Infl_bef ~ Infl_aft + I_h_th_av)
summary (disco4pl)

disco5pl <- lm(data = IT_be_af3, Infl_bef ~ Infl_aft + In_time)
summary (disco4pl)

gqtest(disco1, order.by = ~ Infl_bef, data = IT_be_af3, fraction = 0.2)
gqtest(disco2, order.by = ~ Infl_bef, data = IT_be_af3, fraction = 0.2)

res1 <- dwt(disco1)
res2 <- dwt(disco2)


res1$dw
res2$dw
