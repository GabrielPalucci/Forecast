

#Biblioteca

library(pacman)
library(forecast)

p_load("forecast", "urca", "lmtest", "seasonal",
       "seasonalview", "ggplot2", "tidyverse", "cowplot")



#Adicionando dados Vaz?o



getwd()
Data <- read.csv("Lad.csv", header = T, sep = ",")


names(Data)
Data


Data %>% group_by(Data) %>% count()

Data_1 <- select(Data, Dados)


#Trasnformando em Serie Temporal


ts<- ts(Data_1, start = c(1980,1), end = c(2019,9), frequency = 12)

autoplot(ts)
print(ts)


# Arrumando dados NA

library(imputeTS)


which(is.na(ts))

ggplot_na_distribution(ts) +
  theme_minimal(base_size = 10) +
  labs(title = "Distribui??o dos Valores Sem Informa??es", 
       subtitle = "Serie Temporal - L?dario",
       caption = "Esta??o 66825000",
       x = "Lag",
       y = "Vaz?o")

ggsave("Na.png", dpi = 400)


ts <-  na.interpolation(ts, option = "linear")

autoplot(ts)



#Trasforma??es analise box cox para diminuir as flutua??es

autoplot(log(ts))

BoxCox.lambda(ts)
ts_mod <- autoplot(BoxCox(ts, lambda = 0.23))
plot(ts_mod)


#Analises explotat?ria

Data %>%  filter(Dados < 150) %>%  view()


view(ts)
summary(ts)


skimr::skim(ts)

Box.test(ts, type = "Ljung-Box") #Autocorrela??o lang-1



#Decompondo as estruturas da TS 

ts_decom <-decompose(ts)

autoplot(ts_decom) +
  theme_minimal(base_size = 10) +
  labs(title = "Decomposi??o da Serie Temporal", x = "Anos",
       caption = "Esta??o 66825000")


ggsave("decom.png", dpi = 400)


ts_decom$seasonal #Sazonalidade
ts_decom$trend    #Tendencia
ts_decom$random   #Ruido



ggseasonplot(ts)



# Teste para saber se existe estacionaridade

library(aTSA)
library(tseries)
library(Kendall)
library(trend)


cor.test(co2)
Kendall::MannKendall(ts)
sens.slope(ts)



length(ts)

stationary.test(ts ,method = c("adf", "pp", "kpss"))



ts_est <- ur.kpss(ts)
ts_adf <- adf.test(ts, output = FALSE)
ts_pp <- pp.test(ts, output = FALSE)



#PACF

ggPacf(ts, lag.max = 15) +
      labs(title = "") +
      theme_minimal()



#diffs

ts_est2 <- ndiffs(ts, test = "pp") #2
ts_est2




#Split dos dados
# Dadados divididos em treino de 1990 a 2008



ts_train  <- subset(ts, end = length(ts) - 48)

ts_train <- window(ts, start  = c(1980,1), end = c(2015,9))
ts_test <-  window(ts, start  = c(2015,9), end = c(2019,9))

autoplot(ts_train)
autoplot(ts_test)


plot(ts_train)
lines(ts_test)

  

                          # Raalizando Arima





fit1 <- auto.arima(ts_train, trace = T, stepwise = T,approximation = F,  lambda = 0.23)

checkresiduals(fit1)
shapiro.test(fit1$residuals)
var(fit1$residuals)
mean(fit1$residuals)


acfAr<- ggAcf(fit1$residuals) +
        theme_minimal(base_size = 10) +
        labs(title = "ACF Modelo ARIMA")
    



fit1 %>%  forecast(h= 48) %>%  autoplot()
Ts_prev <- fit1 %>%  forecast(h= 48)

max(Ts_prev$upper)
min(Ts_prev$lower)


plot(Ts_prev)
lines(ts, col ="black")


names(Ts_prev)

A <- autoplot(Ts_prev) +
  xlim(2014,2020) +
  geom_line(data = ts, aes(x = x, y= ts), size = 0.5, linetype = "dashed") +
  theme_minimal(base_size = 9) + 
  labs(title = "Modelo Arima (1.0.2).(0.1.2)", x = "Ano", y = "Cota (cm)")
  
A

ac <- accuracy(Ts_prev, ts_test)
ac

ac1 <-broom::tidy(as.tibble(ac))







                                 #ETS




fit2<- ets(ts_train, lambda = 0.23)
checkresiduals(fit2)
shapiro.test(fit2$residuals)
var(fit1$residuals)
mean(fit1$residuals)


Ts_prev1<- forecast(fit2, h = 48)
autoplot(Ts_prev1)  

max(Ts_prev1$upper)
min(Ts_prev1$lower)


ac1 <- accuracy(Ts_prev1, ts)
ac1

plot(Ts_prev1) 
lines(ts, col ="black")

E <-autoplot(Ts_prev1) +
  xlim(2014,2020) +
  geom_line(data = ts, aes(x = x, y= ts), size = 0.5, linetype = "dashed") +
  theme_minimal(base_size = 9) + 
  labs(title = "ETS(A.Ad.A)", x = "Ano", y = "Cota(cm)")

E

acfETS <- ggAcf(fit2$residuals) +
  theme_minimal(base_size = 10) +
  labs(title = "ACF Modelo ETS")



                        # Rede Neural



Ts_modred$residuals

Ts_modred <- nnetar(ts_train, repeats = 100)
Ts_previ3 <- forecast(Ts_modred, h = 48)


R <-autoplot(Ts_previ3) +
  xlim(2014,2020) +
  geom_line(data = ts, aes(x = x, y= ts), size = 0.5, linetype = "dashed") +
  theme_minimal(base_size = 10) + 
  labs(title = "ETS(A.Ad.A)", x = "Ano", y = "Vaz?o")


checkresiduals(Ts_previ3)

ac3 <- accuracy(Ts_previ3, ts)


acfRN <- ggAcf(Ts_modred$residuals) +
  theme_minimal(base_size = 10) +
  labs(title = "ACF Modelo ETS")




#Salvando Modelos


pmod <- plot_grid(A, E, nrow = 2, ncol = 1)
ggsave("plotmod.png",  dpi = 300, scale = T)



#Salvando Res?duos ACF

pred <- plot_grid(acfAr, acfETS, nrow = 1, ncol = 2)
ggsave("plotred.png",  dpi = 300, scale = T)




                      #Regress?o

mean(ts)

library(ggplot2)
library(ggpmisc)

my.formula <- y ~ x


autoplot(ts) +
  geom_smooth(method = "lm", se = FALSE , color = "red",  formula =  my.formula) +
  annotate("text", label = eq, x = 2015,  y = 600) +
  labs(x = "Tempo (meses)", 
       y = "Cota (cm)",
       caption = "Esta??o 66825000")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal(base_size = 10)
  
abline(h = 325.9)
  

ggsave("Regre.png", dpi = 400)





ts_reg <- tslm(ts ~ trend, data = ts) 
coeff <- coefficients(ts_reg)
eq = paste0("y = ", round(coeff[2],3), "x + ", round(coeff[1],1))


ts_reg$fitted.values
checkresiduals(ts_reg)
shapiro.test(ts_reg$fitted.values)


accuracy(ts_reg)









#Time series cross validation


e <- tsCV(ts, forecastfunction = naive, h = 8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)


# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

naive




#Obs

# xreg ? pra adicioanr vari?vel idependente para realizar o modelo e previs?es
# ao realizar previsoes utilziando ela ? necess?rio a pre exist?ncia do valor

# Trasformando Serie Nao Estacionaria - EM - Estacionaria
