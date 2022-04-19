library(readxl)
dados <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/Residuos Pasqualini, Hobus e Thomé/resid_direita_riodoSul.xlsx")


fit1 <- lm(J.Thome2020 ~ M.Hobus2004, data=dados)
summary(fit1)
fit2 <- lm(Pasqualini ~ M.Hobus2004, data= dados)
summary(fit2)


library(huxtable)
modelos <- list(fit1, fit2)
huxreg(modelos)


library(tidyverse)


# diagnóstico do modelo do thome
ggplot(data = fit1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = fit1, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = fit1, aes(sample = .resid)) +
  stat_qq()

# diagnóstico do modelo do Pasqualin

ggplot(data = fit2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = fit2, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = fit2, aes(sample = .resid)) +
  stat_qq()


# plot dos residuos de thomé
dados$predicted <- predict(fit1)   # Save the predicted values
dados$residuals <- residuals(fit1) # Save the residual values

h <- ggplot(dados, aes(x = M.Hobus2004, y =J.Thome2020)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +        
  geom_segment(aes(xend = M.Hobus2004, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()
h

h + geom_text(label=dados$Bairro)

# do pasqualini

dados$predicted2 <- predict(fit2)   # Save the predicted values
dados$residuals2 <- residuals(fit2) # Save the residual values

g <- ggplot(dados, aes(x = M.Hobus2004, y =Pasqualini)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +       
  geom_segment(aes(xend = M.Hobus2004, yend = predicted2), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals2), size = abs(residuals2))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted2), shape = 1) +
  theme_bw()
g 


g + geom_text(label=dados$Bairro)

# comparadores de correlação
dados$predict_Thome <- dados$predicted
dados$predict_Pasqualin <- dados$predicted2

cor.test(dados$predict_Thome, dados$J.Thome2020)


cor.test(dados$predict_Pasqualin, dados$Pasqualini)


# comparadores de plots

summary(dados$J.Thome2020)

plot(dados$predict_Thome, dados$J.Thome2020) 

plot(dados$predict_Pasqualin, dados$Pasqualini)


# incluir população - pq correlacionou aqui:

thome <- subset(dados, select=c(J.Thome2020, M.Hobus2004, pop))
pasqualini <- subset(dados, select=c(Pasqualini, M.Hobus2004, pop)) 
  

#Usar método de verificação bayesiano para melhor modelo
#primeiro thome
library(BAS)
model<-bas.lm(J.Thome2020~.,data=thome, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(model, which=1, add.smooth=F)
#2. cumulative probability
plot(model, which=2)
#PIP
plot(model, which = 4, ask=FALSE, caption="", sub.caption="")
#model rank
image(model, rotate = F)
summary(model)


#pasqua

model<-bas.lm(Pasqualini~.,data=pasqualini, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(model, which=1, add.smooth=F)
#2. cumulative probability
plot(model, which=2)
#PIP
plot(model, which = 4, ask=FALSE, caption="", sub.caption="")
#model rank
image(model, rotate = F)
summary(model)


# Extract coefficients
coef.ZS=coef(model)

# Po1 and Po2 are in the 5th and 6th columns in UScrime
par(mfrow = c(1,2))
plot(coef.ZS, subset = c(2:3), 
     col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)


# UM TIQUINHO MAIS DE PASQUALINI
# OS BAIRROS MAIS PREVISÍVEIS

fit3 <- lm(Pasqualini ~ M.Hobus2004 + pop, data=pasqualini)
summary(fit3)

dados$previsao <- fit3$fitted.values
dados$residuo <- fit3$residuals

library(knitr)
library(kableExtra)

b5 <- dados %>% 
  dplyr::select(Bairro, Pasqualini, previsao, residuo) %>% 
  arrange(Bairro)
b5 %>%
  kbl(caption = "Acerto por bairro") %>%
  kable_classic(full_width = F, html_font = "Garamond")

# comparador final
modelo <- lm(J.Thome2020 ~ M.Hobus2004+pop, data=dados)
modelo2 <- lm(J.Thome2020 ~ pop, data=dados)
modelosx <- list(fit1, modelo, modelo2)
huxreg(modelosx)



modelo3 <- lm(Pasqualini ~ pop, data=dados)
modelosy <- list(fit2, modelo3, fit3)
huxreg(modelosy)


library(coefplot)
coefplot(modelo, intercept=FALSE, interactive=TRUE)
coefplot(fit3, intercept=FALSE, interactive=TRUE)

#comparadores de todos os modelos com todas as dep (correlation)
cor(dados$J.Thome2020, fit1$fitted.values)#thomé por Hobus
cor(dados$J.Thome2020, modelo2$fitted.values)#Thomé por população
cor(dados$J.Thome2020, modelo$fitted.values)#Thomé por pop+Hobus

cor(dados$Pasqualini, fit2$fitted.values)#Pasqualini por Hobus
cor(dados$Pasqualini, modelo3$fitted.values)#Pasqualini por população
cor(dados$Pasqualini, fit3$fitted.values)#pasqualini por Pop+Hobus


# grafico lindo para dois tipos de população(mais e menos populosos)

dados$tamanho_pop <- ntile(dados$pop, 2)
dados$tamanho_pop <- as.factor(dados$tamanho_pop)
levels(dados$prop_formados) <- c('Menor', 'Maior')

model22 <- lm(Pasqualini ~ M.Hobus2004 + tamanho_pop  + M.Hobus2004:tamanho_pop, data=dados)
summary(model22)
summary(dados$pop)

plot(dados$M.Hobus2004[dados$pop>3002], 
     dados$Pasqualini[dados$pop>3002], 
     col = "blue", 
     ylim = c(18, 40), xlim = c(33, 65), 
     xlab = "Hobus2004", ylab = "Pasqualini", 
     main = "Pasqualini vs. Hobus04,tamanho populacional")
points(dados$M.Hobus2004[dados$pop<3002], 
       dados$Pasqualini[dados$pop<3002],
       col = "red", pch = 16)
legend(32, 35, 
       legend = c("Menos Populosos", "Mais populosos"), 
       col = c("blue", "red"), 
       pch = c(11, 11), bty = "n")

# prop menor 1 -> a= intercp , b = neves14
#propp maior a=interco+9.43, b = neves-interatcie)

abline(a = 4.52, b = 0.4227,
       col = "blue", lwd = 1.7)
abline(a = 4.52-3.81, b = 0.4227+0.2129,
       col = "red", lwd = 1.7)

# inverter para nível hobus

dados$apoio_Hobus <- ntile(dados$M.Hobus2004, 2)
dados$apoio_hobus <- as.factor(dados$apoio_Hobus)
levels(dados$apoio_Hobus) <- c('Menor', 'Maior')

model22 <- lm(Pasqualini ~ pop + apoio_Hobus  + pop:apoio_hobus, data=dados)
summary(model22)
summary(dados)

plot(dados$pop[dados$apoio_Hobus == "1"], 
     dados$Pasqualini[dados$apoio_Hobus =="1"], 
     col = "blue", 
     ylim = c(11, 39), xlim = c(400, 4800), 
     xlab = "População Censo 2010", ylab = "Pasqualini", 
     main = "Pasqualini vs. Hobus04,tamanho populacional")
points(dados$pop[dados$apoio_Hobus == "2"], 
       dados$Pasqualini[dados$apoio_Hobus =="2"],
       col = "red", pch = 16)
legend(600, 40, 
       legend = c("Menos Votos em Hobus2004", "Mais Votos em Hobus2004"), 
       col = c("blue", "red"), 
       pch = c(11, 11), bty = "n")

# prop menor 1 -> a= intercp , b = neves14
#propp maior a=interco+9.43, b = neves-interatcie)

abline(a = 8.1, b = 0.003687,
       col = "blue", lwd = 1.7)
abline(a = 8.1+7.91, b = 0.003687+0.001257,
       col = "red", lwd = 1.7)
# thomé pop

dados$predicted <- predict(modelo2)   # Save the predicted values
dados$residuals <- residuals(modelo2) # Save the residual values

h <- ggplot(dados, aes(x = pop, y =J.Thome2020)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +        
  geom_segment(aes(xend = pop, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()
h

h + geom_text(label=dados$Bairro)
