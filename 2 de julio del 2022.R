
nrep <- 10  # Número de repeticiones
n <- 100    # Tamaño de la muestra
conteo <- numeric(nrep)  # Vector para almacenar el conteo

for (i in 1:nrep) {
  x <- runif(n=n, min=1, max=3)
  conteo[i] <- sum(x >= 2.5)
}
conteo  # Para obtener el conteo

install.packages(nortest)


# Pruebas de Hipotesis 

contenido <- c(510, 492, 494, 498, 492,
               496, 502, 491, 507, 496) 

t.test(contenido, alternative='two.sided',
       conf.level=0.95, mu=500)


######################################################################################


library(readxl)
Datos_Grupo_4 <- read_excel("C:/Users/Sala_000/Downloads/Datos - Grupo 4.xlsx", 
                            sheet = "Hoja1", col_types = c("date", 
                                                           "numeric"))

View(Datos_Grupo_4)


plot(Datos_Grupo_4$`TRM (COP/USD)`,type="l", col= "red") 

summary(Datos_Grupo_4)

sd(Datos_Grupo_4$`TRM (COP/USD)`)


t.test(Datos_Grupo_4$`TRM (COP/USD)`, alternative='greater',
       conf.level=0.99, mu=3738.19)


plot(diff(Datos_Grupo_4$`TRM (COP/USD)`),type="l")



###########################################################################################
# Prueba binomial 
 
z <- (174/200 - 0.90) / sqrt(0.90 * (1 - 0.90) / 200)

z  # Para obtener el valor del estadístico


install.packages("plyr")
library("plyr")

library(readxl)
Mensual_Renta_Fija <- read_excel("C:/Users/Sala_000/Downloads/Mensual Renta Fija.xlsx", 
                                 col_types = c("numeric", "text", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric"))

View(Mensual_Renta_Fija)


hist(Mensual_Renta_Fija$`PENSION BONDS`)

data.frame(Mensual_Renta_Fija)





BS.Value <- function(S, K, r, q=0, T, sigma)
  {  values <- c(2)    
  d1 <- (log(S/K)+(r-q+sigma^2/2)*T)/(sigma*sqrt(T)) 
  d2 <- d1 - sigma * sqrt(T)   
values[1] <- S*exp(-q*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2) 
values[2] <- K*exp(-r*T) * pnorm(-d2) - S*exp(-q*T)*pnorm(-d1)  
values}

 

























