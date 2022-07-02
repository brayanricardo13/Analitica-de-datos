
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























