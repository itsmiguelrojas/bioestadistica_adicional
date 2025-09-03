# Cargar librerías ----
library(tidyverse)
library(readxl)

# Cargar datos ----
pigmentacion <- read_xlsx('datos/whitman2004.xlsx')

# Gráfico de puntos ----
pigmentacion %>%
  ggplot(aes(x = edad, y = proporcion_pigmentacion)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  theme_minimal()

# Modelo lineal ----
pigmentacion.lm <- lm(proporcion_pigmentacion ~ edad, data = pigmentacion)
summary(pigmentacion.lm)

pigmentacion.pred <- cbind(
  pigmentacion,
  predict(pigmentacion.lm, interval = 'prediction')
)

pigmentacion.pred %>%
  ggplot(aes(x = edad, y = proporcion_pigmentacion)) +
  geom_point() +
  geom_smooth(se = F, method = lm) +
  geom_smooth(aes(y = lwr), se = F, method = lm, color = 'red', linetype = 'dashed') +
  geom_smooth(aes(y = upr), se = F, method = lm, color = 'red', linetype = 'dashed')

# Modelo lineal con intersección cero
pigmentacion.lm.0 <- lm(proporcion_pigmentacion ~ edad + 0, data = pigmentacion)
summary(pigmentacion.lm.0)

predict(pigmentacion.lm, method = 'interval')