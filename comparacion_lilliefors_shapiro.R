# Longitud de cada vector ----
R <- 1e4

# Lista con vectores de p-valores ----
p.values <- list(
  l.10 = vector('numeric',R),
  l.20 = vector('numeric',R),
  l.30 = vector('numeric',R),
  l.50 = vector('numeric',R),
  l.100 = vector('numeric',R),
  l.500 = vector('numeric',R),
  l.1000 = vector('numeric',R),
  l.4000 = vector('numeric',R),
  sw.10 = vector('numeric',R),
  sw.20 = vector('numeric',R),
  sw.30 = vector('numeric',R),
  sw.50 = vector('numeric',R),
  sw.100 = vector('numeric',R),
  sw.500 = vector('numeric',R),
  sw.1000 = vector('numeric',R),
  sw.4000 = vector('numeric',R)
)

# Número de muestras ----
n.muestra <- c(10,20,30,50,100,500,1000,4000)

# Toma de muestra y cálculo de p-valor ----
set.seed(123)

for(i in 1:length(n.muestra)){
  for(j in 1:R){
    r.numbers <- sample(abalone$whole_weight, size = n.muestra[i], replace = FALSE)
    
    p.values[[i]][j] <- lillie.test(r.numbers)$p.value
    p.values[[i+8]][j] <- shapiro.test(r.numbers)$p.value
  }
}

# Proporción de p-valores ≥ 0.05
p.values.comp <- data.frame(
  prueba = factor(rep(c('Lilliefors','Shapiro-Wilk'),each=8)),
  n.muestra = rep(n.muestra,2),
  proporcion = rep(NA, 16)
)

for(i in 1:length(p.values)){
  p.values.comp$proporcion[i] <- mean(p.values[[i]] <= 0.05)
}

p.values.comp %>%
  ggplot(aes(x = factor(n.muestra), y = proporcion, shape = prueba)) +
  geom_point(size = 8, color = 'black', stroke = 1.2) +
  ylim(0,1) +
  labs(
    x = 'Tamaño de la muestra',
    y = 'Proporción de p-valores ≤ 0.05',
    title = 'Comparación de la proporción de p-valores entre Lilliefors y Shapiro-Wilk',
    subtitle = '10000 simulaciones',
    shape = 'Prueba',
    caption = 'Fuente: Nash, W., Sellers, T., Talbot, S., Cawthorn, A. y Ford, W. (1994).\nAbalone [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C55C7W.'
  ) +
  scale_shape_manual(values = c(1,4)) +
  theme_minimal() +
  theme(legend.position = 'bottom')

p.values %>%
  as_tibble() %>%
  pivot_longer(cols = 1:16, names_to = 'test.sample', values_to = 'p.value') %>%
  ggplot(aes(x = p.value)) +
  geom_histogram(color = 'black', bins = 14, fill = 'aquamarine') +
  facet_wrap(. ~ factor(test.sample, levels = paste0(rep(c('l.','sw.'),each=8),rep(n.muestra,2)))) +
  theme_minimal()