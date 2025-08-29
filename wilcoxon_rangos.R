library(coin)

carotid <- c(38.51,38.45,38.27,38.52,38.62,38.18)
brain <- c(39.32,39.21,39.20,38.68,39.09,38.94)

summary(carotid)
summary(brain)

wilcoxsign_test(brain ~ carotid)
wilcox.test(x = carotid, y = brain, paired = T, exact = F)

fuller <- data.frame(carotid, brain)

library(writexl)
write_xlsx(fuller, 'fuller.xlsx')