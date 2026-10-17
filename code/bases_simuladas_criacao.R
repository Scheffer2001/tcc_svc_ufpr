### criação das bases simuladas e salvando em csv
library(gridExtra)
library(ggplot2)
library(dplyr)
set.seed(2025)
options(device = 'x11')
### Two Moons
generate_two_moons <- function(n_samples = 100,
                               noise = 0.1,
                               random_seed = NULL) {
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  n_samples_half <- ceiling(n_samples / 2)
  theta <- seq(0, pi, length.out = n_samples_half)
  x1 <- cos(theta) + rnorm(n_samples_half, sd = noise)
  y1 <- sin(theta) + rnorm(n_samples_half, sd = noise)
  class1 <- rep(0, n_samples_half)
  
  theta <- seq(0, pi, length.out = n_samples - n_samples_half)
  x2 <- 1 - cos(theta) + rnorm(n_samples - n_samples_half, sd = noise)
  y2 <- 0.5 - sin(theta) + rnorm(n_samples - n_samples_half, sd = noise)
  class2 <- rep(1, n_samples - n_samples_half)
  x <- c(x1, x2)
  y <- c(y1, y2)
  class <- factor(c(class1, class2))
  data.frame(x = x, y = y, class = class)
}
two_moons <- generate_two_moons(n_samples = 500,
                                noise = 0.1,
                                random_seed = 2025)

add_noise <- function(x, y, sd = 0.05) {
  x <- x + rnorm(length(x), sd = sd)
  y <- y + rnorm(length(y), sd = sd)
  return(data.frame(x, y))
}

### Dois clusters densos + um em forma de U
c1 <- add_noise(rnorm(100, -1, sd = 0.2), rnorm(100, 1, sd = 0.2))
c2 <- add_noise(rnorm(100, 1, sd = 0.2), rnorm(100, 1, sd = 0.2))
x <- seq(-3, 3, length.out = 300)
c3 <- data.frame(x, x^2 / 3 - 1.5)
c3 <- add_noise(c3[, 1], c3[, 2], sd = 0.05)
data_c <- rbind(c1, c2, c3)
labels_c <- factor(rep(1:3, times = c(100, 100, 300)))
smile <- data.frame(data_c, label = labels_c)

### Três camadas horizontais
d1 <- cbind(rnorm(300, 0), rnorm(300, 3))
d2 <- cbind(rnorm(300, 0), rnorm(300, 0))
d3 <- cbind(rnorm(300, 0), rnorm(300, -3))
data_d <- rbind(d1, d2, d3)
labels_d <- factor(rep(1:3, each = 300))
three_layers <- data.frame(data_d, label = labels_d)

### Denso no centro + curva ao redor
g1 <- data.frame(x = rnorm(200, 0, sd = 0.5), y = rnorm(200, 1.3, sd = 0.5))
x <- seq(-3, 3, length.out = 300)
g2 <- data.frame(x, y = x^2 / 3 - 1)
g2 <- add_noise(g2$x, g2$y, sd = 0.08)

data_g <- rbind(g1, g2)
labels_g <- factor(rep(1:2, times = c(200, 300)))
centro_denso <- data.frame(data_g, label = labels_g)

### Quatro blocos separados
h1 <- cbind(rnorm(200, -2, sd = 0.7), rnorm(200, 2, sd = 0.7))
h2 <- cbind(rnorm(200, 2, sd = 0.7), rnorm(200, 2, sd = 0.7))
h3 <- cbind(rnorm(200, -2, sd = 0.7), rnorm(200, -2, sd = 0.7))
h4 <- cbind(rnorm(200, 2, sd = 0.7), rnorm(200, -2, sd = 0.7))
data_h <- rbind(h1, h2, h3, h4)
labels_h <- factor(rep(1:4, each = 200))
quatro_blocos <- data.frame(data_h, labels_h)

### Espiral com 4 voltas
make_spiral <- function(points,
                        revolutions = 1,
                        offset = 0,
                        noise_sd = 0.1) {
  t <- seq(0, 2 * pi * revolutions, length.out = points)
  x <- (t + offset) * cos(t) + rnorm(points, sd = noise_sd)
  y <- (t + offset) * sin(t) + rnorm(points, sd = noise_sd)
  return(data.frame(x, y))
}
spirals <- do.call(rbind, lapply(0:3, function(i)
  make_spiral(300, 1, i * 1.5, noise_sd = 0.15)))
labels_i <- factor(rep(1:4, each = 300))
data_i <- spirals
espirais <- data.frame(data_i, labels_i)

### two rings
generate_two_rings <- function(n = 400, noise = 0.2) {
  generate_circle <- function(n, radius, noise) {
    theta <- runif(n, 0, 2*pi)
    x <- radius * cos(theta) + rnorm(n, 0, noise)
    y <- radius * sin(theta) + rnorm(n, 0, noise)
    data.frame(x = x, y = y)
  }
  inner_circle <- generate_circle(n/2, radius = 1, noise = noise)
  outer_circle <- generate_circle(n/2, radius = 2, noise = noise)
  data <- rbind(
    cbind(inner_circle, class = 0),  # Círculo interno = classe 0
    cbind(outer_circle, class = 1)   # Círculo externo = classe 1
  )
  return(data)
}
set.seed(123)  # Para reprodutibilidade
two_rings <- generate_two_rings(n = 400, noise = 0.15)
plot(two_rings$x, two_rings$y, col = ifelse(two_rings$class == 0, "blue", "red"),
     pch = 19, main = "Two Rings Dataset (0 e 1)", xlab = "X", ylab = "Y")
legend("topright", legend = c("Classe 0", "Classe 1"), col = c("blue", "red"), pch = 19)
###--------------------------------------Gráficos-----------------------------------###
a1 <- two_moons %>% ggplot(aes(x = x, y = y, col = class)) +
  geom_point(alpha = 0.6, size = 2) + ggtitle('Two Moons') +
  theme_minimal() + theme(legend.position = "") +
  scale_color_manual(
    values = c("1" = "indianred",'2' = 'yellowgreen','3' = 'purple','4' = 'lightgray',
      setNames(rainbow(15), as.character(2:16))))
a2 <- smile %>% ggplot(aes(x, y, color = label)) +
  geom_point(alpha = 0.6, size = 2) + ggtitle('Sorriso') +
  theme_minimal() +theme(legend.position = "") +
  scale_color_manual(
    values = c("1" = "indianred",'2' = 'yellowgreen','3' = 'purple','4' = 'lightgray',
      setNames(rainbow(15), as.character(2:16))))
a3 <- three_layers %>% ggplot(aes(x = X1 , y = X2, color = label)) +
  geom_point(alpha = 0.6, size = 2) + ggtitle('Três camadas') +
  theme_minimal() + theme(legend.position = "") +
  scale_color_manual(
    values = c("1" = "indianred",'2' = 'yellowgreen','3' = 'purple','4' = 'lightgray',
               setNames(rainbow(15), as.character(2:16))))
a4 <- centro_denso %>% ggplot(aes(x, y, color = label)) +
  geom_point(alpha = 0.6, size = 2) + ggtitle('Centro denso com curva "U"') +
  theme_minimal() + theme(legend.position = "") +
  scale_color_manual(
    values = c("1" = "indianred",'2' = 'yellowgreen','3' = 'purple','4' = 'lightgray',
      setNames(rainbow(15), as.character(2:16))))
a5 <- quatro_blocos %>% ggplot(aes(x = X1, y = X2, col = labels_h)) +
  geom_point(alpha = 0.6, size = 2) +ggtitle('Quatro blocos') +
  theme_minimal() + theme(legend.position = "") +
  scale_color_manual(
    values = c("1" = "indianred",'2' = 'yellowgreen','3' = 'purple','4' = 'lightgray',
      setNames(rainbow(15), as.character(2:16))))
a6 <- espirais %>% ggplot(aes(x, y, col = labels_i)) +
  geom_point(alpha = 0.6, size = 2) +ggtitle('Quatro espirais') + 
  theme_minimal() + theme(legend.position = "") +
  scale_color_manual(
    values = c("1" = "indianred",'2' = 'yellowgreen','3' = 'purple','4' = 'lightgray',
      setNames(rainbow(15), as.character(2:16))))

grid.arrange(a1, a2, a3, a4, a5, a6, nrow = 2, ncol = 3)





### Salvando arquivos
two_moons_csv <- two_moons %>% rename(label = class)
smile_csv <- smile %>% rename(label = label, x = x, y = y)
three_layers_csv <- three_layers %>% rename(x = X1, y = X2, label = label)
centro_denso_csv <- centro_denso %>% rename(x = x, y = y, label = label)
quatro_blocos_csv <- quatro_blocos %>% rename(x = X1, y = X2, label = labels_h)
espirais_csv <- espirais %>% rename(x = x, y = y, label = labels_i)

write.csv(espirais_csv, "dataset_quatro_espirais.csv", row.names = FALSE)
write.csv(two_moons_csv, "dataset_two_moons.csv", row.names = FALSE)
write.csv(quatro_blocos_csv, "dataset_quatro_blocos.csv", row.names = FALSE)
write.csv(centro_denso_csv,"dataset_centro_denso_curva.csv",row.names = FALSE)
write.csv(three_layers_csv, "dataset_tres_camadas.csv", row.names = FALSE)
write.csv(smile_csv, "dataset_sorriso.csv", row.names = FALSE)
write.csv(two_rings, "two_rings_dataset.csv", row.names = FALSE)
rm(list=ls())