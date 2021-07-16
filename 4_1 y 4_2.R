library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)

#Si nos salen 12 azules y 13 rojas, nuestro valor esperado de X es 0.48
#Usamos este valor como un estimado. Con eso podemos ver la probabilidad de que X este a 1% de nuestro p
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

#Hay una posibilidad de 95% de que Xbarra este dentro de 2 desviaciones estandar del parametro p.

#No conocemos el valor de p, pero podemos darle un estimado para simular:
p <- 0.45
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
x_hat

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#Tener una encuesta con muchos individuos (N=100,000) podria ayudar a predecir mejor, como indica la teoria:
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line(color = "blue")
#En ese caso, donde p fuera cualquier valor entre .35 y .65 nuestro error es menor al .3%
#El problema es que en toda encuesta hay sesgos y los sesgos aumentan el margen de error.