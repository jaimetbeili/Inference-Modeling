library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#Si queremos pronosticar los resultados de una eleccion usando los datos anteriores con estadistica bayesiana, lo hacemos asi
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
#Mu y Tau son el promedio y la sd de los datos antes de tener cualquier observacion (antes de tener encuestas de este ano)
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0 aka probabilidad de que ganara Hillary
1 - pnorm(0, posterior_mean, posterior_se)
#El resultado es de 99.99%, demasiado alto. Aqui habria que entender como juegan los sesgos de cada encuestadora y el sesgo general de la eleccion.

#Si tomamos seis encuestas de una misma casa encuestadora, podemos representar sus spreads de la siguiente forma:
#X_j = d + epsilon_j
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#Podemos hacer lo mismo con cinco encuestadoras y 6 encuestas cada una.
#X_i,j = d + epsilon_i,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#En el codigo anterior no se nota ningun tipo de efecto por diferencias en casa encuestadora, asi que para medirlo anadimos un termino h.
#X_i,j = d + h_i + epsilon_i,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
#Cada encuestadora tiene su propia h. Es decir, su propio efecto diferente.

#Seguimos sin incluir el hecho de que en cada eleccion hay un sesgo general hacia uno de los lados. Podemos anadirlo con el termino b.
#X_i,j = d + b + h_i + epsilon_i,j
mu <- 0
tau <- 0.035
#La b es un aumento en la variabilidad de X que afecta a todas las encuestas y a todas las casas encuestadoras.
#Por eso la podemos incluir directamente en la sigma de X, como hacemos aqui con el termino .025^2.
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#Hasta ahora vimos como predecir el voto total en una eleccion, pero no sirve eso para EUA por el colegio electoral.
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#Lo primero que hacemos es filtrar las encuestas de la ultima semana antes de la eleccion que no sean a nivel federal y sean de buena calidad.
#Agrupamos por Estado y creamos las variables que queremos.
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
#left_join() te permite pegar una base de datos con otra siguiendo un criterio.
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#Corremos una Monte Carlo para cada Estado, asumiendo que no sabemos nada de su resultado (mu = 0) y que los resultados en cada estado cambian poco de un ano a otro (tau = 0.02).
#Asi se ve una eleccion:
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#Ahora asi se ven mil:
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
#Como DC y RI no tienen encuestas, se tienen que agregar esos 7 votos a Clinton, que seguro los gana.
#Lo que esto hace es predecir cuantas veces los votos de clinton superaron 269.
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#Pero el modelo presentado no tomo en cuenta el sesgo general de la eleccion
#Aqui hacemos lo mismo, pero incluyendo un sesgo de 0.03, pues en los estados tiende a ser mayor que a nivel federal
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)


#Tenemos que pensar tambien como cambian las predicciones a lo largo del tiempo de campana.
#Para evitar sesgos de la encuestadora, tomemos una sola.
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

#Pasa que algunos eventos elevan o reducen el furor por un candidato, por lo que el error puede llegar a ser bastante grande.
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#Para atender la variabilidad en el tiempo podriamos agregar un termino b_t, como el que usamos para el sesgo general, cuya varianza disminuya conforme se acerca la eleccion.
#Tambien podemos anadir una f(t) que se refiere a las tendencias. De hecho, esta grafica muestra las tendencias a favor de cada candidato, en lugar del spread entre ellos.
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))


#Distribucion t en lugar de distribucion normal para usarse en casos con pocas observaciones pero que estamos seguros de que la distribucion es normal en la poblacion.
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)
