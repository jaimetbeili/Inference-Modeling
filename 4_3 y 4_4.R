library(tidyverse)

#Los intervalos de confianza son espacios en los que creemos, con cierto nivel de confianza, que podemos encontrar nuestro parametro p.
#Aqui se ven en la grafica
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

#Queremos conocer la probabilidad de que un intervalo entre Xgorrito +- 2errores estandar contenga p. Normalmente esta en 95%.
#Recordemos que X es uns variable aleatoria, estamos tomando una muesta, asi que correr este codigo dos veces nos da resultados diferentes.
p <- 0.45
N <- 1000
X <- sample(c(0,1), N, replace = TRUE, prob = c(1-p, p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

#Si quisieramos un intervalo de confianza diferente al de 95%, se puede calcular asi
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
z
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

#¿Como sabemos que un intervalo de confianza de 95% efectivamente contiene a p el 95% de las veces? Podemos correr una Monte Carlo.
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

#Las encuestadoras ganan por predecir ganadores, no intervalos.
#Si tenemos 25 encuestados nuestro spread va de un numero negativo a uno positivo (incluyendo el 0).
#Como el cero significa que la eleccion empata, tenerlo en nuestro intervalo no nos permite declarar a un ganador.
#Aumentar el tamano de la muestra nos permite cambiar eso.
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

#Un p-value implica la probabilidad de que hayamos visto un efecto significativo cuando en realidad no lo habia.
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))
#En este caso sacamos 52 bolitas azules de 100 que tomamos.
#El p-value nos dice que hay un 68.9% de probabilidad de que ese 52% de bolitas azules no sea algo real, sino solo algo que vimos en nuestra muestra.

#En la eleccion de Obama 2012 habia quien estaba totalmente seguro de que iba a ganar porque hizo un agregado de encuestas
#La diferencia entre Obama y Romney fue de 0.039. Agregamos encuestas con 12 poblaciones diferentes (12 N diferentes) y sacamos la proporcion de gente que voto por obama (p).
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

#Ahora con esos datos construimos intervalos de confianza para cada encuesta.
# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#Ahora, para agregar bien en serio las encuestas, tenemos que sacar un promedio ponderado:
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

#Sacamos el spread y el error del spread. Vemos que el spread +- el error no contiene 0 (Obama es claro ganador) y ademas si contiene el resultado correcto de 3.9
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
s<-round(d_hat*100,1)
se<-round(moe*100, 1)
between(3.9,(s-se),(s+se))

#Vamos a ver como se comportaron las encuestas y predicciones en 2016.

library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

#El spread se predijo en 0.14 y el error en 0.0066, pero el spread real fue de 0.021
#Podemos ver que una de las razones para esto es que el histograma no muestra un spread con distribucion normal. Ahora veremos porque.
# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="pink", binwidth = .01)

# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

#Esta grafica muestra que diferentes encuestadoras predijeron spreads muy diferentes...
# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Incluso cuando los errores no lo son tanto.
# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

#Para atender el sesgo de las encuestadoras podemos tomar una encuesta de cada una y utilizar su desviacion estandar para producir un error promedio.

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

sd(one_poll_per_pollster$spread)

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(color = "red", fill = "darkgreen", binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)
