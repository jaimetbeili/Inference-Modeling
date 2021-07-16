#Hay data que no se puede trabajar solo con lo que ya hemos aprendido, como las dicotomicas o categoricas.
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

#Veamos como tuvieron exito las mujeres ante los hombres en la ciencia.
# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))


#Una forma de poder inferir con data binomial o categorica es con la regla de fisher.
#Fisher calcula la probabilidad de un evento dado otro con un sistema matricial.
#Ej. Una senora que dice poder adivinar si la leche se sirvio antes o despues del te.
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")
#El p-value es la probabilidad de que sea solo suerte.


#Podriamos hacer lo mismo con el tema de las mujeres en la ciencia, pero el caso es diferente.
#En esta ocasion no hay un numero fijo de "respuestas correctas". En el del te si lo habia.
#Podemos usar la chi_cuadrada:
# compute overall funding rate - Todos los exitos en promedio
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data - la misma tablita, para ver si es aleatoria la diferencia o no.
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table - una tablita de lo que esperariamos ver si el success rate fuera aleatorio (independiente del genero)
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value
#0.051 de probabilidad de que eso sea aleatorio.

#Ahora veamoslo con estadistica bayesiana, dado X=1,0 si hombre,mujer y Y=1,0 success,nosuccess
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women


#Los p_values no lo son todo. Hay que incluir el ratio.
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()
