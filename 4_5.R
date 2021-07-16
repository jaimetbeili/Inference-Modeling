#BAYES
#Tenemos una enfermedad que solo afecta a uno de cada 3,900 individuos.
#Las pruebas tienen exactitud del 99% de las veces. Dado que una persona tuvo resultado positivo, ¿cual es la probabilidad de que si tengan la enfermedad?
#El teorema de Bayes dice que la probabilidad de que ocurra A (estar enfermo) dado que ya ocurrio B (test positivo) es:
#(Prob de A y B)/(Prob de B) -> ((Prob de B dado A)*(Prob de A))/(Prob de B)
#Da que la probabilidad de tener la enfermedad dado un test positivo es de 2%.

prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_D
N_H <- sum(outcome == "Healthy")    # number healthy
N_H

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

