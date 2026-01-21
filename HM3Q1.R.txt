# Dados observados
x <- c(0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84,
       2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27)

# Tamanho da amostra 
n <- length(x)

# Estimador de máxima verossimilhança
lambda_hat <- n / sum(x)

# Intervalo de valores para lambda
lambda <- seq(0.01, 0.5, length.out = 500)

# Função log-verossimilhança
loglik <- n * log(lambda) - lambda * sum(x)

# Gráfico da log-verossimilhança
plot(lambda, loglik, type = "l", lwd = 2,
     xlab = expression(lambda),
     ylab = expression(l(lambda)),
     main = "")

# Linha vertical indicando o MLE
abline(v = lambda_hat, col = "red", lwd = 2, lty = 2)

# Texto indicando o valor do MLE
text(lambda_hat, max(loglik),
     labels = expression(hat(lambda)),
     pos = 4, col = "red")