# --------------------------------------------------
# Importação dos dados
# --------------------------------------------------

install.packages("palmerpenguins")  # executar apenas uma vez
library(palmerpenguins)

data("penguins")

# Selecionar variáveis de interesse e remover NA
dados <- na.omit(penguins[, c("body_mass_g", "bill_length_mm")])

x <- dados$body_mass_g
y <- dados$bill_length_mm

# --------------------------------------------------
# Gráfico de dispersão
# --------------------------------------------------

plot(
  x, y,
  xlab = "Massa corporal (g)",
  ylab = "Comprimento do bico (mm)",
  main = "",
  pch = 16,
  col = "gray"
)

# --------------------------------------------------
# Estimadores de mínimos quadrados (cálculo manual)
# --------------------------------------------------

x_bar <- mean(x)
y_bar <- mean(y)

beta1_hat <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
beta0_hat <- y_bar - beta1_hat * x_bar

beta0_hat
beta1_hat

# --------------------------------------------------
# Ajuste via lm()
# --------------------------------------------------

modelo_lm <- lm(bill_length_mm ~ body_mass_g, data = dados)
coef(modelo_lm)

# Adicionar reta de regressão ao gráfico
abline(beta0_hat, beta1_hat, col = "red", lwd = 2)

# --------------------------------------------------
# Resíduos
# --------------------------------------------------

residuos <- resid(modelo_lm)
y_hat <- fitted(modelo_lm)

# Gráfico de resíduos
plot(
  x, residuos,
  xlab = "Massa corporal (g)",
  ylab = "Resíduos (mm)",
  main = "",
  pch = 16,
  col = "gray"
)
abline(h = 0, col = "red", lwd = 2)

# --------------------------------------------------
# RMSE e R²
# --------------------------------------------------

RMSE <- sqrt(mean(residuos^2))
RMSE

R2_manual <- 1 - sum(residuos^2) / sum((y - mean(y))^2)
R2_lm <- summary(modelo_lm)$r.squared

R2_manual
R2_lm

# --------------------------------------------------
# Introdução de um outlier artificial
# --------------------------------------------------

dados_outlier <- dados

outlier <- data.frame(
  body_mass_g = 8000,
  bill_length_mm = 80
)

dados_outlier <- rbind(dados_outlier, outlier)

# Ajuste do novo modelo
modelo_outlier <- lm(bill_length_mm ~ body_mass_g, data = dados_outlier)

coef(modelo_lm)
coef(modelo_outlier)

# --------------------------------------------------
# Comparação gráfica das retas
# --------------------------------------------------

plot(
  dados_outlier$body_mass_g,
  dados_outlier$bill_length_mm,
  xlab = "Massa corporal (g)",
  ylab = "Comprimento do bico (mm)",
  main = "",
  pch = 16,
  col = "gray"
)

abline(modelo_lm, col = "blue", lwd = 2)
abline(modelo_outlier, col = "red", lwd = 2)

legend(
  "topleft",
  legend = c("Modelo original", "Modelo com outlier"),
  col = c("blue", "red"),
  lwd = 2
)

# --------------------------------------------------
# RMSE e R² com outlier
# --------------------------------------------------

rmse <- function(modelo) {
  sqrt(mean(resid(modelo)^2))
}

RMSE_original <- rmse(modelo_lm)
RMSE_outlier  <- rmse(modelo_outlier)

R2_original <- summary(modelo_lm)$r.squared
R2_outlier  <- summary(modelo_outlier)$r.squared

RMSE_original
RMSE_outlier

R2_original
R2_outlier
