library(tidyverse)
library(readr)
library(nnet)
library(ggplot2)

WineQualityWhite <- read_delim("C:/Users/luiz1/Downloads/Datasets/WineQualityWhite/WineQualityWhite.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Excluir a classificação do 3 e o 9
table(WineQualityWhite$quality)

WineQualityWhite$quality_class <- ifelse(WineQualityWhite$quality >= 6, "bom", "ruim")
WineQualityWhite <- WineQualityWhite[, -which(names(WineQualityWhite) == "quality")]

#Verificando os NA's
sum(is.na(WineQualityWhite$fixedacidity))
sum(is.na(WineQualityWhite$volatileacidity))
sum(is.na(WineQualityWhite$citricacid))
sum(is.na(WineQualityWhite$residualsugar))
sum(is.na(WineQualityWhite$chlorides))
sum(is.na(WineQualityWhite$freesulfurdioxide))
sum(is.na(WineQualityWhite$totalsulfurdioxide))
sum(is.na(WineQualityWhite$density))
sum(is.na(WineQualityWhite$pH))
sum(is.na(WineQualityWhite$sulphates))
sum(is.na(WineQualityWhite$alcohol)) # Tem NA
sum(is.na(WineQualityWhite$quality_class))

# Resolvendo os NA's
WineQualityWhite$alcohol[is.na(WineQualityWhite$alcohol)] <- mean(WineQualityWhite$alcohol, na.rm = TRUE)

# Verificar o resultado

set.seed(123) # Garantir replicabilidade
index <- sample(1:nrow(WineQualityWhite), 0.8 * nrow(WineQualityWhite))
train <- WineQualityWhite[index, ]
test <- WineQualityWhite[-index, ]

# Remover a coluna 'residualsugar' do conjunto de treino e teste
train_data <- train[, -which(names(train) == "residualsugar")]
test_data <- test[, -which(names(test) == "residualsugar")]

# Certifique-se de que as variáveis resposta ainda estão disponíveis
train_labels <- train$residualsugar
test_labels <- test$residualsugar

# Transformar a variável categórica em numérica
train_data <- train_data %>%
  mutate(quality_class = ifelse(quality_class == "bom", 1, 0))

test_data <- test_data %>%
  mutate(quality_class = ifelse(quality_class == "bom", 1, 0))

# Redes neurais

#Classificação

train$quality_class = as.factor(train$quality_class)
nn_model <- nnet(quality_class ~ ., data = train[, c(1:11, 12)], size = 10, maxit = 1000) 
pred_nn <- predict(nn_model, test[, 1:11], type = "class")

# Redes neurais: Métricas de Avaliação

# Confusion Matrix
conf_matrix_nn <- table(Predicted = pred_nn, Actual = test$quality_class)

# Extração dos valores focando na classe "bom"
TP <- conf_matrix_nn["bom", "bom"]  # True Positives (Predito como bom e é bom)
FP <- conf_matrix_nn["bom", "ruim"]  # False Positives (Predito como bom, mas é ruim)
FN <- conf_matrix_nn["ruim", "bom"]  # False Negatives (Predito como ruim, mas é bom)
TN <- conf_matrix_nn["ruim", "ruim"]  # True Negatives (Predito como ruim e é ruim)

# Total de observações
total <- sum(conf_matrix_nn)

# Cálculo das métricas
accuracy <- (TP + TN) / total  # Acurácia
precision <- TP / (TP + FP)  # Precisão (Valor Preditivo Positivo)
npv_nn <- TN / (TN + FN)  # Valor Preditivo Negativo
recall <- TP / (TP + FN)  # Recall (Sensibilidade)
specificity <- TN / (TN + FP)  # Especificidade
f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-Score
false_positive_rate <- FP / (FP + TN)  # Taxa de Falso Positivo
false_negative_rate <- FN / (FN + TP)  # Taxa de Falso Negativo

# Kappa
po <- (TP + TN) / total  # Observado
pe <- ((sum(conf_matrix_nn[, "bom"]) * sum(conf_matrix_nn["bom", ])) +
         (sum(conf_matrix_nn[, "ruim"]) * sum(conf_matrix_nn["ruim", ]))) / (total^2)  # Esperado
kappa <- (po - pe) / (1 - pe)  # Coeficiente Kappa

# Intervalo de Confiança para Acurácia
z_value <- qnorm(0.975)  # Valor crítico para 95% de confiança
se_accuracy <- sqrt((accuracy * (1 - accuracy)) / total)
accuracy_ci_lower <- accuracy - z_value * se_accuracy
accuracy_ci_upper <- accuracy + z_value * se_accuracy

# Intervalo de Confiança para Kappa
se_kappa <- sqrt((po * (1 - po)) / (1 - pe)^2 / total + (2 * (1 - po) * pe) / (1 - pe)^3 / total^2)
kappa_ci_lower <- kappa - z_value * se_kappa
kappa_ci_upper <- kappa + z_value * se_kappa

# Exibição dos resultados
cat("Métricas do Modelo de Redes Neurais (Evento de Interesse: 'bom'):\n")
cat("Acurácia:", accuracy, "\n")
cat("Intervalo de Confiança para Acurácia (95%): [", accuracy_ci_lower, ", ", accuracy_ci_upper, "]\n")
cat("Precisão (Valor Preditivo Positivo):", precision, "\n")
cat("Valor Preditivo Negativo (NPV):", npv_nn, "\n\n")
cat("Recall (Sensibilidade):", recall, "\n")
cat("Especificidade:", specificity, "\n")
cat("F1-Score:", f1_score, "\n")
cat("Taxa de Falso Positivo:", false_positive_rate, "\n")
cat("Taxa de Falso Negativo:", false_negative_rate, "\n")
cat("Kappa:", kappa, "\n")
cat("Intervalo de Confiança para Kappa (95%): [", kappa_ci_lower, ", ", kappa_ci_upper, "]\n")

# Criar a matriz de confusão para o modelo de redes neurais
conf_matrix_nn_df <- as.data.frame(as.table(conf_matrix_nn))

# Plotar a matriz de confusão com ggplot2
ggplot(data = conf_matrix_nn_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "green") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  labs(
    title = "Matriz de Confusão - Modelo Redes Neurais",
    x = "Classe Real",
    y = "Classe Prevista",
    fill = "Frequência"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )


#reg
nn_reg <- nnet(train_labels ~ ., data = train_data[, 1:11], size = 10, maxit = 1000, linout = TRUE)
pred_nn_reg <- predict(nn_reg, test_data[, 1:11])

r2 <- 1 - (sum((test_labels - pred_nn_reg)^2) / sum((test_labels - mean(test_labels))^2))

mse = mean((test_labels - pred_nn_reg)^2)

rmse <- sqrt(mse)

mae <- mean(abs(test_labels - pred_nn_reg))

mape <- mean(abs((test_labels - pred_nn_reg) / test_labels)) * 100
