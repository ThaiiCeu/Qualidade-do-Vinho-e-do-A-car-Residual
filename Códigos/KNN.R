library(tidyverse)
library(readr)
library(ggplot2)
library(class)
library(FNN)

# Chamando a base
WineQualityWhite <- read_delim("C:/Users/luiz1/Downloads/Datasets/WineQualityWhite/WineQualityWhite.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Criando a variável quality_class e exluindo a var quality
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

# K - nn

#Classificação
knn_model <- knn(train[, 1:11], test[, 1:11], cl = train$quality_class, k = 5)

# Confusion Matrix
conf_matrix <- table(Predicted = knn_model, Actual = test$quality_class)

# Extração de valores da matriz de confusão focando no evento de interesse "bom"
TP <- conf_matrix["bom", "bom"]  # True Positives
FP <- conf_matrix["bom", "ruim"]  # False Positives
FN <- conf_matrix["ruim", "bom"]  # False Negatives
TN <- conf_matrix["ruim", "ruim"]  # True Negatives

# Total de observações
n <- sum(conf_matrix)

# Verificar se a matriz de confusão é válida
if (n == 0) stop("A matriz de confusão está vazia ou inválida.")

# Cálculo das métricas
accuracy <- (TP + TN) / n  # Acurácia
precision <- TP / (TP + FP)  # Precisão
negative_predictive_value <- TN / (TN + FN)  # Valor Preditivo Negativo
recall <- TP / (TP + FN)  # Recall (Sensibilidade)
specificity <- TN / (TN + FP)  # Especificidade
f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-Score
false_positive_rate <- FP / (FP + TN)  # Taxa de Falso Positivo
false_negative_rate <- FN / (FN + TP)  # Taxa de Falso Negativo

# Função para calcular Kappa e intervalo de confiança
calculate_kappa_ci <- function(conf_matrix, conf_level = 0.95) {
  # Calcular probabilidade observada (Po)
  po <- sum(diag(conf_matrix)) / n
  
  # Calcular probabilidade esperada (Pe)
  row_totals <- rowSums(conf_matrix)
  col_totals <- colSums(conf_matrix)
  pe <- sum((row_totals * col_totals) / n^2)
  
  # Verificar se Pe é igual a 1 para evitar divisão por zero
  if (pe == 1) stop("A probabilidade esperada (Pe) é 1, não é possível calcular o Kappa.")
  
  # Calcular Kappa
  kappa <- (po - pe) / (1 - pe)
  
  # Erro padrão do Kappa (ajustado)
  term1 <- (po * (1 - po)) / ((1 - pe)^2 * n)
  term2 <- (2 * (1 - po) * pe) / ((1 - pe)^3 * n^2)
  var_kappa <- term1 + term2
  
  # Verificar se a variância é negativa ou inválida
  if (var_kappa <= 0 || is.nan(var_kappa)) {
    stop("A variância do Kappa é inválida. Verifique a matriz de confusão.")
  }
  
  se_kappa <- sqrt(var_kappa)
  
  # Calcular intervalo de confiança
  z_value <- qnorm((1 + conf_level) / 2)
  ci_lower <- kappa - z_value * se_kappa
  ci_upper <- kappa + z_value * se_kappa
  
  list(
    kappa = kappa,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# Intervalo de confiança para a acurácia
z_value <- qnorm(0.975)  # Valor crítico para 95% de confiança
error_margin <- z_value * sqrt((accuracy * (1 - accuracy)) / n)
accuracy_ci_lower <- accuracy - error_margin
accuracy_ci_upper <- accuracy + error_margin

# Exibição dos resultados
cat("Métricas (Evento de Interesse: 'bom'):\n")
cat("Acurácia:", accuracy, "\n")
cat("Precisão:", precision, "\n")
cat("Valor Preditivo Negativo:", negative_predictive_value, "\n")
cat("Recall (Sensibilidade):", recall, "\n")
cat("Especificidade:", specificity, "\n")
cat("F1-Score:", f1_score, "\n")
cat("Taxa de Falso Positivo:", false_positive_rate, "\n")
cat("Taxa de Falso Negativo:", false_negative_rate, "\n")
cat("Intervalo de Confiança para Acurácia (95%): [", accuracy_ci_lower, ", ", accuracy_ci_upper, "]\n")
tryCatch({
  kappa_results <- calculate_kappa_ci(conf_matrix)
  cat("Kappa:", kappa_results$kappa, "\n")
  cat("Intervalo de Confiança para Kappa (95%): [", kappa_results$ci_lower, ", ", kappa_results$ci_upper, "]\n")
}, error = function(e) {
  cat("Erro:", e$message, "\n")
})

# Criar a matriz de confusão como data frame
conf_matrix_df <- as.data.frame(as.table(conf_matrix))

# Plotar a matriz de confusão com ggplot2
ggplot(data = conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  labs(
    title = "Matriz de Confusão - Modelo KNN",
    x = "Classe Real",
    y = "Classe Prevista",
    fill = "Frequência"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

############## Código a mão que utilizamos
KNN <- function(train, test, tr.class, te.class, dist = 'euclidean', k = 3, lambda)
{
  # Códigos de erro para entrada de argumentos inválidos 
  
  if(!is.data.frame(train) && !is.matrix(train))
    stop("Erro001: O objeto 'train' deve ser do tipo data.frame ou matrix.")
  
  if(!is.na(tr.class[1]))
  {
    tr.class    <- as.matrix(tr.class)
    if(nrow(train) != length(tr.class))
      stop("Erro002: Os objetos 'class' e 'train' devem ter o mesmo número de linhas.")
  }
  
  if(!(dist %in% c('euclidean', 'manhattan', 'minkowski', 'canberra', 'chebyshev')))
    stop("Erro003: 'dist' deve ser: 'euclidean', 'manhattan', 'minkowski', 'canberra', ou 'chebyshev'.")
  
  if (dist == 'minkowski' && is.null(lambda)) 
  {
    stop("Erro004: 'lambda' deve ser definido quando 'dist' é 'minkowski'.")
  }
  
  # Inicialização dos objetos
  
  nrow.tr      <- nrow(train)
  nrow.te      <- nrow(test)
  dist_matrix  <- matrix(0, nrow = nrow.te, ncol = nrow.tr) # Distâncias
  predict      <- as.character(nrow.te) # Predições
  
  # Cálculo das distâncias
  
  for (i in 1:nrow.te) 
  {
    for (j in 1:nrow.tr) 
    {
      if(dist == 'euclidean'){
        dist_matrix[i, j] <- sqrt(sum((train[j, ] - test[i, ])^2))
      } else if(dist == 'manhattan'){
        dist_matrix[i, j] <- sum(abs(train[j, ] - test[i, ]))
      } else if(dist == 'canberra'){
        dist_matrix[i, j] <- sum(abs(train[j, ] - test[i, ]) / (train[j, ] + test[i, ]))
      } else if(dist == 'chebyshev'){
        dist_matrix[i, j] <- max(abs(train[j, ] - test[i, ]))
      } else if (dist == 'minkowski'){
        dist_matrix[i, j] <- (sum(abs(train[j, ] - test[i, ])^lambda))^(1 / lambda)
      }
    }
    
    # Determinar os k vizinhos mais próximos
    
    nearest_indices <- order(dist_matrix[i, ])[1:k]
    tb.knn          <- table(tr.class[nearest_indices])
    
    # Classificação das instâncias
    
    predict[i] <- names(tb.knn[which.max(tb.knn)])
  }
  
  # Matriz de confusão   
  
  labs <- names(table(te.class))
  conf_matrix <- matrix(0, nrow = length(labs), ncol = length(labs))
  rownames(conf_matrix) <- labs
  colnames(conf_matrix) <- labs
  
  for (k in 1:length(predict))
  {
    pred <- as.character(predict[k])
    true <- as.character(te.class[k])
    conf_matrix[pred, true] <- conf_matrix[pred, true] + 1
  }
  
  # Gráfico da matriz de confusão usando ggplot2
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) 
  {
    install.packages("ggplot2")
  }
  suppressWarnings(library(ggplot2))
  
  conf_matrix_melted           <- as.data.frame(as.table(conf_matrix))
  colnames(conf_matrix_melted) <- c("Predicted", "True", "Count")
  conf_matrix_melted$FillColor <- ifelse(conf_matrix_melted$Predicted == conf_matrix_melted$True, 
                                         conf_matrix_melted$Count, 
                                         NA)
  conf_matrix_plot     <- ggplot(conf_matrix_melted, aes(x = True, y = Predicted, fill = FillColor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), color = "black") +
    scale_fill_gradient(low = "gray", high = "salmon", na.value = "white") +
    scale_y_discrete(limits = rev(labs)) + 
    labs(x = "Classes Atuais", y = "Classes Preditas", title = "Matriz de Confusão") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 45), 
          axis.title.x = element_text(margin = margin(t = 12)),
          axis.title.y = element_text(margin = margin(r = 12)),
          legend.position = "none")
  
  # Cálculo das métricas de desempenho:
  
  # - Acurácia (acc)
  # - Precisão (Ou Valor Preditivo Positivo) (precision)
  # - Valor Preditivo Negativo (negative_predictive_value)
  # - Recall (recall)
  # - Especificidade (specificity)
  # - F1-Score (f1_score)
  # - Taxa de Falso Positivo (false_positive_rate)
  # - Taxa de Falso Negativo (false_negative_rate)
  # - Coeficiente Kappa (kappa)
  # - Intervalos de Confiança para Acurácia e Kappa
  
  acc         <- sum(diag(conf_matrix)) / sum(conf_matrix) 
  n           <- length(te.class)
  z           <- qnorm(1 - 0.05 / 2)
  se          <- sqrt((acc * (1 - acc)) / n)
  ci_low      <- acc - z * se
  ci_high     <- min(acc + z * se, 1)
  
  precision   <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fp      <- sum(conf_matrix[, label]) - tp
    return(tp / (tp + fp))
  }
  
  precision_values <- sapply(labs, function(label) precision(conf_matrix, label))
  
  recall      <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fn      <- sum(conf_matrix[label, ]) - tp
    return(tp / (tp + fn))
  }
  
  recall_values <- sapply(labs, function(label) recall(conf_matrix, label))
  
  specificity <- function(conf_matrix, label) 
  {
    tn      <- sum(conf_matrix) - sum(conf_matrix[label, ]) - sum(conf_matrix[, label]) + conf_matrix[label, label]
    fp      <- sum(conf_matrix[, label]) - conf_matrix[label, label]
    return(tn / (tn + fp))
  }
  
  specificity_values <- sapply(labs, function(label) specificity(conf_matrix, label))
  
  f1_score    <- function(precision, recall) 
  {
    if(precision + recall == 0) return(0)
    return(2 * (precision * recall) / (precision + recall))
  }
  
  f1_scores   <- mapply(f1_score, precision_values, recall_values)
  
  false_positive_rate <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fp      <- sum(conf_matrix[, label]) - tp
    tn      <- sum(conf_matrix) - sum(conf_matrix[label, ]) - sum(conf_matrix[, label]) + conf_matrix[label, label]
    return(fp / (fp + tn))
  }
  
  fpr_values  <- sapply(labs, function(label) false_positive_rate(conf_matrix, label))
  
  false_negative_rate <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fn      <- sum(conf_matrix[label, ]) - tp
    return(fn / (fn + tp))
  }
  
  fnr_values  <- sapply(labs, function(label) false_negative_rate(conf_matrix, label))
  
  negative_predictive_value <- function(conf_matrix, label) 
  {
    tn      <- sum(conf_matrix) - sum(conf_matrix[label, ]) - sum(conf_matrix[, label]) + conf_matrix[label, label]
    fn      <- sum(conf_matrix[label, ]) - conf_matrix[label, label]
    return(tn / (tn + fn))
  }
  
  npv_values  <- sapply(labs, function(label) negative_predictive_value(conf_matrix, label))
  
  observed_acc        <- acc
  expected_acc        <- sum(rowSums(conf_matrix) * colSums(conf_matrix)) / (n^2)
  
  kappa               <- (observed_acc - expected_acc) / (1 - expected_acc)
  se_kappa            <- sqrt((observed_acc * (1 - observed_acc)) / (n*(1 - expected_acc)^2))
  ci_kappa_low        <- kappa - z * se_kappa
  ci_kappa_high       <- min(kappa + z * se_kappa, 1)
  
  # Descrição das distâncias
  
  dist_description <- switch(dist,
                             'euclidean' = 'Euclidiana',
                             'manhattan' = 'Manhattan',
                             'minkowski' = 'Minkowski',
                             'canberra'  = 'Canberra',
                             'chebyshev' = 'Chebyshev')
  
  # Gerar texto com os resultados
  
  conf_matrix_text <- paste("Matriz de Confusão:\n",
                            paste(capture.output(print(conf_matrix)), collapse = "\n"))
  
  metrics_text <- paste(
    "Métricas:\n",
    sprintf("Distância Utilizada: %s", dist_description), "\n",
    sprintf("Acurácia: %.3f", acc), "\n",
    sprintf("Intervalo de Confiança da Acurácia: [%.3f, %.3f]", ci_low, ci_high), "\n",
    sprintf("Coeficiente Kappa: %.3f", kappa), "\n",
    sprintf("Intervalo de Confiança do Kappa: [%.3f, %.3f]", ci_kappa_low, ci_kappa_high), "\n",
    sprintf("Recall: %s", paste(sprintf("%.3f", recall_values), collapse = ", ")), "\n",
    sprintf("Especificidade: %s", paste(sprintf("%.3f", specificity_values), collapse = ", ")), "\n",
    sprintf("Precisão: %s", paste(sprintf("%.3f", precision_values), collapse = ", ")), "\n",
    sprintf("Previsão Negativa: %s", paste(sprintf("%.3f", npv_values), collapse = ", ")), "\n",
    sprintf("Taxa de Falso Positivo: %s", paste(sprintf("%.3f", fpr_values), collapse = ", ")), "\n",
    sprintf("Taxa de Falso Negativo: %s", paste(sprintf("%.3f", fnr_values), collapse = ", ")), "\n",
    sprintf("F1-Score: %s", paste(sprintf("%.3f", f1_scores), collapse = ", ")), "\n")
  
  result_text   <- paste(conf_matrix_text, metrics_text, sep = "\n\n")
  
  # Resultados como lista para compatibilidade
  
  result <- list('Distance' = dist,
                 'Accuracy' = acc,
                 'Confusion Matrix' = conf_matrix,
                 'Prediction' = as.factor(predict),
                 'Precision' = precision_values,
                 'Recall' = recall_values,
                 'Specificity' = specificity_values,
                 'F1-Score' = f1_scores,
                 'False Positive Rate' = fpr_values,
                 'False Negative Rate' = fnr_values,
                 'Negative Predictive Value' = npv_values,
                 'Confidence Interval of Accuracy' = c(Lower = ci_low, Upper = ci_high),
                 'Kappa' = kappa,
                 'Confidence Interval of Kappa' = c(Lower = ci_kappa_low, Upper = ci_kappa_high))
  
  # Mostrar a saída como texto
  
  cat(result_text)
  
  # Plotar a matriz de confusão
  
  print(conf_matrix_plot)
  
}

set.seed(123)

train_indices     <- sample(1:nrow(WineQualityWhite), size = 0.8 * nrow(WineQualityWhite))
train_data_OQ        <- WineQualityWhite[train_indices, 1:11]
test_data_OQ         <- WineQualityWhite[-train_indices, 1:11]
train_labels_OQ      <- WineQualityWhite[train_indices, 12]
test_labels_OQ       <- WineQualityWhite[-train_indices, 12]

train_labels_OQ = as.factor(train_labels_OQ$quality_class)
test_labels_OQ = as.factor(test_labels_OQ$quality_class)

KNN(train = train_data_OQ, test = test_data_OQ, tr.class = train_labels_OQ, te.class = test_labels_OQ, dist = 'euclidean', k = 5)


################# #reg
knn_reg <- knn.reg(train_data, test_data, train_labels, k = 5)
knn_predictions <- knn_reg$pred

r2 <- 1 - (sum((test_labels - knn_reg$pred)^2) / sum((test_labels - mean(test_labels))^2))

mse = mean((test_labels - knn_reg$pred)^2)

rmse <- sqrt(mse)

mae <- mean(abs(test_labels - knn_reg$pred))

mape <- mean(abs((test_labels - knn_reg$pred) / test_labels)) * 100

##############
kNN <- function(train, test, tr.class, te.class, dist = 'euclidean', k = 3, lambda)
{
  # Códigos de erro para entrada de argumentos inválidos 
  
  if(!is.data.frame(train) && !is.matrix(train))
    stop("Erro001: O objeto 'train' deve ser do tipo data.frame ou matrix.")
  
  if(!is.na(tr.class[1]))
  {
    tr.class    <- as.matrix(tr.class)
    if(nrow(train) != length(tr.class))
      stop("Erro002: Os objetos 'class' e 'train' devem ter o mesmo número de linhas.")
  }
  
  if(!(dist %in% c('euclidean', 'manhattan', 'minkowski', 'canberra', 'chebyshev')))
    stop("Erro003: 'dist' deve ser: 'euclidean', 'manhattan', 'minkowski', 'canberra', ou 'chebyshev'.")
  
  if (dist == 'minkowski' && is.null(lambda)) 
  {
    stop("Erro004: 'lambda' deve ser definido quando 'dist' é 'minkowski'.")
  }
  
  # Inicialização dos objetos
  
  nrow.tr      <- nrow(train)
  nrow.te      <- nrow(test)
  dist_matrix  <- matrix(0, nrow = nrow.te, ncol = nrow.tr) # Distâncias
  predict      <- as.character(nrow.te) # Predições
  
  # Cálculo das distâncias
  
  for (i in 1:nrow.te) 
  {
    for (j in 1:nrow.tr) 
    {
      if(dist == 'euclidean'){
        dist_matrix[i, j] <- sqrt(sum((train[j, ] - test[i, ])^2))
      } else if(dist == 'manhattan'){
        dist_matrix[i, j] <- sum(abs(train[j, ] - test[i, ]))
      } else if(dist == 'canberra'){
        dist_matrix[i, j] <- sum(abs(train[j, ] - test[i, ]) / (train[j, ] + test[i, ]))
      } else if(dist == 'chebyshev'){
        dist_matrix[i, j] <- max(abs(train[j, ] - test[i, ]))
      } else if (dist == 'minkowski'){
        dist_matrix[i, j] <- (sum(abs(train[j, ] - test[i, ])^lambda))^(1 / lambda)
      }
    }
    
    # Determinar os k vizinhos mais próximos
    
    nearest_indices <- order(dist_matrix[i, ])[1:k]
    tb.knn          <- table(tr.class[nearest_indices])
    
    # Classificação das instâncias
    
    predict[i] <- names(tb.knn[which.max(tb.knn)])
  }
  
  # Matriz de confusão   
  
  labs <- names(table(te.class))
  conf_matrix <- matrix(0, nrow = length(labs), ncol = length(labs))
  rownames(conf_matrix) <- labs
  colnames(conf_matrix) <- labs
  
  for (k in 1:length(predict))
  {
    pred <- as.character(predict[k])
    true <- as.character(te.class[k])
    conf_matrix[pred, true] <- conf_matrix[pred, true] + 1
  }
  
  # Gráfico da matriz de confusão usando ggplot2
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) 
  {
    install.packages("ggplot2")
  }
  suppressWarnings(library(ggplot2))
  
  conf_matrix_melted           <- as.data.frame(as.table(conf_matrix))
  colnames(conf_matrix_melted) <- c("Predicted", "True", "Count")
  conf_matrix_melted$FillColor <- ifelse(conf_matrix_melted$Predicted == conf_matrix_melted$True, 
                                         conf_matrix_melted$Count, 
                                         NA)
  conf_matrix_plot     <- ggplot(conf_matrix_melted, aes(x = True, y = Predicted, fill = FillColor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), color = "black") +
    scale_fill_gradient(low = "gray", high = "salmon", na.value = "white") +
    scale_y_discrete(limits = rev(labs)) + 
    labs(x = "Classes Atuais", y = "Classes Preditas", title = "Matriz de Confusão") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 45), 
          axis.title.x = element_text(margin = margin(t = 12)),
          axis.title.y = element_text(margin = margin(r = 12)),
          legend.position = "none")
  
  # Cálculo das métricas de desempenho:
  
  # - Acurácia (acc)
  # - Precisão (Ou Valor Preditivo Positivo) (precision)
  # - Valor Preditivo Negativo (negative_predictive_value)
  # - Recall (recall)
  # - Especificidade (specificity)
  # - F1-Score (f1_score)
  # - Taxa de Falso Positivo (false_positive_rate)
  # - Taxa de Falso Negativo (false_negative_rate)
  # - Coeficiente Kappa (kappa)
  # - Intervalos de Confiança para Acurácia e Kappa
  
  acc         <- sum(diag(conf_matrix)) / sum(conf_matrix) 
  n           <- length(te.class)
  z           <- qnorm(1 - 0.05 / 2)
  se          <- sqrt((acc * (1 - acc)) / n)
  ci_low      <- acc - z * se
  ci_high     <- min(acc + z * se, 1)
  
  precision   <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fp      <- sum(conf_matrix[, label]) - tp
    return(tp / (tp + fp))
  }
  
  precision_values <- sapply(labs, function(label) precision(conf_matrix, label))
  
  recall      <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fn      <- sum(conf_matrix[label, ]) - tp
    return(tp / (tp + fn))
  }
  
  recall_values <- sapply(labs, function(label) recall(conf_matrix, label))
  
  specificity <- function(conf_matrix, label) 
  {
    tn      <- sum(conf_matrix) - sum(conf_matrix[label, ]) - sum(conf_matrix[, label]) + conf_matrix[label, label]
    fp      <- sum(conf_matrix[, label]) - conf_matrix[label, label]
    return(tn / (tn + fp))
  }
  
  specificity_values <- sapply(labs, function(label) specificity(conf_matrix, label))
  
  f1_score    <- function(precision, recall) 
  {
    if(precision + recall == 0) return(0)
    return(2 * (precision * recall) / (precision + recall))
  }
  
  f1_scores   <- mapply(f1_score, precision_values, recall_values)
  
  false_positive_rate <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fp      <- sum(conf_matrix[, label]) - tp
    tn      <- sum(conf_matrix) - sum(conf_matrix[label, ]) - sum(conf_matrix[, label]) + conf_matrix[label, label]
    return(fp / (fp + tn))
  }
  
  fpr_values  <- sapply(labs, function(label) false_positive_rate(conf_matrix, label))
  
  false_negative_rate <- function(conf_matrix, label) 
  {
    tp      <- conf_matrix[label, label]
    fn      <- sum(conf_matrix[label, ]) - tp
    return(fn / (fn + tp))
  }
  
  fnr_values  <- sapply(labs, function(label) false_negative_rate(conf_matrix, label))
  
  negative_predictive_value <- function(conf_matrix, label) 
  {
    tn      <- sum(conf_matrix) - sum(conf_matrix[label, ]) - sum(conf_matrix[, label]) + conf_matrix[label, label]
    fn      <- sum(conf_matrix[label, ]) - conf_matrix[label, label]
    return(tn / (tn + fn))
  }
  
  npv_values  <- sapply(labs, function(label) negative_predictive_value(conf_matrix, label))
  
  observed_acc        <- acc
  expected_acc        <- sum(rowSums(conf_matrix) * colSums(conf_matrix)) / (n^2)
  
  kappa               <- (observed_acc - expected_acc) / (1 - expected_acc)
  se_kappa            <- sqrt((observed_acc * (1 - observed_acc)) / (n*(1 - expected_acc)^2))
  ci_kappa_low        <- kappa - z * se_kappa
  ci_kappa_high       <- min(kappa + z * se_kappa, 1)
  
  # Descrição das distâncias
  
  dist_description <- switch(dist,
                             'euclidean' = 'Euclidiana',
                             'manhattan' = 'Manhattan',
                             'minkowski' = 'Minkowski',
                             'canberra'  = 'Canberra',
                             'chebyshev' = 'Chebyshev')
  
  # Gerar texto com os resultados
  
  conf_matrix_text <- paste("Matriz de Confusão:\n",
                            paste(capture.output(print(conf_matrix)), collapse = "\n"))
  
  metrics_text <- paste(
    "Métricas:\n",
    sprintf("Distância Utilizada: %s", dist_description), "\n",
    sprintf("Acurácia: %.3f", acc), "\n",
    sprintf("Intervalo de Confiança da Acurácia: [%.3f, %.3f]", ci_low, ci_high), "\n",
    sprintf("Coeficiente Kappa: %.3f", kappa), "\n",
    sprintf("Intervalo de Confiança do Kappa: [%.3f, %.3f]", ci_kappa_low, ci_kappa_high), "\n",
    sprintf("Recall: %s", paste(sprintf("%.3f", recall_values), collapse = ", ")), "\n",
    sprintf("Especificidade: %s", paste(sprintf("%.3f", specificity_values), collapse = ", ")), "\n",
    sprintf("Precisão: %s", paste(sprintf("%.3f", precision_values), collapse = ", ")), "\n",
    sprintf("Previsão Negativa: %s", paste(sprintf("%.3f", npv_values), collapse = ", ")), "\n",
    sprintf("Taxa de Falso Positivo: %s", paste(sprintf("%.3f", fpr_values), collapse = ", ")), "\n",
    sprintf("Taxa de Falso Negativo: %s", paste(sprintf("%.3f", fnr_values), collapse = ", ")), "\n",
    sprintf("F1-Score: %s", paste(sprintf("%.3f", f1_scores), collapse = ", ")), "\n")
  
  result_text   <- paste(conf_matrix_text, metrics_text, sep = "\n\n")
  
  # Resultados como lista para compatibilidade
  
  result <- list('Distance' = dist,
                 'Accuracy' = acc,
                 'Confusion Matrix' = conf_matrix,
                 'Prediction' = as.factor(predict),
                 'Precision' = precision_values,
                 'Recall' = recall_values,
                 'Specificity' = specificity_values,
                 'F1-Score' = f1_scores,
                 'False Positive Rate' = fpr_values,
                 'False Negative Rate' = fnr_values,
                 'Negative Predictive Value' = npv_values,
                 'Confidence Interval of Accuracy' = c(Lower = ci_low, Upper = ci_high),
                 'Kappa' = kappa,
                 'Confidence Interval of Kappa' = c(Lower = ci_kappa_low, Upper = ci_kappa_high))
  
  # Mostrar a saída como texto
  
  cat(result_text)
  
  # Plotar a matriz de confusão
  
  print(conf_matrix_plot)
  
}

train_indices     <- sample(1:nrow(WineQualityWhite), size = 0.8 * nrow(WineQualityWhite))
train_data_OQ        <- WineQualityWhite[train_indices, 1:11]
test_data_OQ         <- WineQualityWhite[-train_indices, 1:11]
train_labels_OQ      <- WineQualityWhite[train_indices, 12]
test_labels_OQ       <- WineQualityWhite[-train_indices, 12]

train_labels_OQ = as.factor(train_labels_OQ$quality_class)
test_labels_OQ = as.factor(test_labels_OQ$quality_class)

train_labels = train[,12]
test_labels = test[,12]

train_labels = as.factor(train_labels$quality_class)
test_labels = as.factor(test_labels$quality_class)


train = train[,1:11]
test = test[,1:11]

kNN(train = train, test = test, tr.class = train_labels, te.class = test_labels, dist = 'euclidean', k = 5) # KNN que preciso arrumar: Falar com o Prof
