library(pROC)
library(ggplot2)

# Probabilidades preditas de cada modelo (assumindo que os modelos estão treinados)
# Árvore de Decisão
pred_probs_tree <- predict(tree_model, test[, 1:11], type = "prob")[, 2]

# Floresta Aleatória
pred_probs_rf <- predict(rf_model, test[, 1:11], type = "prob")[, 2]

# Redes Neurais
pred_probs_nn <- predict(nn_model, test[, 1:11], type = "raw")[, 1]

# KNN (não retorna diretamente as probabilidades, pode usar normalização manual se necessário)
knn_probs <- attr(knn(train[, 1:11], test[, 1:11], cl = train$quality_class, k = 5, prob = TRUE), "prob")
pred_probs_knn <- ifelse(knn_model == "bom", knn_probs, 1 - knn_probs)

# Classes verdadeiras (1 para "bom", 0 para "ruim")
true_labels <- ifelse(test$quality_class == "ruim", 1, 0)

# Gerar objetos ROC
roc_tree <- roc(true_labels, pred_probs_tree, levels = c(0, 1), direction = "<")
roc_rf <- roc(true_labels, pred_probs_rf, levels = c(0, 1), direction = "<")
roc_nn <- roc(true_labels, pred_probs_nn, levels = c(0, 1), direction = "<")
roc_knn <- roc(true_labels, pred_probs_knn, levels = c(0, 1), direction = "<")

# Plotar todas as curvas ROC juntas
plot(roc_tree, col = "red", lwd = 2, main = "Curvas ROC - Modelos de Classificação")
plot(roc_rf, col = "blue", lwd = 2, add = TRUE)
plot(roc_nn, col = "green", lwd = 2, add = TRUE)
plot(roc_knn, col = "purple", lwd = 2, add = TRUE)

# Adicionar legenda ajustada com tamanho menor
legend("bottomright", 
       legend = c("Árvore de Decisão", "Floresta Aleatória", "Redes Neurais", "KNN"),
       col = c("red", "blue", "green", "purple"), 
       lwd = 2, 
       cex = 0.6,  # Ajusta o tamanho do texto (0.8 para menor)
       box.lwd = 0)  # Remove a borda da legenda


# Exibir as AUC (Área Sob a Curva) de cada modelo
cat("Árvore de Decisão - AUC:", auc(roc_tree), "\n")
cat("Floresta Aleatória - AUC:", auc(roc_rf), "\n")
cat("Redes Neurais - AUC:", auc(roc_nn), "\n")
cat("KNN - AUC:", auc(roc_knn), "\n")
