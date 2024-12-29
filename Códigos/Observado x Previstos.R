library(gridExtra)

# Função para criar gráficos
create_plot <- function(real, predicted, model_name) {
  ggplot(data.frame(Real = real, Predicted = predicted), aes(x = Real, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggtitle(model_name) +
    xlab("Valores Reais") +
    ylab("Valores Previstos") +
    theme_minimal()
}

# Criar os gráficos
plot_knn <- create_plot(test_labels, knn_predictions, "KNN")
plot_nn <- create_plot(test_labels, pred_nn_reg, "Rede Neural")
plot_rf <- create_plot(test_labels, pred_rf_reg, "Random Forest")
plot_tree <- create_plot(test_labels, pred_tree_reg, "Árvore de Decisão")

# Combinar os gráficos em uma única imagem
grid.arrange(plot_knn, plot_nn, plot_rf, plot_tree, ncol = 2)
