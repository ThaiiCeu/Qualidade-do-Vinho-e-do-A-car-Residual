# Qualidade do Vinho e do Açucar Residual
# Análise da Qualidade do Vinho

Este projeto realiza uma análise da qualidade do vinho utilizando modelos de *machine learning*. A seguir, estão detalhados os objetivos, a metodologia e os resultados obtidos.

## 📚 Introdução

O vinho é uma das bebidas mais antigas e valorizadas pela humanidade, carregando um rico simbolismo cultural, social e histórico. No Brasil, a produção de vinho foi impulsionada pelos imigrantes italianos no século XIX, especialmente no Rio Grande do Sul. Hoje, o país conta com mais de 1.100 vinícolas e consome, em média, 2 litros de vinho per capita por ano.

---

## 🎯 Objetivo

O objetivo principal deste projeto é comparar o desempenho de quatro modelos de *machine learning* em tarefas de:
1. **Classificação** da qualidade do vinho.
2. **Regressão** para prever o teor de açúcar residual.

---

## 🔬 Metodologia

A análise foi dividida em quatro etapas principais:

### 1. Análise exploratória
Exploração dos dados, avaliando consistência e correlação entre variáveis químicas, físicas e a qualidade do produto.

### 2. Tratamento dos dados
Preparação dos dados no software RStudio, incluindo:
- Tratamento de valores ausentes.
- Ajustes nos tipos das variáveis.
- Normalização e padronização, quando necessário.

### 3. Modelagem
Treinamento de quatro modelos de *machine learning*:
- **Árvore de Decisão**
- **Florestas Aleatórias**
- **Redes Neurais**
- **KNN (K-Nearest Neighbors)**

### 4. Avaliação dos resultados
Comparação de métricas para classificação, como:
- `Accuracy`, `Precision`, `Recall`, `F1 Score`.

E para regressão:
- `MSE`, `RMSE`, `MAE`, `MAPE`.

---

## 📊 Análise exploratória

Foram analisadas as variáveis presentes na base de dados, incluindo medidas de posição, dispersão e gráficos de boxplot e histograma. A análise completa está disponível no **Anexo A**.

---
