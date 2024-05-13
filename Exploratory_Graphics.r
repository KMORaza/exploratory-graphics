# Load the required libraries
library(ggplot2)
library(GGally)
library(reshape2)
library(MASS)      # For parallel coordinates
library(plot3D)    # For 3D plots
library(lattice)   # For trellis plots
library(hexbin)    # For hexbin plots
library(ggridges)  # For ridgeline plots
library(igraph)    # For network graphs
library(circlize)
library(datasets)
# Load the dataset
data("Orange")

# Summary of the dataset
summary(Orange)

# Scatter plot of circumference over age
plot(Orange$age, Orange$circumference, xlab = "Age (days)", ylab = "Circumference (mm)", main = "Circumference vs. Age")

# Boxplot of circumference grouped by Tree
boxplot(circumference ~ Tree, data = Orange, xlab = "Tree", ylab = "Circumference (mm)", main = "Circumference Distribution by Tree")

# Histogram of age
hist(Orange$age, xlab = "Age (days)", ylab = "Frequency", main = "Histogram of Age")

# Scatter plot matrix
pairs(Orange[, c("age", "circumference")], main = "Scatter Plot Matrix")

# Density plot of circumference
ggplot(Orange, aes(x = circumference)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Circumference (mm)", y = "Density", title = "Density Plot of Circumference")

# Density plot of age
ggplot(Orange, aes(x = age)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(x = "Age (days)", y = "Density", title = "Density Plot of Age") +
  theme_minimal()

# Calculate correlation matrix (excluding 'Tree' variable)
cor_matrix <- cor(Orange[, c("age", "circumference")])

# Create heatmap
ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Correlation Heatmap of Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pair plots
ggpairs(Orange[, c("age", "circumference")])

# Parallel coordinates
parcoord(Orange[, c("age", "circumference")])

# Andrews curves (manually)
ggplot(Orange, aes(x = seq(1, nrow(Orange)), y = age, color = circumference)) +
  geom_path() +
  labs(x = "Index", y = "Age (days)", color = "Circumference (mm)", title = "Andrews Curves")

# 3D plot
scatter3D(Orange$age, Orange$circumference, seq_along(Orange$age), colvar = NULL, pch = 16, cex = 1.5,
          xlab = "Age (days)", ylab = "Circumference (mm)", zlab = "Index",
          main = "3D Scatter Plot")

# Trellis plot
xyplot(circumference ~ age | Tree, data = Orange, type = c("p", "smooth"), layout = c(2, 3))

# Hexbin plot
hexbinplot(circumference ~ age | Tree, data = Orange, xbins = 15, main = "Hexbin Plot")

# Violin plot
ggplot(Orange, aes(x = Tree, y = circumference, fill = Tree)) +
  geom_violin(trim = FALSE) +
  labs(x = "Tree", y = "Circumference (mm)", title = "Violin Plot of Circumference by Tree")

# Ridgeline plot
ggplot(Orange, aes(x = circumference, y = Tree, fill = Tree)) +
  geom_density_ridges(scale = 0.8, alpha = 0.6) +
  labs(x = "Circumference (mm)", y = "Tree", title = "Ridgeline Plot of Circumference by Tree")

# Network graph
edges <- cbind(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 1))
graph <- graph_from_edgelist(edges, directed = FALSE)
plot(graph, layout = layout_in_circle, vertex.label = c("Tree 1", "Tree 2", "Tree 3", "Tree 4", "Tree 5"))


matrix_data <- Orange
# Create a chord diagram
chordDiagram(matrix_data)