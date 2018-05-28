colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
title("1NN")      ## Отобразим выборку, используя два последних признака таблицы:

euclideanDistance <- function(u, v){
  sqrt(sum((u - v)^2))
}                             ## формула обычное евклидово расстояние

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1                        ## Сортируем объекты согласно расстояния до объекта z
  
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l){
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }                                           ## Создаём матрицу расстояний
  
  orderedXl <- xl[order(distances[, 2]), ]
  
  return (orderedXl);
} ## Сортируем

kNN <- function(xl, z, k){                     ## Применяем метод kNN
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1                 ## Сортируем выборку согласно классифицируемого объекта       
  classes <- orderedXl[1:k, n + 1]          ## Получаем классы первых k соседей
  counts <- table(classes)                    ## Составляем таблицу встречаемости каждого класса
  class <- names(which.max(counts))
  
  return (class)
}                                       ## Находим класс, который доминирует среди первых k соседей

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
= colors[iris$Species], asp = 1)            ## Рисуем выборку

z <- c(2.7, 1)
xl <- iris[, 3:5]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)   ## Классификация одного заданного объекта
