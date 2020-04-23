# Включаем библиотеки
library('keras')
library('tensorflow')

# Грузим данные из keras
mnist <- dataset_mnist()

# Выделим тренировочные списки с векторами
train_images <- mnist$train$x
train_labels <- mnist$train$y
# Выделим тестовые списки с векторами
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Определяем слои
network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = 'relu', input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation = 'softmax')

# Проводим дополнительную настройку, добавляя оптимизатор, перекрёстную энтропию и вывод точности
network %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# Преобразуем матрицы
train_images <- array_reshape(train_images, c(60000, 28*28))
train_images <- train_images/255
test_images <- array_reshape(test_images, c(10000, 28*28))
test_images <- test_images/255

# Cоздадим категории для ярлыков
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)



# Тренируем сеть
network %>% fit(train_images, train_labels, epochs = 30, batch_size = 128)
# В среднем точность в 95% достигается уже во второй эпохе,
# 99% точность достигается в 6 эпохе.
# В 26-28 эпохах точность достигает 100%

metric <- network %>% evaluate(test_images, test_labels)
metric
# Точность тестовой выборки 98,25-98,31%



# Получим для сравнений тестовые ярлыки значений
test_labels1 <- mnist$test$y

# Первые 10 значений тестовой выборки
firp <- network %>% predict_classes(test_images[1:10,])
firr <- test_labels1[1:10]
# Внесём в таблицу предсказанные и реальные значения первых 10 значений
FPRED <- data.frame(FirP = firp, FirR = firr)
# Значения совпали

# Последние 10 значений тестовой выборки
lastp <- network %>% predict_classes(test_images[9991:10000,])
lastr <- test_labels1[9991:10000]
# Внесём в таблицу предсказанные и реальные значения последних 10 значений
LPRED <- data.frame(LastP = lastp, LastR = lastr)
# Значения совпали



# Подготавливаем почву для вывода изображений последних 10 чисел
i1 <- mnist$test$x[9991, 1:28, 1:28]
i2 <- mnist$test$x[9992, 1:28, 1:28]
i3 <- mnist$test$x[9993, 1:28, 1:28]
i4 <- mnist$test$x[9994, 1:28, 1:28]
i5 <- mnist$test$x[9995, 1:28, 1:28]
i6 <- mnist$test$x[9996, 1:28, 1:28]
i7 <- mnist$test$x[9997, 1:28, 1:28]
i8 <- mnist$test$x[9998, 1:28, 1:28]
i9 <- mnist$test$x[9999, 1:28, 1:28]
i10 <- mnist$test$x[10000, 1:28, 1:28]

# Кайфуем
image(as.matrix(i1))
image(as.matrix(i2))
image(as.matrix(i3))
image(as.matrix(i4))
image(as.matrix(i5))
image(as.matrix(i6))
image(as.matrix(i7))
image(as.matrix(i8))
image(as.matrix(i9))
image(as.matrix(i10))
