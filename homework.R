#######################################
##### Домашнее задание
##### Южная Корея - KOSPI Composite Index
#######################################

# Подготовка пакетов
install.packages('BatchGetSymbols')
install.packages('plotly')
install.packages('keras')
install.packages('tensorflow')
install.packages('minimax')

library('BatchGetSymbols')
library('plotly')
library('keras')
library('tensorflow')
library('minimax')


#######################################
##### LSTM
#######################################
# Загрузка данных
tickers <- c('%5EKS11')
first.date <- Sys.Date() - 360*5
last.date <- Sys.Date()

yts <- BatchGetSymbols(tickers = tickers,
                       first.date = first.date,
                       last.date = last.date,
                       cache.folder = file.path(tempdir(),
                                                'BGS_Cache') )

# Подготовка данных
y <-  yts$df.tickers$price.close
myts <-  data.frame(index = yts$df.tickers$ref.date, price = y, vol = yts$df.tickers$volume)
myts <-  myts[complete.cases(myts), ]
myts <-  myts[-seq(nrow(myts) - 1200), ]
myts$index <-  seq(nrow(myts))

# График
plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol)

# Стандартизация
msd.price <-  c(mean(myts$price), sd(myts$price))
msd.vol <-  c(mean(myts$vol), sd(myts$vol))
myts$price <-  (myts$price - msd.price[1])/msd.price[2]
myts$vol <-  (myts$vol - msd.vol[1])/msd.vol[2]
summary(myts)

# Деление на тестовую и тренировочную
datalags = 20
train <-  myts[seq(1000 + datalags), ]
test <-  myts[1000 + datalags + seq(200 + datalags), ]
batch.size <- 50

# Создание массивов
x.train <-  array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags, 2))
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags, 1))

x.test <-  array(data = lag(cbind(test$vol, test$price), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags, 2))
y.test <-  array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags, 1))

#####
##### LSTM, adam, mse
#####
model <- keras_model_sequential()  %>%
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'adam')
model %>% fit(x.train, y.train, epochs = 5, batch_size = batch.size)
las <- 0.1708

#####
##### LSTM, adam, mape
#####
model <- keras_model_sequential()  %>%
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mape', optimizer = 'adam')
model %>% fit(x.train, y.train, epochs = 5, batch_size = batch.size)
lap <- 95.2883

#####
##### LSTM, rmsprop, mse
#####
model <- keras_model_sequential()  %>%
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = 'rmsprop')
model %>% fit(x.train, y.train, epochs = 10, batch_size = batch.size)
lrs <- 0.1870

#####
##### LSTM, rmsprop, mape
#####
model <- keras_model_sequential()  %>%
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mape', optimizer = 'rmsprop')
model %>% fit(x.train, y.train, epochs = 10, batch_size = batch.size)
lrp <- 101.4188











#######################################
##### RNN
#######################################

# Загрузка данных
tickers <- c('%5EKS11')
first.date <- Sys.Date() - 360*5
last.date <- Sys.Date()

yts <- BatchGetSymbols(tickers = tickers,
                       first.date = first.date,
                       last.date = last.date,
                       cache.folder = file.path(tempdir(),
                                                'BGS_Cache') )

# Подготовка данных
y <-  yts$df.tickers$price.close
myts <-  data.frame(index = yts$df.tickers$ref.date, price = y, vol = yts$df.tickers$volume)
myts <-  myts[complete.cases(myts), ]
myts <-  myts[-seq(nrow(myts) - 1200), ]
myts$index <-  seq(nrow(myts))

# Стандартизация минимакс
myts <- data.frame(index = rminimax(myts$index), price = rminimax(myts$price), vol= rminimax(myts$vol))
myts

# Деление на тестовую и тренировочную
datalags = 20
train <-  myts[seq(1000 + datalags), ]
test <-  myts[1000 + datalags + seq(200 + datalags), ]
batch.size <- 50

# Создание массивов
x.train <-  array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags))
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags))

x.test <-  array(data = lag(cbind(test$vol, test$price), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags))
y.test <-  array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags))

#####
##### RNN, adam, mse
#####
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 50) %>%
  layer_simple_rnn(units = 50) %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = "adam",
  loss = "mse",
)

history <- model %>% fit(
  x.train, y.train,
  epochs = 5,
  batch_size = 50,
  validation_split = 0.2
)
ras <- 0.0783

#####
##### RNN, adam, mape
#####
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 50) %>%
  layer_simple_rnn(units = 50) %>%
  layer_dense(units = 2, activation = "sigmoid")
model %>% compile(
  optimizer = "adam",
  loss = "mape",
)

history <- model %>% fit(
  x.train, y.train,
  epochs = 5,
  batch_size = 50,
  validation_split = 0.2
)
rap <- 95.9025

#####
##### RNN, rmsprop, mse
#####
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 50) %>%
  layer_simple_rnn(units = 50) %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
)

history <- model %>% fit(
  x.train, y.train,
  epochs = 5,
  batch_size = 50,
  validation_split = 0.2
)
rrs <- 0.0782

#####
##### RNN, rmsprop, mape
#####
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 50) %>%
  layer_simple_rnn(units = 50) %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "mape",
)

history <- model %>% fit(
  x.train, y.train,
  epochs = 5,
  batch_size = 50,
  validation_split = 0.2
)

rrp <- 96.4934








#######################################
##### SM
#######################################
tickers <- c('%5EKS11')
first.date <- Sys.Date() - 360*5
last.date <- Sys.Date()

# Загрузка данных
yts <- BatchGetSymbols(tickers = tickers,
                       first.date = first.date,
                       last.date = last.date,
                       cache.folder = file.path(tempdir(),
                                                'BGS_Cache') )

# Подготовка данных
y <-  yts$df.tickers$price.close
myts <-  data.frame(index = yts$df.tickers$ref.date, price = y, vol = yts$df.tickers$volume)
myts <-  myts[complete.cases(myts), ]
myts <-  myts[-seq(nrow(myts) - 1200), ]
myts$index <-  seq(nrow(myts))

# Стандартизация минимакс
myts <- data.frame(index = rminimax(myts$index), price = rminimax(myts$price), vol= rminimax(myts$vol))
myts

# Деление на тестовую и тренировочную
datalags = 20
train <-  myts[seq(1000 + datalags), ]
test <-  myts[1000 + datalags + seq(200 + datalags), ]
batch.size <- 50

# Создание массивов
x.train <-  array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags))
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags))

x.test <-  array(data = lag(cbind(test$vol, test$price), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags))
y.test <-  array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags))


#####
##### SM, adam, mse
#####
sm <- keras_model_sequential() %>%
  layer_dense(units = 1000, activation = 'relu') %>%
  layer_dense(units = 5, activation = 'sigmoid')

sm %>% compile(
  optimizer = 'adam',
  loss = 'mse')
sm %>% fit(x.train, y.train, epochs = 5, batch_size = 50)
sas <- 0.0839


#####
##### SM, adam, mape
#####
sm <- keras_model_sequential() %>%
  layer_dense(units = 1000, activation = 'relu') %>%
  layer_dense(units = 5, activation = 'sigmoid')

sm %>% compile(
  optimizer = 'adam',
  loss = 'mape')
sm %>% fit(x.train, y.train, epochs = 5, batch_size = 50)
sap <- 98.4158

#####
##### SM, rmsprop, mse
#####
sm <- keras_model_sequential() %>%
  layer_dense(units = 1000, activation = 'relu') %>%
  layer_dense(units = 5, activation = 'sigmoid')

sm %>% compile(
  optimizer = 'rmsprop',
  loss = 'mse')
sm %>% fit(x.train, y.train, epochs = 5, batch_size = 50)
srs <- 0.0831

#####
##### SM, rmsprop, mape
#####
sm <- keras_model_sequential() %>%
  layer_dense(units = 1000, activation = 'relu') %>%
  layer_dense(units = 5, activation = 'sigmoid')

sm %>% compile(
  optimizer = 'rmsprop',
  loss = 'mape')
sm %>% fit(x.train, y.train, epochs = 5, batch_size = 50)
srp <- 98.2407


#######################################
##### Таблица
#######################################

mse <- c(srs,sas,rrs,ras,lrs,las)
mape <- c(srp,sap,rrp,rap,lrp,lap)

final <- data.frame('NN' = c(rep('SM', 2), rep('RNN', 2), rep('lstm', 2)), 
                'optimizer' = rep(c('rmsprop', 'adam'), 3),
                'MSE' = mse,
                'MAPE' = mape)
final
