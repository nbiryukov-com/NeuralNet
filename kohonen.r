install.packages("kohonen")
library('kohonen')
install.packages("boot")
library('boot')
data('brambles')
head(brambles)
age <- brambles$age

set.seed(1)

som.brambles <- som(scale(brambles), grid = somgrid(5, 5, 'hexagonal'))
som.brambles
dim(getCodes(som.brambles))

plot(som.brambles, main = 'Brambles data Kohonen SOM')

graphics.off()
par(mfrow = c(1,1))
plot(som.brambles, type = 'changes', main = 'brambles')

train <- sample(nrow(brambles), 800)
X_train <- scale(brambles[train,])
X_test <- scale(brambles[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_data <- list(measurements = X_train,
                   age = age[train])
test_data <- list(measurements = X_test,
                  age = age[-train])

mygrid <- somgrid(5, 5, 'hexagonal')

som.brambles <- supersom(train_data, grid = mygrid)                

som.predict <- predict(som.brambles, newdata = test_data)
table(age[-train], som.predict$predictions[['age']])
map(som.brambles)

plot(som.brambles, main = 'Brambles data Kohonen SOM')
