setwd('C:/Users/LENOVO/Desktop/Big Data & Bussines Analytics')
data = read.csv('data_tc.csv', row.names = 1)
library(tidyverse)
# write.csv(data, file ='data_tc_final.csv')

data2 = data[,c(1:20,24:50)]

ejemplo2 = lm(HAA.S80 ~ ., data2)
summary(ejemplo2)

productividad = mutate(data,
                       (cump.produc = (HAA.S80 >= 359.7)))
productividad$`(cump.produc = (HAA.S80 >= 359.7))`

data3 = productividad[,c(1:20, 24:50)]

levante2 = lm(Peso.Hvo..Prom.Acu..S80 ~ Peso.Ave.S4 + Peso.Ave.S10 + Peso.Ave.S14 + 
                X..Uniformidad.S4 + Cosm.Acum.S10, data3)
summary(levante2)

produccion2 = lm(Peso.Hvo..Prom.Acu..S80 ~ gr.ave.dia..S25 + gr.ave.dia..S40 + 
                   gr.ave.dia..S72 + Mort.selcc.Acu..S25 + Mort.selcc.Acu..S40 +
                   Mort.selcc.Acu..S72 + G.P.S25, data3)
summary(produccion2)

mortalidad = lm(Mort.selcc.Acu..S80 ~ Peso.Ave.S4 + Peso.Ave.S10 + Peso.Ave.S14 + 
                  X..Uniformidad.S4 + Cosm.Acum.S10 + Cosm.Acum.S4 + 
                  Cosm.Acum.S14, data3)
summary(mortalidad)


data %>% ggplot(aes(Sistema.Produccion, HAA.S72, fill = Sistema.Produccion)) + 
  geom_boxplot() + labs(title = 'Produccion de huevos de acuerdo al clima de producción',
                        x = 'Sistema de producción',
                        y = 'Huevos por ave alojada en la semana 72',
                        fill = 'Sistema de producción'
  ) + theme_bw()

data %>% ggplot(aes(Sistema.Produccion, HAA.S80, fill = Sistema.Produccion)) + 
  geom_boxplot() + labs(title = 'Produccion de huevos de acuerdo al clima de producción',
                        x = 'Sistema de producción',
                        y = 'Huevos por ave alojada en la semana 80',
                        fill = 'Sistema de producción'
  ) + theme_bw()


data %>% ggplot(aes(Clima.Produccion, HAA.S72, fill = Clima.Produccion)) + 
  geom_boxplot() + labs(title = 'Produccion de huevos de acuerdo al clima de producción',
                        x = 'Clima de producción',
                        y = 'Huevos por ave alojada en la semana 72',
                        fill = 'Clima de producción'
  ) + theme_bw()

data %>% ggplot(aes(Clima.Produccion, HAA.S80, fill = Clima.Produccion)) + 
  geom_boxplot() + labs(title = 'Produccion de huevos de acuerdo al clima de producción',
                        x = 'Clima de producción',
                        y = 'Huevos por ave alojada en la semana 80',
                        fill = 'Clima de producción'
  ) + theme_bw()



data %>% ggplot(aes(HAA.S80, Peso.Ave.S10, shape = Clima.Produccion, 
                    colour = Sistema.Produccion)) + 
  geom_point() + 
  labs(title = 'Variables influyentes en la producción de huevos',
       x = 'Huevos por ave alojada en la semana 80',
       y = 'Peso en gramos en la Semana 10',
       colour = 'Sistema de producción',
       shape = 'Clima de producción'
  ) + theme_bw()

# data$Sistema.Produccion[data$Sistema.Produccion == "NA"] <- NA

library(dummies)
df.dummy = dummy.data.frame(data)
df.dummy = data.frame(df.dummy)

produccion3 = lm(Peso.Hvo..Prom.Acu..S80 ~ Sistema.ProduccionBatería.Ambiente + 
                   Sistema.ProduccionBatería.Automática + 
                   Sistema.ProduccionJaula.Tradicional + Sistema.ProduccionPiso + 
                   Sistema.ProduccionTemp + Clima.ProduccionCálido + 
                   Clima.ProduccionFrío + Clima.ProduccionMedio, df.dummy)

summary(produccion3)

# Decision tree
library(rpart)
library(rpart.plot)
library(caret)
set.seed(1234)

levante.df = productividad[,c(1:20,22,53)]
produc.df = productividad[,c(24:27,32:35,41,45:48,51:53)]

train = sample_frac(levante.df, .7)
test = setdiff(levante.df, train)
tree = rpart(`(cump.produc = (HAA.S80 >= 359.7))` ~ ., train, method = 'class')
tree
rpart.plot(tree, extra = 106)
tree$variable.importance

pred = predict(tree, test, type = 'class')
table_l = table(test$`(cump.produc = (HAA.S80 >= 359.7))`, pred)
confusionMatrix(table_l, positive = 'TRUE')


set.seed(1234)
train_P = sample_frac(produc.df, .7)
test_P = setdiff(produc.df, train_P)
tree_P = rpart(`(cump.produc = (HAA.S80 >= 359.7))` ~ ., train_P, method = 'class')
tree_P
rpart.plot(tree_P, extra = 106)

pred_P = predict(tree_P, test_P, type = 'class')
table_P = table(test_P$`(cump.produc = (HAA.S80 >= 359.7))`, pred_P)
confusionMatrix(table_P, positive = 'TRUE')

# random forest

library(randomForest)
library(caret)
library(rsample)

levante2.df = na.omit(levante.df)
produc2.df = na.omit(produc.df)

# levante
levante2.df$`(cump.produc = (HAA.S80 >= 359.7))` = as.factor(levante2.df$`(cump.produc = (HAA.S80 >= 359.7))`)

set.seed(1234)
train.rf = sample_frac(levante2.df, .7)
test.rf = setdiff(levante2.df, train.rf)
rf_levante = randomForest(`(cump.produc = (HAA.S80 >= 359.7))` ~ ., train.rf,importance=TRUE,
                          proximity=TRUE)
varImpPlot(rf_levante)

pred.rf.levante = predict(rf_levante, test.rf)
table.rf.levante = table(test.rf$`(cump.produc = (HAA.S80 >= 359.7))`, pred.rf.levante)

confusionMatrix(table.rf.levante, positive = 'TRUE')

# PRODUCTIVIDAD
produc2.df$`(cump.produc = (HAA.S80 >= 359.7))` = as.factor(produc2.df$`(cump.produc = (HAA.S80 >= 359.7))`)

set.seed(1234)
train.rf = sample_frac(produc2.df, .7)
test.rf = setdiff(produc2.df, train.rf)
rf_produccion = randomForest(`(cump.produc = (HAA.S80 >= 359.7))` ~ ., train.rf,importance=TRUE,
                             proximity=TRUE)
varImpPlot(rf_produccion)

pred.rf.produccion = predict(rf_produccion, test.rf)
table.rf.produccion = table(test.rf$`(cump.produc = (HAA.S80 >= 359.7))`, pred.rf.produccion)


confusionMatrix(table.rf.levante, positive = 'TRUE')

confusionMatrix(table.rf.produccion, positive = 'TRUE')