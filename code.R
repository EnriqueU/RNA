library("neuralnet", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("grid", lib.loc="/usr/lib/R/library")
library("MASS", lib.loc="/usr/lib/R/library")

# ENTRENADO LA RED PARA LA FUNCION LÓGICA OR
or = read.csv("OR.csv")
or
or$x1 = c(or$z==1)
or$x0 = c(or$z==0)
or$z = NULL
or

inet <- neuralnet(x1 + x0 ~ x + y, or, hidden=1, lifesign="full")
plot(inet,rep="best")

# ENTRENADO LA RED PARA LA FUNCION LÓGICA AND
and = read.csv("AND.csv")
and
and$x1 = c(and$z==1)
and$x0 = c(and$z==0)
and$z = NULL
and

inet <- neuralnet(x1 + x0 ~ x + y, and, hidden=1, lifesign="full")
plot(inet,rep="best")

# ENTRENADO LA RED PARA LA FUNCION LÓGICA XOR
xor = read.csv("XOR.csv")
xor
xor$x1 = c(xor$z==1)
xor$x0 = c(xor$z==0)
xor$z = NULL
xor

inet <- neuralnet(x1 + x0 ~ x + y, xor, hidden=2, lifesign="full")
plot(inet,rep="best")

# ENTRENADO LA RED PARA LA BASE DE DATOS IRIS(SUPERVISADO)
# De las 150 linea elije 100 columnas aleatorias
itrain <- iris[sample(1:150,100),]
itrain $ setosa<- c( itrain $Species =='setosa')
itrain $ versicolor <- c( itrain $Species =='versicolor')
itrain $ virginica <- c( itrain $Species == 'virginica')

itrain
itrain $ Species <- NULL
itrain

inet <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, itrain, hidden=3, lifesign="full")

# VERIFICANDO
predict <- compute(inet, iris[1:4])
#predict
which.max(predict$net.result[1,])
result<-0
for (i in 1:150) { result[i] <- which.max(predict$net.result[i,]) }

for (i in 1:150) { if (result[i]==1) {result[i] = "setosa"} }
for (i in 1:150) { if (result[i]==2) {result[i] = "versicolor"} }
for (i in 1:150) { if (result[i]==3) {result[i] = "virginica"} }

comparison <- iris
comparison$Predicted <- result
comparison

# ENTRENADO LA RED PARA LA BASE DE DATOS IRIS(NO SUPERVISADO)

library("kohonen", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library(kohonen)
i = iris
set.seed(7)
i$Species=NULL
i.sc = scale(i)
i.som = som(data = i.sc,grid = somgrid(5,5,topo = "hexagonal"))

plot(i.som,main = "Datos de la tabla iris",type="codes")
plot(i.som,main = "Datos de la tabla iris",type="changes")
plot(i.som,main = "Datos de la tabla iris",type="counts")
plot(i.som,main = "Datos de la tabla iris",type="dist.neighbours")