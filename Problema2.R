
iris
mis_dades <- iris

y <- mis_dades$Sepal.Length
y
x <- mis_dades$Petal.Length
x

plot(x,y)

xbar <- mean(x)
ybar <- mean(y)

m <- sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
b <- ybar - m*xbar # y = mx+ b

m*1.5 + b

mod <- lm(y~x) #altgr + 4 + space

ypredicted <- predict(mod, data.frame(x=x))

plot(x,y, pch=16, col ="red")

lines(x, ypredicted)

#coeficiente de determinación #R-squared

Rsq <- sum((ypredicted-ybar)^2)/sum((y-ybar)^2)

summary(mod)
sqrt(Rsq)
cor.test(x,y)
