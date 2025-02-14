#title: beginner data analysis
#name: Yu-Jing
#compare setosa and versicolor

setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]
virginica <- iris[iris$Species == "virginica",]

nrow(iris)
nrow(setosa)

t.test(x=setosa$Petal.Length, y=versicolor$Petal.Length)

### Challenge 1  
#Test for significant differences in petal lengths between _I. setosa_ and 
#_I. virginica_ and between _I. versicolor_ and _I. virginica_.
t.test(setosa$Petal.Length,y=virginica$Petal.Length)
t.test(versicolor$Petal.Length, y=virginica$Petal.Length)


