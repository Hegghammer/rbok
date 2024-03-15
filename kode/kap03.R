#-----------------------------------
# Kode til kapittel 3 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

install.packages("flametree")

library(flametree)

tre <- flametree_grow()

flametree_plot(tre)

trær <- flametree_grow(time = 10, trees = 5)

flametree_plot(trær)

print("Hello world!")

10 + 10

mtcars

plot(mtcars$hp, mtcars$mpg)

plot(mtcars$hp, mtcars$mpg, col = "red")

print("Hello world!")

print("Hello world!")
10 + 10
mtcars
plot(mtcars$hp, mtcars$mpg)
plot(mtcars$hp, mtcars$mpg, col = "red")

print(
             "Hello world!")

plot(x = mtcars$hp,

y = mtcars$mpg)

# Dette er en kommentar. Den vil bli ignorert av R.

print("Hello world!") # Enda en

install.packages("remotes")
library(remotes)
install_github("hegghammer/rforalle")

library(rforalle)
hent_kode("kap03.R")

remove.packages("flametree")
