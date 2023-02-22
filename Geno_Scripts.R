# # # # # SCRIPTS DE GENÓMICA # # # # #

# # # # # # # # # I G R A P H # # # # # # # # #
library(igraph)
g <- make_empty_graph(n=5, directed = T)
# Hace una red vacia, "n" es el numero de nodos, y "directed" pregunta si sera
# una red dirigida o no.

V(g)$color = "yellow"
# "V" de vertex, se indica el objeto al que se le va a poner el color. 

V(g)$shape = "sphere"  
# "V" de vertex, se indica el objeto al que se le va a poner la forma. 
plot(g)
# Imprime el objeto. 

g <- add.edges(g, c(1,2, 1,3, 2,4, 3,4, 4,5))
plot(g)
# Agnade conexiones entre nodos. 

g <- add.vertices(g, 1, color="red", shape="sphere")
# Sirve para agandir nuevos nodos a la red ya generada.
plot(g)

g <- add.edges(g, c(3,6, 6,5))
plot(g)
# Se le agnadieron conexiones al nuevo nodo.

class(g)
# La clase de objetos que se generan con el paquete son de clase "igraph".

g <- delete.edges(g, c(2))
g <- add.edges(g,c(3,1))
plot(g)
# Aqui se reemplazo la conexion que habái de 1 a 3, siendo ahora de 3 a 1.

V(g)$name <- LETTERS[1:6]
plot(g)
# Se cambiaron los números por letras, van en orden alfabetico.

E(g)

# # # # # D E G R E E # # # # # #

# Degree es el numero de interacciones o conexiones que tiene un nodo con otro nodo. 
# En caso de que sea una red dirigida, solo cuentan aquellas interacciones que lleven a algo. 
degree(g)

plot(g, layout=layout_nicely, vertex.size=degree(g, V(g), "in")*15+15,
     vertex.label.dist=0.5, edge.arrow.size=0.5)
# Es posible graficar la red donde el tamagno de los nodos sea proporcional a las
# conexiones que tiene.

# # # D E G R E E  D I S T R I B U T I O N # # # 

# El degree distribution se define como la fraccion de nodos en una red. Se represente como 
# P(k); si hay n nodos en total y nk de ellos tieren un degree k, se tiene P(k)=nk/n. 
# Por ejemplo, si hubiera una red con 10 nodos totales y uno se conecta 4 veces, sería 
# P(k)=nk/n -> p(k)=4/10
# Generalmente se hacen histogramas.

plot(degree_distribution(g), main="Degree distribution", xlab="Degree", ylab="Frequency")
# De esta manera se puede graficar el degree distribution .

hist(degree(g),col="salmon")
# De esta manera se puede hacer un histograma.

g1 <- as.matrix(get.adjacency(g))
g1
# También se puede hacer una matriz de adyacencia, estas estan llenas de 0 y 1 donde 
# el 0 representa 0 conectividad y el 1 representa conectividad.

heatmap(g2, Rowv=NA, Colv="Rowv")
# Es posible hacer un heatmap de la matriz de adyacencia previamente generada.

# # # star (make_star()) # # #
# El comando "make_star" hace una estrella, primero va el numero de nodos, el modo
# es para poner si la red va dirigida o no, si va en un solo sentido o es mutua la
# interaccion, y por ultimo, el centro elige que nodo va en medio. 
ca <- make_star(6, mode = "undirected")
ca
plot(ca)
# 1 es el nodo central y es una red no dirigida.

ca1 <- make_star(6, mode = "out")
ca1
plot(ca1)
# 1 es el nodo central y es una red dirigida donde 1 se conecta a todos, pero ninguno 
# se conecta con otros nodos ni con 1 de vuelta.

ca2 <- make_star(6, mode = "in")
ca2
plot(ca2)
# 1 es el centro y nuevamente es una red dirigda donde todos los nodos llevan a 1 pero 
# la conexion no es de regreso.

ca3 <- make_star(6, mode = "mutual")
ca3
plot(ca3)
# 1 es el centro, y es una red dirigida donde 1 conecta con todos y todos se conectan a 1
# de vuelta, pero no hay mas interacciones.

ca4 <- make_star(6, mode = "out", center = 6)
ca4
plot(ca4)
# Aqui 6 es el centro y es una red dirigida del centro a las puntas de la estrella.

# # # ring (make_ring()) # # #
# El comando "make_ring" crea un anillo de una dimension. Primero va el numero de nodos,
# luego si va dirigida "directed = " con TRUE o FALSE, si las conexiones seran mutuas 
# "mutual =", y si es circular con TRUE o False "circular = "
ca5 <- make_ring(6, directed = T, mutual = F, circular = T)
ca5
plot(ca5)
# Aqui se ve como estan dirigidas y no son mutuas las conexiones, ademas de ser circular.

ca6 <- make_ring(6, directed = F, mutual = T, circular = T)
ca6
plot(ca6)
# Aqui se ve que no esta dirigida pero son mutuas, al no ser circular pierde completamente
# el sentido de usar el comando "make_ring"

# # # tree (make_tree()) # # #
# El comando "make_tree" crea arboles. Primero va el numero de nodos, despues se le agrega 
# el numero de hijos que tendra "children = ", y por ultimo el modo, tiene 3 modos: 
# "in, out, undirected". 
ca7 <- make_tree(6, children = 2, mode = "out")
ca7
plot(ca7)
# En este arbol podemos ver que la separacion apunta de la madre a los hijos, es una 
# red dirigida hacia afuera.

ca8 <- make_tree(6, children = 2, mode = "in")
ca8
plot(ca8)
# En este arbol podemos ver que en la separacion, la red va hacia dentro, de los hijos a 
# a la madre. Es una red dirigida hacia adentro.

ca9 <- make_tree(6, children = 2, mode = "undirected")
ca9
plot(ca9)
# En este ultimo caso, es una red no dirigida, así que no hay mucho que decir.

# # # lattice (make_lattice()) # # #
# Es un comando que permite crear redes de dimensiones arbitrarias, periodicas o 
# aperiodicas. Tiene 2 formas, en la primera solo proporciona el "dimvector", pero 
# no la longitud ni el dim; en la segunda omite el "dimvector" pero proporciona la 
# longitud y el dim. El "dim" es la dimensión de la red; "dimvector" es un vector del tamagno
# de la red en cada dimensión.
d <- make_lattice(
  dimvector = NULL,
  length = NULL,
  dim = NULL,
  nei = 1,
  directed = FALSE,
  mutual = FALSE,
  circular = FALSE
)
d
plot(d)

# # # kautz (make_kautz_graph()) # # #
# El comando "make_kautz_graph" se encarga de hacer un grafico de kautz, el cual es un 
# grafico dirigido de grado y dimension, que tiene vertices etiquetados por todas las
# posibles cadenas de longitud que se componen de caracteres elegidos de un alfabeto que
# contiene simbolos distintos, sujeto a la condicion de que los caracteres adyacentes en 
# la cadena no pueden ser iguales. Primero va "m"que es el tamagno del alfabeto, despues
# va "n" que es el largo de las etiquetas. 
d1 <- make_kautz_graph(2,2)
d1
V(d1)$name <- LETTERS[1:length(d)]
plot(d1)

# # PATHS, SHORTEST PATHS, DISTANCES AND DIAMETER # #
library(igraph)


f <- random.graph.game(40, 0.20)
f
plot(f)

# random.graph.game lo que hace es generar modelos aleatorios acorde al modelo 
# Erdös- Rényi, cada nodo es creado con la mismo probabilidad de mantenerse constante.
# El primer número es el número de nodos, y el segundo es la probabilidad de conectarse.

sp <- shortest.paths(f)
sp
# Ya sea para nodos dirigidos o no dirigidos, lo que hace es calcular el largo  
# del camino mas corto de la red. 

f1 <- barabasi.game(40, directed = T)
plot(f1)

f2 <- barabasi.game(40, directed = F)
plot(f2)

f3 <- barabasi.game(40, power = 1)
plot(f3)

#Usar 
