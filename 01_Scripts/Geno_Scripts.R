# # # # # SCRIPTS DE GENÓMICA # # # # #

# # # # # # # # # I G R A P H # # # # # # # # #
  # # # # 15 / F E B R E R O / 2023 # # # #
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
      # # # # 15 / F E B R E R O / 2023 # # # #
library(igraph)

# "shortest_paths" o "shortest.path" calcula el camino mas corto entre el nodo fuente 
# hasta el nodo deseado.
# "distance" calcula el camino mas corte de un par de nodos a otro.
# "mean_distance" calcula el camino promedio en un grafo, lo hace calculando los caminos
# mas cortos de todos los nodos.

e <- random.graph.game(40, 0.20)
e
plot(e)
# Genera modelos donde cada nodo tenga la miisma constante de conexiones. Son modelos 
# aleatorios acorde al modelo Erdos-Rényi. El primer valor es el numero de nodos, mientras
# que el segundo es la probabilidad de conectasre.

e1 <- barabasi.game(25, directed = F)
e1
plot(e1)
# El comando "barabasi.game" crea redes a través de modelos estocasticos. El primer valor
# indica el numero de nodos, y el segundo si va dirigida o no. Tiene muchos mas usos la funcion. 

e2 <- barabasi.game(100, power = 1)
layout <- layout.fruchterman.reingold(e2)
plot(e2, layout = layout, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.5)
# En esta ocasion, "power" se usa para denotar el poder de que haya preferencia hacia cierto
# nodo. 

e3 <- random.graph.game(15, 0.5)
plot(e3)
e4 <- shortest.paths(e3)
e4
# Calcula la distancia entre un nodo y otro. Lo arroja como una matriz de distancias.
heatmap(e4)
# Un heatmap de las distancias.

e5 <- shortest_paths(e3, 4,6)
e5
# En el primer argumento se coloca el objeto donde esta la red, y en el segundo se coloca
# del nodo inicial al nodo final. 

diameter(e3, directed = F, unconnected = F, weights = NULL)
# El diametro de un grafo es la distanci amaxima que hay entre cualquier  par de nodos. 
# Primero se pone el grafo a analizar, despues si se deben de considerar si llevan 
# una direccion o no, si no es dirigida se debe de ignorar; unconnected si es falso va a 
# regresar el mayor diametro posible, si es verdadero va a arrojar solamente el diametro 
# mas largo de los que estan conectados. Weights es opcional, pero se puede poner si el 
# grafo es pesada.

e6 <- mean_distance(e3)
e6

# Weighted Networks #
e7 <- matrix(nc = 3, byrow = T, c(1,2,0, 1,3,2, 1,4,1, 2,3,0, 2,5,5, 2,6,2, 3,2,1, 3,4,1,
                                  3,7,1, 4,3,0, 4,7,2, 5,6,2, 5,8,8, 6,3,2, 6,7,1, 6,9,1,
                                  6,10,3, 8,6,1, 8,9,1, 9,10,4))
e8 <- add_edges(make_empty_graph(10), t(el[,1:2]), weight=el[,3])
plot(e8) # No salio :c

# Coeficiente de clusterizacion
# Para caloular esto el comando a usar es "transitivity" 
e9 <- make_ring(10)
plot(e9)
transitivity(e9)

f <- make_full_graph(5)
plot(f)
transitivity(f)

# # # # 22 / F E B R E R O / 2023 # # # #
raw_friends <- read.csv("C:/Users/HP240/Documents/Genomica_F/Genomica/02_Raw_Data/aMIGOs_rEd.csv")
raw_friends
nvo_fn <- raw_friends[c(-15,-3),c(-1,-4,-16)] #Hay que quitar el 3,15,32 de ambos
nvo_fn
row.names(nvo_fn) <- colnames(nvo_fn)
nvo_fn
# Aqui ya se quito a Mayo y Yarissa de la red, ya que no tenian datos. Se quito la primer
# columna porque los nombres los contaba como otra variable y para mayor comodidad se hizo 
# que las columnas tuvieran el mismo nombre de las filas. 

raw_friends$YARISSA <- NULL
raw_friends
# Esta es otra forma de quitar columnas

raw_friends1 <- edit(data.frame())
# Esto es para crear una data frame donde se escriben los datos de forma directa.

mikasa <- graph.data.frame(nvo_fn, directed = T)
plot(mikasa)
# No sirve porque hace un grafico con los numeros de interacciones, no con los nombres. 

red_amigos <- graph_from_data_frame(nvo_fn, directed = F)
red_amigos
plot(red_amigos)
# No salio, es el mismo caso de arriba.

# # # # # AHORA SI LA RED BUENA # # # # #
raw_friends
ramigos <- raw_friends[c(-15,-3),c(-1,-4,-16)]
ramigos
# Primero hay que quitar la primer columna de los nombres, y las personas que no participaron
# con sus datos que son Mayo y Yarissa, así que hay que  quitarlos de las columans y de las 
# filas.

row.names(ramigos) <- colnames(ramigos)
ramigos
# Seguido de ello, se puso el mismo nombre de las columnas en los renglones.

ramigos <- as.matrix(ramigos)
diag(ramigos) <- rep(0,30)
ramigos
# Se convirtio el archivo "csv" en una matriz, y con el comando "diag" se convirtio en una
# matriz diagonal, este comando puede extraer o reemplazar la diagonal de una matriz, o como
# en este caso construir una matriz diagonal. 
# Por otro lado, el comando "rep" replica los valores (o algo asi, no entendi bien).

ami_net <- graph_from_adjacency_matrix(ramigos, mode = "directed", weighted = c("in", "out"))
ami_net
plot(ami_net)

# Para manipular los datos de la matriz de adyacencia se utiliza el comando:
# "graph_from_adjacency_matrix". Algo interesante a mencionar es que la grafica cambia por 
# completo según el argumento "weighted"; "NULL" separa la red en distintos componentes y 
# "TRUE" une la red; si en cambio se pone in y out, las flechas reflejan las interacciones de 
# entrada y de salida.

sort(degree(ami_net, mode = "in"), decreasing = T)
# Con este codigo se contabiliza cuantas flechas ingresan a cada nodo. Cuantas personas consideran
# a tal persona su amigo.
sort(degree(ami_net, mode = "out"), decreasing = T)
# Con este codigo se contabiliza cuantas flechas salen de cada nodo. A cuantas personas considera
# el sujeto en particular sus amigos.

class(ami_net)

# Redes personalizadas

# No me dejen hacer redes a mi, me quedan horribles. Mi red es una EVA Referencia.
plot(ami_net, vertex.size = 10, edge.arrow.mode = E(ami_net)$arrow, 
                edge.arrow.size = 0.4, edge.curved = 0.1, edge.color= "orchid", 
                vertex.color = "green", vertex.label.color = "black",
                vertex.label.family = "Helvetica", main = "Red De Amigos 2023", arrow.mode = 3)

# La verdad no se ve chidda.
edge.start <- ends(ami_net, es=E(ami_net), names=F)[,1]
edge.col <- V(ami_net)$color[edge.start]
plot(ami_net, edge.color=edge.col, edge.curved=.4)

#Asi se ve un poco mas profesional, pero no esta tan cool
plot(ami_net, edge.arrow.size=.2, edge.curved=0, vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(ami_net)$media, vertex.label.color="black", vertex.label.cex=.7) 

# Asi los nodos no son circulos
plot(ami_net, vertex.shape="none", vertex.label=V(ami_net)$media, vertex.label.font=2, 
     vertex.label.color="gray40", vertex.label.cex=.7, edge.color="gray85")

#Con clusters
reiner <- graph.adjacency(ramigos, mode = "directed", weighted = c("in", "out"))
reiner
class(reiner)
# May obtuvo distintas graficas, asi que use exactamente los mismos codigos. El primero es 
# con "ami_net" que fue hecha con "graph_from_adjacency_matrix", mientras que "reiner" fue 
# realizado con el comando "graph.adjacency". Lamentablemente todo se sigue viendo igual. 

# Cluster 1
cna <- cluster_optimal (ami_net)
plot (cna, ami_net, edge.arrow.size = 0.2,vertex.label=V(ami_net)$media,
               vertex.label= NA)

cna1 <- cluster_optimal (reiner)
plot (cna1, reiner, edge.arrow.size = 0.2,vertex.label=V(ami_net)$media,
      vertex.label= NA)

# Con este codigo es posible ver una sepracion entre quienes pertenecen al salon y quienes
# son externos, esto es gracias las interacciones mostradas. 

# Cluster 3
cna2 <- cluster_edge_betweenness(ami_net) 
plot (cna2, ami_net, edge.arrow.size = 0.2, edge.color = "gray40", vertex.label.color="black",
      vertex.label.cex=.6)

cna3 <- cluster_edge_betweenness(reiner) 
plot (cna3, reiner, edge.arrow.size = 0.2, edge.color = "gray40", vertex.label.color="black",
      vertex.label.cex=.6)
# A pesar de crear un cluster gigante, es posible observa que hay una mayor separacion 
# entre ciertas personas, siendo los que pertenecen al salon y los externos. 

# Mas intentos
plot(ami_net, vertex.color = colors(), vertex.frame.color = "black", vertex.shape = "circle", 
     vertex.size = 1, vertex.label.family = "Times", vertex.label.dist = T, edge.color = "black", 
     edge.width = 0.5, edge.arrow.size = 1)

# RED PERO HECHA CON CODIGO DE ROBERTO, ESTA SI ESTA BIEN BIEN BIEN HECHA.
mat <- read.csv("C:/Users/HP240/Documents/Genomica_F/Genomica/02_Raw_Data/aMIGOs_rEd.csv")
View(mat)

mat <- mat[ ,-1]
rownames(mat) <- colnames(mat)
mat <- mat[-c(3,15,32),-c(3,15,32)]
mat <- as.matrix(mat)
diag(mat) <- 0
any(diag(mat)!=0)

red <- graph_from_adjacency_matrix(mat, mode = "directed", weighted = T)
plot(red)

# Calcular la distribucion de conectividades (in & out)

# Hacer el degree de entrada
din <- degree(red, mode = "in")
hist(din)
# lo mismo pero graficando de forma directa
hist(degree(red,mode="in"),col = "tomato", main = "Distribución de conectividades. Entrada", 
     xlab = "Conexiones", ylab = "Frecuencia")

# Hacer el degree de salida
dout <- degree(red, mode = "out")
hist(dout)
# lo mismo pero graficando de forma directa
hist(degree(red,mode="out"),col = "deepskyblue", main = "Distribución de conectividades. Salida", 
     xlab = "Conexiones", ylab = "Frecuencia")

degree.distribution(red, v = V(red), mode = "in")
degree.distribution(red, v = V(red), mode = "out")

# Calcular el diametro 
diam <- diameter(red, directed= T)

diameter(red, directed = TRUE, unconnected = TRUE, weights = TRUE)
diameter(red)
#NOmas no sale el perro diametro 

# Calcular el average degree (in & out)

degree(red, mode = "in") ["PEDRO_JESUS"]  # Entran 18
degree(red, mode = "out") ["PEDRO_JESUS"] # Salen 16

(which(mat["PEDRO_JESUS",]==1)) # 4 amigos, Gaby, Sam, Jany y Ale

(which(mat["GABRIELA",]==1)) # Gaby sale con 7 amigos
(which(mat["SAMUEL",]==1)) # Sam sale con 1 amigo 
(which(mat["JANETZY",]==1)) # Jany con 4 amigos  
(which(mat["ALEJANDRA",]==1)) # Ale con 0

# Se procede a hacer un vector con esos datos para poder sacar el average degree 
ada <- c (7, 1, 4, 0)
adegree <- sum(ada)/length((which(mat["PEDRO_JESUS",]==1))) 
adegree

# RED NO PONDERADA #
mat1 <- ifelse(mat >= 0.5, 1, 0)
mat1
# Aquellos que poseen un valor mayor o igual a 0.5, se le pone un 1, si no, se le pone 0.

red2 <- graph_from_adjacency_matrix(mat1, mode = "directed")
red2
plot(red2)
red
# Calcular degree
degree(red2)
hist(degree(red2,mode="in"),col = "tomato", main = "Distribución de conectividades. Entrada, red no ponderada", 
     xlab = "Conexiones", ylab = "Frecuencia")
hist(degree(red2,mode="out"),col = "deepskyblue",  main = "Distribución de conectividades. Salida, red no ponderada", 
     xlab = "Conexiones", ylab = "Frecuencia")

# Calcular diametro

red2[,1]
rownames(red2)

# Calcular average degree con una funcion
(which(mat1["PEDRO_JESUS",]==1))
length(aa)

promedio_amixes <- function(){
  print(red2[,1])
  a <- readline(prompt = "Inserte su nombre igual que como aparece en la lista recién desplegada y presione enter: ")
  a <- as.character(a)
  b <- which(mat1[a,]==1)
  c <- c(0)
  for (i in 1:length(b)) {
  d <- which(mat1[i,]==1)
  e <- append(c,d)
  }
  f <- sum(e)/length((which(mat1[a,]==1))) 
  return(f)
}
promedio_amixes()
# Aun no queda, no sé cómo

# # # # 01 / 03 / 2023 # # # #
# Instalar "igraphdata"

g <- barabasi.game(100, directed = T)
# barabasi.game genera redes sin escala.
plot(g, vertex.label = NA)
g1 <- degree(g, mode = "in")
g1
hist(g1)

g2 <- mean(degree(g))
g3 <- degree(g)
# Se calcula el average degree para saber 
var(g3)

sd(g3)

g4 <- degree.distribution(g)

#################

fit1 <- fit_power_law(g1+1, 10)
# Fit se utiliza para ajustar algo a una ley de poder, en este caso se ajusta la colita
# de la grafica. 

fit2 <- fit_power_law(g1+1, 10, implementation = "R.mle")
fit1$alpha

stats4::coef(fit2)

fit1$logLik
stats4::logLik(fit2)

g5 <- barabasi.game(100, directed = F)
g5

library(igraphdata)
library(igraph)

ben <- random.graph.game(10, 0.5)
ben
plot(ben)

karate <- make_graph("zachary")
karate

plot(karate)


# # # # # 6 / F E B R E R O / 2023 # # # # # 
po <- make_ring(10) %>%
  set_vertex_attr("name", value = LETTERS[1:10])
po
plot(po)

po1 <- delete_vertices(po, c(1,5)) %>%
  delete_vertices("B")
po1
V(po1)

plot(po1)

# Ejercicios 
# Using the next networks, remove randomly 1/100 nodes. Calculate the mean of 
# the distance and the diameter. Plot the network. Repeat this process 10 times 
# for each network.
p100<-barabasi.game(100,directed = FALSE)
plot(p100)
dele <- sample(1:100, replace = F)
p100a <- delete_vertices(p100, dele)
p100a
plot(p100a)

deleicionador <- function(x){
  for (i in 1:10) {
    x <- delete.vertices(x,c(sample(1:length(V(x)),1)))
    print(paste("Promedio de las distancias", x, "es", mean_distance(x)))
  }
}
deleicionador(p100)

p1K<-barabasi.game(1000,directed = FALSE)

p2<-random.graph.game(1000,0.20)

p3<-sample_smallworld(1,1000,p=0.2,nei=3)



grafot <- read.table("C:/Users/HP240/Documents/Genomica_F/Genomica/02_Raw_Data/bio-celegans.mtx")
grafot <- graph_from_data_frame(grafot)
plot(grafot)

ografot <- read.table("C:/Users/HP240/Documents/Genomica_F/Genomica/02_Raw_Data/Physical_Interactions.IREF-matrixdb.txt")
ografot <- graph_from_data_frame(ografot)
plot(ografot)

#clusterizacion
eb <- edge.betweenness.community(p100, directed = F)
plot(eb, p100)

sg <- spinglass.community(p100)
plot(sg, p100)

g40 <- barabasi.game(40, directed = FALSE)
sg <- spinglass.community(g40)
plot(sg, g40)

# 09/03/2023 #
library(BoolNet)
library(Minet)
library(WGCNA)
library(STRINGdb)