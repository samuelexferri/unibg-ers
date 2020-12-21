#############################################################################################################################################
####### Progetto Network Analysis (aa 2020-2021) - GRUPPO 7 SLOT 8 - Air Algerie (AH)
#############################################################################################################################################

############################
####### DATASET 2020 #######
############################

# Importo file nodes2020
## Windows
nodes2020 <- read.csv(file="C:/GitHub/unibg-ers/code/NODES_2020.csv", sep= ";", fileEncoding="UTF-8-BOM")
nodes2020

# Importo file edges2020
## Windows
edges2020 <- read.csv(file="C:/GitHub/unibg-ers/code/EDGES_2020.csv", sep= ";", fileEncoding="UTF-8-BOM")
edges2020

# ZOOM
library(zoom)
#zm()# Chiamare solo dopo aver plottato, per uscire cliccare 'q' sulla figura

# Esamino i dati
head(nodes2020) 
head(edges2020) 
nrow(nodes2020) # 73
length(unique(nodes2020$Airport.Code)) # Tutti i nodi sono unici
nrow(edges2020) # 329
nrow(unique(edges2020[,c("Dep.Airport.Code","Arr.Airport.Code")])) # Tutte le tratte sono uniche

### Rappresenazione grafica
library(igraph)
net<-graph_from_data_frame(d=edges2020,vertices=nodes2020,directed=T)
class(net)
as_edgelist(net,names=T)
# Attenzione per network grandi può far saltare il programma as_adjacency_matrix(net,attr="weight")
as_data_frame(net,what="edges")
as_data_frame(net,what="vertices")

#### Plotto
plot(net,edge.arrow.size=.4,vertex.size=3,vertex.label.cex=.01)
plot(net,edge.arrow.size=.4,vertex.size=3,vertex.label.cex=.5)
plot(net,edge.arrow.size=.4,vertex.label=V(net)$name,vertex.size=3,vertex.label.cex=.5)
plot(net, edge.arrow.size=.4, vertex.shape="none", vertex.label=V(net)$name, vertex.size=3,vertex.label.font=2, vertex.label.color="gray40",vertex.label.cex=.5, edge.color="gray85")

#### Plotto con link curvi
plot(net,edge.arrow.size=.4,vertex.size=.9,edge.curved=.1,vertex.label.cex=.3)

#### Plotto con link non curvi e ridotta dimensione delle frecce
plot(net,edge.arrow.size=.2,edge.curved=0,vertex.color="orange",vertex.frame.color="#555555",vertex.label=V(net)$name,vertex.size=4.9,vertex.label.color="black",vertex.label.cex=.5)

#### Genero colore in base al Region.Code
unique(nodes2020[,c("Region.Code")]) # Region.Code unici: "AF1" "AF3" "ME1" "EU1" "EU2" "NA1" "AS4"
#V(net)$type <- 404
V(net)$type[V(net)$Region.Code=="AF1"] <- 1 # North Africa
V(net)$type[V(net)$Region.Code=="AF3"] <- 2 # Central/Western Africa
V(net)$type[V(net)$Region.Code=="ME1"] <- 3 # Middle East
V(net)$type[V(net)$Region.Code=="EU1"] <- 4 # Western Europe
V(net)$type[V(net)$Region.Code=="EU2"] <- 5 # Eastern/Central Europe
V(net)$type[V(net)$Region.Code=="NA1"] <- 6 # North America
V(net)$type[V(net)$Region.Code=="AS4"] <- 7 # North East Asia
V(net)$type
colrs<-c("orange","yellow","green","blue","sky blue","red","purple")
V(net)$color<-colrs[V(net)$type]
plot(net,edge.arrow.size=.2,edge.curved=0,vertex.label=V(net)$name,vertex.size=10,vertex.label.color="black",pt.bg=colrs,vertex.label.cex=.5,vertex.frame.color="white")
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#### Setto la taglia dei nodi in base a size (es. dimensione aeroporto per seats totali nel network)
#V(net)$size<-V(net)$Seats.Total*0.1
V(net)$size<-0.01*(log(V(net)$Seats.Total)^3+1)
#V(net)$size<-V(net)$Seats.Total*0.5

#### Tolgo i label
V(net)$label<-V(net)$name

#### Setto lo spessore dei link in base al weight
E(net)$width<-E(net)$Seats.Total/10000

#### Cambio la dimensione delle frecce e il colore degli edge/link
E(net)$arrow.size<-.5
E(net)$edge.color<-"gray80"

#### Attenzione! Se la linea è troppo spessa non si vede la freccia
#E(net)$width<-1+E(net)$weight/12
plot(net,vertex.label.cex=.45)
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#### Snellire il network (cut-off di voli con pochi passeggeri)
head(edges2020)
hist(edges2020$Seats.Total,30, col=c("lightskyblue1"), xlab="Passeggeri totali", ylab="Numero di tratte", main="Istrogramma dei passeggeri totali per tratta") # Seats Total
hist(edges2020$Frequency,30, col=c("lightskyblue1"), xlab="Numero di voli", ylab="Numero di tratte", main="Isogramma della frequenza dei voli") # Frequency Total
hist(edges2020$Seats.Total/edges2020$Frequency,30, col=c("lightskyblue1"), xlab="Passeggeri medi", ylab="Numero di tratte", main="Istogramma dei passeggeri medi")
mean(edges2020$Seats.Total) # Media 2511.198
sd(edges2020$Seats.Total) # Standard Deviation 3513.872
cut.off<-mean(edges2020$Seats.Total)
net.sp<-delete_edges(net,E(net)[Seats.Total<cut.off])
plot(net.sp,edge.color="lightskyblue1",vertex.color="gray50",vertex.label.cex=.45)

#### Snellire il network (cut-off di voli con pochi passeggeri)
#### Genero colore in base al Region.Code
unique(nodes2020[,c("Region.Code")]) # Region.Code unici: "AF1" "AF3" "ME1" "EU1" "EU2" "NA1" "AS4"
#V(net)$type <- 404
V(net)$type[V(net)$Region.Code=="AF1"] <- 1 # North Africa
V(net)$type[V(net)$Region.Code=="AF3"] <- 2 # Central/Western Africa
V(net)$type[V(net)$Region.Code=="ME1"] <- 3 # Middle East
V(net)$type[V(net)$Region.Code=="EU1"] <- 4 # Western Europe
V(net)$type[V(net)$Region.Code=="EU2"] <- 5 # Eastern/Central Europe
V(net)$type[V(net)$Region.Code=="NA1"] <- 6 # North America
V(net)$type[V(net)$Region.Code=="AS4"] <- 7 # North East Asia
V(net)$type
colrs<-c("orange","yellow","green","blue","sky blue","red","purple")
V(net)$color<-colrs[V(net)$type]
V(net)$size<-0.01*(log(V(net)$Seats.Total)^3+1)
cut.off<-mean(edges2020$Seats.Total)
net.sp<-delete_edges(net,E(net)[Seats.Total<cut.off])
plot(net.sp,edge.arrow.size=.2,edge.curved=0,vertex.label=V(net)$name,vertex.label.color="black",pt.bg=colrs,vertex.label.cex=.5,vertex.frame.color="white")
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#### Separare i due tipi di link in base alla regione dell'aeroporto di destinazione
net<-graph_from_data_frame(d=edges2020,vertices=nodes2020,directed=T)
E(net)$width<-2.0
#V(net)$label<-NA
net_africa <- net - E(net)[E(net)$Arr.Region.Code=="EU1" | E(net)$Arr.Region.Code=="EU2" | E(net)$Arr.Region.Code=="NA1" | E(net)$Arr.Region.Code=="AS4"]
net_extra_africa <- net - E(net)[E(net)$Arr.Region.Code=="AF1" | E(net)$Arr.Region.Code=="AF2" | E(net)$Arr.Region.Code=="ME1"]
par(mfrow=c(1,2))
plot(net_africa,vertex.color="orange",vertex.label.cex=.5,edge.arrow.size=.2,main="Tie: Africa")
plot(net_extra_africa,vertex.color="lightskyblue1",vertex.label.cex=.5,edge.arrow.size=.2,main="Tie: Extra Africa")

#### Mini (Algeris (ALG) to AF3, EU2, ME1, NA1, AS4)
#### Voli verso regioni diverse da AF1 e EU1 gestiti da più aeroporti africani
#### Notare un solo aeroporto in EU2 (Siviglia (SVQ))
net<-graph_from_data_frame(d=edges2020,vertices=nodes2020,directed=T)
E(net)$width<-2.0
#V(net)$label<-NA
V(net)$type[V(net)$Region.Code=="AF1"] <- 1 # North Africa
V(net)$type[V(net)$Region.Code=="AF3"] <- 2 # Central/Western Africa
V(net)$type[V(net)$Region.Code=="ME1"] <- 3 # Middle East
V(net)$type[V(net)$Region.Code=="EU1"] <- 4 # Western Europe
V(net)$type[V(net)$Region.Code=="EU2"] <- 5 # Eastern/Central Europe
V(net)$type[V(net)$Region.Code=="NA1"] <- 6 # North America
V(net)$type[V(net)$Region.Code=="AS4"] <- 7 # North East Asia
V(net)$type
colrs<-c("orange","yellow","green","blue","sky blue","red","purple")
V(net)$color<-colrs[V(net)$type]
V(net)$size<-0.01*(log(V(net)$Seats.Total)^3+1)
net_mini <- net - E(net)[E(net)$Arr.Region.Code=="AF1" | E(net)$Arr.Region.Code=="EU1"]
plot(net_mini,edge.arrow.size=.2,edge.curved=0,vertex.label=V(net)$name,vertex.label.color="black",pt.bg=colrs,vertex.label.cex=.5,vertex.frame.color="white", main="Voli verso regioni diverse da AF1 e EU1 gestiti da più aeroporti africani")
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#### Mini (notAF1)
#### Soltanto un volo la cui origine o destinazione non sia in AF1 (BUD-VIE)
net<-graph_from_data_frame(d=edges2020,vertices=nodes2020,directed=T)
E(net)$width<-2.0
#V(net)$label<-NA
V(net)$type[V(net)$Region.Code=="AF1"] <- 1 # North Africa
V(net)$type[V(net)$Region.Code=="AF3"] <- 2 # Central/Western Africa
V(net)$type[V(net)$Region.Code=="ME1"] <- 3 # Middle East
V(net)$type[V(net)$Region.Code=="EU1"] <- 4 # Western Europe
V(net)$type[V(net)$Region.Code=="EU2"] <- 5 # Eastern/Central Europe
V(net)$type[V(net)$Region.Code=="NA1"] <- 6 # North America
V(net)$type[V(net)$Region.Code=="AS4"] <- 7 # North East Asia
V(net)$type
colrs<-c("orange","yellow","green","blue","sky blue","red","purple")
V(net)$color<-colrs[V(net)$type]
V(net)$size<-0.01*(log(V(net)$Seats.Total)^3+1)
net_mini <- net - E(net)[E(net)$Dep.Region.Code=="AF1" | E(net)$Arr.Region.Code=="AF1"]
plot(net_mini,edge.arrow.size=.2,edge.curved=0,vertex.label=V(net)$name,vertex.label.color="black",pt.bg=colrs,vertex.label.cex=.5,vertex.frame.color="white", main="Soltanto un volo la cui origine o destinazione non sia in AF1 (BUD-VIE)")
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#### Mini (onlyAF1)
#### Voli la cui origine e destinazione è AF1
net<-graph_from_data_frame(d=edges2020,vertices=nodes2020,directed=T)
E(net)$width<-2.0
#V(net)$label<-NA
V(net)$type[V(net)$Region.Code=="AF1"] <- 1 # North Africa
V(net)$type[V(net)$Region.Code=="AF3"] <- 2 # Central/Western Africa
V(net)$type[V(net)$Region.Code=="ME1"] <- 3 # Middle East
V(net)$type[V(net)$Region.Code=="EU1"] <- 4 # Western Europe
V(net)$type[V(net)$Region.Code=="EU2"] <- 5 # Eastern/Central Europe
V(net)$type[V(net)$Region.Code=="NA1"] <- 6 # North America
V(net)$type[V(net)$Region.Code=="AS4"] <- 7 # North East Asia
V(net)$type
colrs<-c("orange","yellow","green","blue","sky blue","red","purple")
V(net)$color<-colrs[V(net)$type]
V(net)$size<-0.01*(log(V(net)$Seats.Total)^3+1)
net_mini <- net - E(net)[E(net)$Dep.Region.Code!="AF1" | E(net)$Arr.Region.Code!="AF1"]
plot(net_mini,edge.arrow.size=.2,edge.curved=0,vertex.label=V(net)$name,vertex.label.color="black",pt.bg=colrs,vertex.label.cex=.5,vertex.frame.color="white", main="Voli la cui origine e destinazione è AF1")
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#### Mini (notAlgeris)
#### Voli la cui origine o destinazione non sia Algeris (ALG)
net<-graph_from_data_frame(d=edges2020,vertices=nodes2020,directed=T)
E(net)$width<-2.0
#V(net)$label<-NA
V(net)$type[V(net)$Region.Code=="AF1"] <- 1 # North Africa
V(net)$type[V(net)$Region.Code=="AF3"] <- 2 # Central/Western Africa
V(net)$type[V(net)$Region.Code=="ME1"] <- 3 # Middle East
V(net)$type[V(net)$Region.Code=="EU1"] <- 4 # Western Europe
V(net)$type[V(net)$Region.Code=="EU2"] <- 5 # Eastern/Central Europe
V(net)$type[V(net)$Region.Code=="NA1"] <- 6 # North America
V(net)$type[V(net)$Region.Code=="AS4"] <- 7 # North East Asia
V(net)$type
colrs<-c("orange","yellow","green","blue","sky blue","red","purple")
V(net)$color<-colrs[V(net)$type]
V(net)$size<-0.01*(log(V(net)$Seats.Total)^3+1)
net_mini <- net - E(net)[E(net)$Dep.Airport.Name=="Algiers" | E(net)$Arr.Airport.Name=="Algiers"]
plot(net_mini,edge.arrow.size=.2,edge.curved=0,vertex.label=V(net)$name,vertex.label.color="black",pt.bg=colrs,vertex.label.cex=.5,vertex.frame.color="white", main="Voli la cui origine o destinazione non sia Algeris (ALG)")
legend(x=-1.5, y=-1.1, c("North Africa","Central/Western Africa","Middle East","Western Europe","Eastern/Central Europe","North America","North East Asia"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#############################################################################################################################################

#######################
####### PARTE 2 #######
#######################

## Densità - Numero dei link esistenti rispetto a tutti i possibili link; un grafico in cui ogni nodo è collegato con tutti gli altri ha denistà=1; in generale, più il valore è alto più sono le città che possono essere raggiunte con voli diretti (point-to-point)
edge_density(net,loops=F)
ecount(net)/(vcount(net)*(vcount(net)-1))

## Reciprocità - Frazione dei link bidirezionali
reciprocity(net)
dyad_census(net) # $mut = Simmetrici, $asym = Asimmetrici
2*dyad_census(net)$mut/ecount(net)

## Transitività - Triplette transitive (rapporto tra le triplette transitive e le triple potenzialmente transitive)(3*n_tringoli/n_triple)
## Network trattati come undirected! (Verifichiamo che reciprocità prossima a 100% e trattiamo il network come undirected)
transitivity(net,type="global")
transitivity(as.undirected(net, mode="collapse"))
triad_census(net) # Possibili triadi

## Diametro - Rappresenta il minimo numero di voli diretti necessari per connettere le due città/aeroporti più remoti nel network
diameter(net, directed=F,weights=NA)
diameter(net, directed=T,weights=NA)
diameter(net, directed=T)
diam<-get_diameter(net,directed=T)
diam
### Coloro i nodi lungo il diametro
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange"
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, vertex.size=10, vertex.label.cex=.5, edge.curved=.1, edge.color=ecol, edge.arrow.mode=0)

#############################################################################################################################################

#######################
####### PARTE 3 #######
#######################

## Gradi dei nodi
# Degree centrality è il numero di connessioni dirette rispetto al potenziale numero di connessioni dirette, misura l'importanza di un vertice in base al numero di connessioni del vertice
deg<-degree(net,mode="all")
plot(net, vertex.size=deg/7.5, vertex.label=V(net)$name, vertex.label.cex=.6, edge.arrow.size=.2, vertex.color="lightskyblue1")
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot(x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="lightskyblue1", xlab="Degree", ylab="Cumulative Frequency", type="s", lwd=5)
# Centralità e centralizzazione
degree(net, mode="in") # Si considerano solo i link che arrivano a uno scalo
centr_degree(net, mode="in", normalized=T) # Centalizzazione considerando la "in degree"

# Closeness (centrality based on distance to others in the graph) - Inverse of the node’s average geodesic distance to others in the network
closeness(net, mode="all", weights=NA)
centr_clo(net, mode="all", normalized=T)

# Betweenness (centrality based on a broker position connecting others) - Number of geodesics that pass through the node or the edge
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)

## Distanza e paths
# Average path length - The mean of the shortest distance between each pair of nodes in the network (#in both directions for directed graphs)
mean_distance(net, directed=F) # Undirect
mean_distance(net, directed=T) # Direct

## Matrice di adiacenza
as_adjacency_matrix(net)

#############################################################################################################################################

#########################
####### World Map #######
#########################

library(maps)
library(geosphere)

xlim <- c(-18.938281, +41.601563)
ylim <- c(15.039321, 60.856229)

map("world", col="grey90", fill=TRUE, bg="white", lwd=0.25, xlim=xlim, ylim=ylim)
title("Diametro della rete (2020)")
mtext(text = "", side = 4, line = -1, adj = 0.01, cex = 0.8)

# Coordinate aeroporti del diametro
lat_BMW <- 21.371585
lon_BMW <- 0.923955
lat_AZR <- 27.841194
lon_AZR <- -0.188132
lat_ALG <- 36.697961
lon_ALG <- 3.206887
lat_CDG <- 49.013853
lon_CDG <- 2.542437
lat_CFK <- 36.217643
lon_CFK <- 1.324707

inter2 <- gcIntermediate(c(lon_BMW, lat_BMW), c(lon_AZR, lat_AZR), n=50, addStartEnd=TRUE)
lines(inter2, col="red", lwd=5)
inter3 <- gcIntermediate(c(lon_AZR, lat_AZR),c(lon_ALG, lat_ALG), n=50, addStartEnd=TRUE)
lines(inter3, col="red", lwd=5)
inter4 <- gcIntermediate(c(lon_ALG, lat_ALG),c(lon_CDG, lat_CDG), n=50, addStartEnd=TRUE)
lines(inter4, col="red", lwd=5)
inter5 <- gcIntermediate(c(lon_CDG, lat_CDG),c(lon_CFK, lat_CFK), n=50, addStartEnd=TRUE)
lines(inter5, col="red", lwd=5)

text((lon_BMW+0.6), (lat_BMW+0.6),"BMW", col="black",cex = .8,font=2)
text((lon_AZR+0.6), (lat_AZR+0.6),"AZR", col="black",cex = .8,font=2)
text((lon_ALG+0.8), (lat_ALG+0.6),"ALG", col="black",cex = .8,font=2)
text((lon_CDG+0.6), (lat_CDG+0.6),"CDG", col="black",cex = .8,font=2)
text((lon_CFK-0.6), (lat_CFK-0.6),"CFK", col="black",cex = .8,font=2)