library(igraph)
library(ngram)
library(readr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(wordcloud)
library(textdata)
library(RColorBrewer)
library(reshape2)
library(topicmodels)

#library(remotes)
data("AssociatedPress")



nodos <- c("Ana", "Luis", "Carlos", "Marta", "Juan", 
           "Sofia", "Pedro", "Elena", "Raul", "Carmen")




aristas_no_dirigidas <- c(

  "Ana","Luis", "Ana","Carlos", "Ana","Marta",
  "Luis","Carlos", "Luis","Marta", "Carlos","Marta",

  "Ana","Juan", "Juan","Sofia", "Juan","Pedro", "Sofia","Pedro",

  "Elena","Raul", "Raul","Carmen", "Carmen","Elena"
)


g_no_dirigido <- graph(edges = aristas_no_dirigidas, directed = FALSE)

set.seed(123)
plot(g_no_dirigido, 
     vertex.color = "lightblue", 
     vertex.size = 35, 
     vertex.label.cex = 0.9,
     vertex.label.color = "black")





aristas_dirigidas <- c(
 
  "Ana","Luis", "Luis","Ana", "Ana","Carlos", "Carlos","Ana",
  "Ana","Marta", "Marta","Ana", "Luis","Carlos", "Carlos","Luis",
  "Luis","Marta", "Marta","Luis", "Carlos","Marta", "Marta","Carlos",

  "Juan","Ana", 

  "Sofia","Pedro", "Pedro","Sofia", "Pedro","Juan", "Sofia","Juan",
  
  "Elena","Raul", "Raul","Carmen", "Carmen","Elena"
)


g_dirigido <- graph(edges = aristas_dirigidas, directed = TRUE)

set.seed(123)
plot(g_dirigido, 
     vertex.color = "lightgreen", 
     vertex.size = 35, 
     vertex.label.cex = 0.9,
     vertex.label.color = "black",
     edge.arrow.size = 0.5) # Tamaño de la flecha



#### 

vcount(g_no_dirigido)
ecount(g_no_dirigido)
as_adjacency_matrix(g_no_dirigido, sparse = FALSE)






rutas_vuelos <- data.frame(
  origen = c("Bogota", "Bogota", "Bogota", "Bogota", "Miami", "Miami", "Nueva York", "Nueva York", "CDMX"),
  destino = c("Miami", "Nueva York", "CDMX", "Madrid", "Nueva York", "CDMX", "CDMX", "Madrid", "Madrid"),
  peso = c(250, 400, 300, 800, 150, 200, 350, 600, 850)
)


g_vuelos <- graph_from_data_frame(rutas_vuelos, directed = FALSE)


set.seed(50) 
plot(g_vuelos, 
     layout = layout_with_kk(g_vuelos),            
     
 
     vertex.color = "lightblue",  
     vertex.frame.color = "darkblue", 
     vertex.size = 50,              
     vertex.label.cex = 0.85,        
     vertex.label.color = "black",
     vertex.label.font = 2,         
     
   
     edge.color = "gray60",
     edge.width = 2,
     edge.label = E(g_vuelos)$peso, 
     edge.label.color = "darkred", 
     edge.label.cex = 1.1,          
     edge.label.family = "sans",
     edge.label.font = 2            
)



metrics <- round(c(
  mean_distance(g_no_dirigido), 
  mean(degree(g_no_dirigido)), 
  sd(degree(g_no_dirigido)), 
  clique_num(g_no_dirigido), 
  edge_density(g_no_dirigido), 
  transitivity(g_no_dirigido), 
  assortativity_degree(g_no_dirigido )
), 4)

nombres_metricas <- c(
  "Dist. media", "Grado media", "Grado desviación", 
  "Número clan", "Densidad", "Transitividad", "Asortatividad"
)


tabla_red <- data.frame(
  Estadistica = nombres_metricas,
  Valor = round(metrics,2)
)


centralidad <- tibble(
  word = V(g_no_dirigido)$name,
  eigen = eigen_centrality(g_no_dirigido)$vector
)


centralidad %>%
  arrange(desc(eigen)) %>%
  slice_head(n = 10) %>%
  mutate(eigen = round(eigen, 2))



####### lda


ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda


ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics



ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%

  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +

  scale_x_reordered() + 
  labs(x = "Palabra", y = "Probabilidad")


beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))




beta_spread %>%

  group_by(direccion = log_ratio > 0) %>%
  

  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  

  mutate(term = reorder(term, log_ratio)) %>%
  

  ggplot(aes(x = term, y = log_ratio, fill = log_ratio > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  

  labs(y = "Log2(beta_2 / beta_1)", x = "Palabra") +
  scale_fill_manual(values = c("darkred", "steelblue"))



ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents


ap_documents_ordenado <- ap_documents %>%
  arrange(document)

ap_documents_ordenado