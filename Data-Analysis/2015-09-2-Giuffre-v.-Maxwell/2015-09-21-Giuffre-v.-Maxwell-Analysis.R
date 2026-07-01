#### 2015-09-21-Giuffre-v.-Maxwell-Analysis

library(textdata)
library(ggwordcloud)
library(scales)
library(ngram)
library(readr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(wordcloud)
library(textdata)
library(RColorBrewer)
library(reshape2)
library(igraph)
library(ggraph)
library(tidygraph)

data(stop_words)


##### Import text----------

raw_text <- read_file("2015-09-21-Complaint-Giuffre-v.-Maxwell-C.txt")

#### Transform to tidy format

text <- tibble(
  text = raw_text               
)

#### Tokenization

text %<>%
  unnest_tokens(input = text, output = word) %>%  
  filter(!is.na(word))  

#### Cleaning Tokens

#Delete tokens with numbers

text%<>%
  filter(!grepl(pattern = "[0-9]", x = word)) 

#Remove stop words
text %<>% 
anti_join(x = ., y = stop_words)


####### Word Frequency--------------

#Table
text %>%
  count(word, sort = TRUE) %>%     
  mutate(porcentaje = round((n / sum(n)) * 100, 2)) %>%  # 
  head(n = 10)

#Histogram

text %>%
  count(word, sort = TRUE) %>%
  mutate(
    porcentaje = (n / sum(n)) * 100,
    etiqueta = paste0(n, " (", round(porcentaje, 2), "%)"),
    word = reorder(word, n)
  ) %>%
  slice_max(n, n = 20, with_ties = FALSE) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_segment(aes(xend = word, yend = 0), color = "gray80", linewidth = 0.8) + 
  geom_point(color = "#C0392B", size = 3.5) + 
  geom_text(
    aes(label = etiqueta),
    hjust = -0.15,          
    size = 3.5,            
    color = "gray20"       
  ) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL,
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(color = "gray40"),
    plot.title = element_text(face = "bold", size = 13)
  )
#World Cloud


set.seed(123)
text %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(
    words = word,
    freq = n,
    max.words = 20,
    ordered.colors = FALSE,
    colors = brewer.pal(9, "Greys")[7:9],
    scale = c(3.5, 0.6)     
  ))


######## Sentiment Analysis----------

afinn <- get_sentiments("afinn")

#Histogram



text %>%
  inner_join(afinn, by = "word") %>%
  count(word, value, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(score = n * value) %>% 
  mutate(sentiment = ifelse(value > 0, "Positivo", "Negativo")) %>% 
  mutate(word = reorder(word, score)) %>% 
  ggplot(aes(word, score, fill = sentiment)) +
  geom_col(width = 0.7) + 
  coord_flip() +
  scale_fill_manual(
    name = "Sentimiento",
    values = c("Negativo" = "#d73027", "Positivo" = "#4575b4")
  ) +

  scale_y_continuous(
    breaks = pretty_breaks(n = 6) #
  ) +
  labs(
    y = "Puntaje",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "plain", color = "black"), 
    legend.position = "bottom" 
  )


### Word cloud

set.seed(123)

text %>%
  inner_join(afinn, by = "word") %>%
  count(word, value, sort = TRUE) %>%

  mutate(sentiment = ifelse(value > 0, "Positivo", "Negativo")) %>%

  mutate(impacto = n * abs(value)) %>% 

  acast(word ~ sentiment, value.var = "impacto", fill = 0) %>%

  comparison.cloud(
    colors  = c("firebrick", "steelblue"), 
    max.words = 50,
    title.size = 1.5
  )

###NRC sentiments

nrc <- get_sentiments("nrc")


emociones_clean <- text %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("negative", "positive")) %>%
  count(sentiment) %>%
  mutate(
    porcentaje = (n / sum(n)) * 100,
    sentiment = case_when(
      sentiment == "anger" ~ "Ira",
      sentiment == "anticipation" ~ "Expectativa",
      sentiment == "disgust" ~ "Asco",
      sentiment == "fear" ~ "Miedo",
      sentiment == "joy" ~ "Alegría",
      sentiment == "sadness" ~ "Tristeza",
      sentiment == "surprise" ~ "Sorpresa",
      sentiment == "trust" ~ "Confianza"
    ),
    etiqueta = ifelse(porcentaje > 2, paste0(round(porcentaje, 1), "%"), "")
  )


colores_pastel <- c(
  "Ira" = "#FF9999",        
  "Tristeza" = "#4A90E2",   
  "Alegría" = "#FDFD96",     
  "Miedo" = "#CDB4DB",       
  "Asco" = "#66CC66",        
  "Expectativa" = "#FFB347", 
  "Sorpresa" = "#FFD1DC",    
  "Confianza" = "#AEEEEE"    
)

ggplot(emociones_clean, aes(x = "", y = porcentaje, fill = sentiment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(

    aes(x = 1.25, label = etiqueta), 
    position = position_stack(vjust = 0.5), 
    size = 4.5, 
    fontface = "bold",
    color = "gray20" 
  ) +
  scale_fill_manual(
    values = colores_pastel,
    name = "Emociones"
  ) +
  theme_void() 


######### Bigram -----

#Load the raw text into tidy format

text <- tibble(
  text = raw_text               
)




#Tokenization by bigrams

text %>%
  unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) -> text_bi


# Clean Bigrams with numbers and stop words
text_bi %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  
  # Delete bigrams with numbers
  filter(!grepl(pattern = '[0-9]', x = word1)) %>%
  filter(!grepl(pattern = '[0-9]', x = word2)) %>%
  
  # Delete stop words 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  
  
  # Delete NAs
  filter(!is.na(word1)) %>%
  filter(!is.na(word2)) %>%
  
  # Count most popular bigrams
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) -> text_bi_counts  


### Create a Network with brigams with 3 or more appearances


g <- text_bi_counts %>%
  filter(weight > 2) %>%
  graph_from_data_frame(directed = FALSE)

summary(g)
#Extract components
V(g)$cluster <- components(graph = g)$membership

#Extract the biggest connected component

gcc <- induced_subgraph(
  graph = g, 
  vids = which(V(g)$cluster == which.max(components(graph = g)$csize))
)

## Setting the plots
par(mfrow = c(1, 2), mar = c(1, 1, 2, 1), mgp = c(1, 1, 1))

# Plot 1: connected component without Weigth
set.seed(12)
plot(
  gcc, 
  layout = layout_with_lgl, 
  vertex.color = 1, 
  vertex.frame.color = 1, 
  vertex.size = 3, 
  vertex.label.color = 'black', 
  vertex.label.cex = 1.1,  
  vertex.label.dist = 1
)

# Plot 2: connected component with Weigth
set.seed(12)
plot(
  gcc, 
  layout = layout_with_lgl, 
  vertex.color = adjustcolor('darkolivegreen4', 0.1), 
  vertex.frame.color = 'darkolivegreen4', 
  vertex.size = 2 * strength(gcc), 
  vertex.label.color = 'black', 
  vertex.label.cex = 1.1, 
  vertex.label.dist = 1, 
  edge.width = 3 * E(gcc)$weight / max(E(gcc)$weight)
)













####### Skipgram ---------------



text <- tibble(
  text = raw_text               
)

#Tokenization by skipgrams

text %>%
  unnest_tokens(input = text, output = skipgram, token = "skip_ngrams", n = 2) %>%
  filter(!is.na(skipgram)) -> text_skip

#Count words

text_skip$num_words <- text_skip$skipgram %>% 
  map_int(.f = ~ wordcount(.x))


#Delte unigrams

text_skip %<>% 
  filter(num_words == 2) %>%  
  select(-num_words)         



# Clean Skipgrams with numbers and stop words
text_skip %>%
  
  separate(skipgram, c("word1", "word2"), sep = " ") %>%
  
  # Delete words with numbers
  filter(!grepl(pattern = '[0-9]', x = word1)) %>%
  filter(!grepl(pattern = '[0-9]', x = word2)) %>%
  
  # Delete stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  
  # Remove NA
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  
  # Frecuency
  count(word1, word2, sort = TRUE) %>%
  
  # Añadir la frecuencia relativa (en porcentaje)
  mutate(frec_relativa = round((n / sum(n)) * 100,2)) %>%
  
  # Rename count column to "weight"
  rename(weight = n) -> text_skip_counts

# Table top 10 skipgrams
head(text_skip_counts, n = 10)



#Create a network and extract the connected component


g <- text_skip_counts %>%
  filter(weight > 0) %>%
  graph_from_data_frame(directed = FALSE)


g <- igraph::simplify(g)  


V(g)$cluster <- components(graph = g)$membership



gcc <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(components(graph = g)$csize)))


#Circular layout plot

set.seed(123)
ggraph(gcc, layout = 'linear', circular = TRUE) + 

  geom_edge_arc(aes(edge_width = weight, edge_alpha = weight), 
                color = "darkolivegreen4", show.legend = FALSE) +

  geom_node_point(aes(size = strength(gcc)), color = "gray20", alpha = 0.8, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.2, 1.5)) +
  scale_edge_alpha_continuous(range = c(0.1, 0.5)) +
  scale_size_continuous(range = c(1, 4)) +
  theme_void() +
  coord_fixed() 















#Network stats

metrics <- round(c(
  mean_distance(gcc), 
  mean(degree(gcc)), 
  sd(degree(gcc)), 
  clique_num(gcc), 
  edge_density(gcc), 
  transitivity(gcc), 
  assortativity_degree(gcc)
), 4)


nombres_metricas <- c(
  "Dist. media", "Grado media", "Grado desviación", 
  "Número clan", "Densidad", "Transitividad", "Asortatividad"
)


tabla_red <- data.frame(
   Estadistica = nombres_metricas,
  Valor = round(metrics,2)
)


tabla_red


#Top 10 important word with eigen centrality


centralidad <- tibble(
  word = V(gcc)$name,
  eigen = eigen_centrality(gcc)$vector
)


centralidad %>%
  arrange(desc(eigen)) %>%
  slice_head(n = 10) %>%
  mutate(eigen = round(eigen, 2))


# Clustering




set.seed(123)
kc <- cluster_fast_greedy(gcc)



#Plot

cols <- c(
  brewer.pal(9, "Set1")[1:9],
  brewer.pal(8, "Set2")[1:7],
  brewer.pal(8, "Set2")[1:7],
  brewer.pal(12, "Set3")[1:3]
)

tidy_gcc_ordenado <- as_tbl_graph(gcc) %>%
  activate(nodes) %>%
  mutate(
    fuerza = strength(gcc),
    comunidad = as.factor(membership(kc))
  ) %>%
  arrange(comunidad)

set.seed(123)

ggraph(tidy_gcc_ordenado, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight), 
    color = "gray70", 
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(color = comunidad, size = fuerza), 
    show.legend = FALSE
  ) +
  scale_color_manual(values = cols) +
  scale_size_continuous(range = c(1, 5)) +
  scale_edge_width_continuous(range = c(0.5, 2)) +  
  scale_edge_alpha_continuous(range = c(0.4, 0.9)) + 
  theme_void() +
  coord_fixed() 







#Cluster stats
sizes(kc)


datos_clusters <- data.frame(
  Palabra = V(gcc)$name, 
  Cluster = kc$membership,
  Fuerza = strength(gcc)
)


top_clusters_palabras <- datos_clusters %>%

  group_by(Cluster) %>%
  mutate(Tamano_Cluster = n()) %>%
  ungroup() %>%

  arrange(desc(Tamano_Cluster)) %>%

  filter(Cluster %in% unique(Cluster)[1:5]) %>%
  
  
  group_by(Cluster) %>%
  arrange(desc(Fuerza)) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  
  
  select(Cluster, Palabra, Fuerza) %>%
  mutate(Fuerza = round(Fuerza, 2))


print(top_clusters_palabras)
