
library(tidyverse)   
library(magrittr)     
library(tidytext)     
library(textdata)     
library(ngram)        
library(topicmodels)  
library(igraph)       
library(tidygraph)    
library(ggraph)       
library(ggwordcloud)  
library(wordcloud)    
library(RColorBrewer) 
library(scales)       
library(reshape2)     


####### LDA ---------------


#Extact file names

archivos <- list.files(pattern = "\\.txt$")


#Load the files

textos_completos <- tibble(
  documento = archivos,
  text = map_chr(archivos, read_file)
)




#### Tokenization 
text_tidy <- textos_completos %>%
  unnest_tokens(input = text, output = word) %>%  
  filter(!is.na(word))


#### Cleaning Tokens

text_tidy %<>%
  filter(!grepl(pattern = "[0-9]", x = word)) 


text_tidy %<>% 
  anti_join(x = ., y = stop_words, by = "word")

# Count words by file

conteo_palabras <- text_tidy %>%
  count(documento, word, sort = TRUE)

# Transform into a Document-Term matrix
dtm_argumento <- conteo_palabras %>%
  cast_dtm(document = documento, term = word, value = n)



#Tunning parameter k


valores_k <- 2:11
perplejidades <- numeric(length(valores_k))


for (i in seq_along(valores_k)) {
  k_actual <- valores_k[i]
  
  # Entrenamos un modelo rápido por cada K
  modelo_temp <- LDA(dtm_argumento, k = k_actual, method = "Gibbs",
                     control = list(seed = 1234, iter = 1000, burnin = 500))
  
  # Guardamos la perplejidad calculada por el paquete
  perplejidades[i] <- perplexity(modelo_temp, dtm_argumento)
}

# 1. Configurar los parámetros gráficos
par(mfrow = c(1,1), mar = c(3, 3, 2, 0.5), mgp = c(1.7, 0.7, 0))

# 2. Graficar con type = "b" y pch = 21
plot(valores_k, perplejidades, 
     type = "b",                     
     pch = 21,                        
     col = "black",                   
     bg = adjustcolor("darkred", 0.8),
     lwd = 2,                         
     cex = 1.4,                     
     xlab = "K", 
     ylab = "Perplejidad")


#Valor optimo 6



# Setting gibs parametrs
control_gibbs <- list(
  seed = 1234,
  burnin = 5000, 
  iter = 20000,   
  keep = 10      # Keep Log-Likelihood each 10 iteration
)

#Running model



modelo_lda <- LDA(dtm_argumento, k = 6, method = "Gibbs", control = control_gibbs)
modelo_lda


#Log-likelihood 

ll <- modelo_lda@logLiks


par(mfrow = c(1,1), mar = c(2.75, 3, 1.5, 0.5), mgp = c(1.7, 0.7, 0))

plot(ll[-c(1:10)], type = "p", pch = 16, col = adjustcolor(1, 0.8), cex = 1.2, 
     xlab = "Iteración", ylab = "Log-verosimilitud")





#Extract beta matix

temas <- tidy(modelo_lda, matrix = "beta")



# Top 10 word per topic
top_terminos <- temas %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot
ggplot(top_terminos, aes(x = beta, 
                         y = reorder_within(term, beta, topic), 
                         fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Tema", topic), scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_viridis_d(option = "turbo", alpha = 0.85) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(), # Quita las líneas horizontales para un look más limpio
    axis.text.y = element_text(color = "black")
  ) +
  labs(
    x = "Probabilidad (Beta)",
    y = NULL
  )


#Exlusive terms per topic using log2 ratio


terminos_exclusivos <- temas %>%
  group_by(term) %>%
  filter(sum(beta) > 1e-4) %>%
  mutate(
    beta_promedio_otros = (sum(beta) - beta) / (n() - 1),
    log_ratio = log2(beta / (beta_promedio_otros + 1e-10))
  ) %>%
  ungroup() %>%
  
  
  group_by(topic) %>%
  slice_max(log_ratio, n = 10) %>%
  ungroup() %>%
  arrange(topic, -log_ratio)


#Plot

ggplot(terminos_exclusivos, aes(x = log_ratio, 
                                y = reorder_within(term, log_ratio, topic), 
                                fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Tema", topic), scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_viridis_d(option = "plasma", alpha = 0.85) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "black")
  ) +
  labs(
    x = "Nivel de Exclusividad (Log-Ratio)",
    y = NULL
  )


#Theta distribution of the files per topics


documentos_theta <- tidy(modelo_lda, matrix = "gamma") %>%
  mutate(
    topic = factor(topic, levels = 1:6, labels = paste("Tema", 1:6)),
    document = document %>%
      str_remove("^\\d{4}-\\d{2}-\\d{2}-[A-Za-z]+-") %>% 
      str_remove("-C\\.txt$") %>% 
      str_replace_all("-", " ") 
  )

ggplot(documentos_theta, aes(x = gamma, y = document, fill = topic)) +
  geom_col(position = "fill", alpha = 0.9) +
  scale_fill_brewer(palette = "Set1", name = "Tópicos") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    axis.text.y = element_text(color = "black", face = "bold"),
    panel.grid.major.y = element_blank() 
  ) +
  labs(
    x = "Proporción del Documento (Theta)",
    y = "Documentos"
  )


######### lexical concord

text_tidy_limpio <- text_tidy %>%
  mutate(
    documento = str_remove(documento, "^\\d{4}-\\d{2}-\\d{2}-[A-Za-z]+-"),
    documento = str_remove(documento, "-C\\.txt$"),
    documento = str_replace_all(documento, "-", " ")
  )


frec <- text_tidy_limpio %>%
  count(documento, word) %>%
  group_by(documento) %>%
  mutate(proportion = n / sum(n)) %>%          
  select(-n) %>%
  spread(documento, proportion, fill = 0)      




nombres_docs <- setdiff(names(frec), "word")
n_docs <- length(nombres_docs)

# Crear la matriz vacía para guardar los resultados
matriz_coincidencia <- matrix(0, nrow = n_docs, ncol = n_docs, 
                              dimnames = list(nombres_docs, nombres_docs))

# Bucle que aplica tu código exacto a cada par de documentos
for(i in 1:n_docs) {
  for(j in 1:n_docs) {
    
    if(i == j) {
      matriz_coincidencia[i, j] <- NA # Ignorar si es el mismo documento
    } else {
      doc_A <- nombres_docs[i]
      doc_B <- nombres_docs[j]
      
      frec_par <- frec %>%
        select(word, all_of(doc_A), all_of(doc_B)) %>%
        filter(.data[[doc_A]] != 0 | .data[[doc_B]] != 0) 
      

      frec_comun <- frec_par %>%
        filter(.data[[doc_A]] != 0, .data[[doc_B]] != 0)
      

      prop_palabras_comunes <- dim(frec_comun)[1] / dim(frec_par)[1]
      

      matriz_coincidencia[i, j] <- prop_palabras_comunes
    }
    
  }
}



# Convertir a formato para graficar
df_matriz_final <- as.data.frame(matriz_coincidencia) %>%
  rownames_to_column(var = "Doc1") %>%
  pivot_longer(cols = -Doc1, names_to = "Doc2", values_to = "Proporcion") %>%
  filter(!is.na(Proporcion))

# Graficar
ggplot(df_matriz_final, aes(x = Doc1, y = Doc2, fill = Proporcion)) +
  geom_tile(color = "white", size = 0.4) +
  
  # Texto siempre negro y en negrita
  geom_text(aes(label = scales::percent(Proporcion, accuracy = 0.1)), 
            color = "black", 
            size = 3.2, fontface = "bold") +
  
  # Paleta morada ajustada: 
  # low = blanco con un ligero tinte lila
  # high = morado amatista medio (legible con texto negro)
  scale_fill_gradient(low = "#f4ebf8", high = "#c39bd3", 
                      labels = scales::percent_format(), name = "% Coincidencia") +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", face = "bold"),
    axis.text.y = element_text(color = "black", face = "bold"),
    panel.grid = element_blank(),
    aspect_ratio = 1
  ) +
  labs(
    x = NULL, y = NULL
  )


#Terms per pairs of document


obtener_top_comun <- function(doc_A, doc_B, datos = frec, n_palabras = 10) {
  datos %>%
    select(word, all_of(doc_A), all_of(doc_B)) %>%
    filter(.data[[doc_A]] != 0, .data[[doc_B]] != 0) %>%
    # Ordenar por la suma de las frecuencias en ambos documentos
    mutate(suma_importancia = .data[[doc_A]] + .data[[doc_B]]) %>%
    arrange(desc(suma_importancia)) %>%
    head(n = n_palabras)
}



# Par 1: Doe v. JP Morgan & Doe v. Deutsche Bank
top_par_1 <- obtener_top_comun("Doe v. JP Morgan", "Doe v. Deutsche Bank")

# Par 2: Giuffre v. Maxwell & Giuffre v. Dershowitz
top_par_2 <- obtener_top_comun("Giuffre v. Maxwell", "Giuffre v. Dershowitz")

# Par 3: Government v. JP Morgan & Doe v. JP Morgan
top_par_3 <- obtener_top_comun("Government v. JP Morgan", "Doe v. JP Morgan")

# Par 4: USA v. Maxwell & USA v. Epstein
top_par_4 <- obtener_top_comun("USA v. Maxwell", "USA v. Epstein")

# Inspección de los resultados
print(top_par_1)
print(top_par_2)
print(top_par_3)
print(top_par_4)



###### Correlation terms---------------



###### Correlation terms---------------

matriz_frecuencias <- t(as.matrix(dtm_argumento))

colnames(matriz_frecuencias) <- colnames(matriz_frecuencias) %>%
  stringr::str_remove("^\\d{4}-\\d{2}-\\d{2}-[A-Za-z]+-") %>% 
  stringr::str_remove("-C\\.txt$") %>% 
  stringr::str_replace_all("-", " ")

matriz_cor <- cor(matriz_frecuencias, method = "spearman")

df_correlacion <- as.data.frame(matriz_cor) %>%
  rownames_to_column(var = "Doc1") %>%
  pivot_longer(cols = -Doc1, names_to = "Doc2", values_to = "Correlacion")


df_correlacion_limpio <- df_correlacion %>%
  filter(as.character(Doc1) != as.character(Doc2))


ggplot(df_correlacion_limpio, aes(x = Doc1, y = Doc2, fill = Correlacion)) +
  geom_tile(color = "white", size = 0.3) +
  
  geom_text(aes(label = sprintf("%.2f", Correlacion), 
                color = Correlacion > 0.7), 
            size = 3.5, fontface = "bold") +
  

  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  
  scale_fill_gradient(low = "#f4f4f6", high = "#003f5c", 
                      limits = c(0, 1), name = "Spearman") +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", face = "bold"),
    axis.text.y = element_text(color = "black", face = "bold"),
    panel.grid = element_blank(),
    aspect_ratio = 1 
  ) +
  labs(
    x = NULL,
    y = NULL
  )

############## Joint Anlsys -------------------------------------------

text <- textos_completos %>%
  summarise(
    text = str_flatten(text, collapse = " ")
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
  head(n = 5)

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
  mutate(score = n * value) %>% 
  filter(abs(score) > 60) %>%
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
    breaks = pretty_breaks(n = 10)
  ) +
  labs(
    y = "Puntaje",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "plain", color = "black", size = 11), 
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

text <- textos_completos %>%
  summarise(
    text = str_flatten(text, collapse = " ")
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


### Create a Network with brigams with 31 or more appearances


g <- text_bi_counts %>%
  filter(weight > 30) %>%
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
  vertex.size = 0.04 * strength(gcc), 
  vertex.label.color = 'black', 
  vertex.label.cex = 1, 
  vertex.label.dist = 1, 
  edge.width = 3 * E(gcc)$weight / max(E(gcc)$weight)
)













####### Skipgram ---------------



text <- textos_completos %>%
  summarise(
    text = str_flatten(text, collapse = " ")
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















#Component stats

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
  brewer.pal(8, "Set2")[1:20],
  brewer.pal(12, "Set3")[1:20]
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





# --- Cluster stats ---

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
  
  filter(Cluster %in% head(unique(Cluster), 5)) %>% 
  
  
  group_by(Cluster) %>%
  arrange(desc(Fuerza), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  
  
  select(Cluster, Tamano_Cluster, Palabra, Fuerza) %>%
  mutate(Fuerza = round(Fuerza, 2))

print(top_clusters_palabras)
