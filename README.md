\# Epstein Files: Text Mining, Sentiment Analysis \& Network Analysis



This repository contains a comprehensive text mining, sentiment analysis, and network analysis workflow applied to the unsealed court documents related to the Jeffrey Epstein case (a corpus consisting of 10 dense legal documents). 



The pipeline explores the underlying narratives, linguistic structures, and hidden networks within the judicial text using statistical text processing, sentiment/emotion classification, and graph theory.



\---



\## Summary of Insights



\* \*\*Linguistic Prominence:\*\* Text frequency and skipgram analyses confirm that \*\*Epstein\*\* acts as the central cross-cutting node of all narratives. High-frequency legal and criminal terms such as ```víctimas''`, ```tráfico''`, ```abuso''`, and ```sexual''` form the core structural framework of the corpus.

\* \*\*Financial Connections:\*\* The text density and lexical volume are heavily concentrated in documents involving financial institutions like \*\*JP Morgan\*\* and \*\*Deutsche Bank\*\*, indicating a massive amount of textual data dedicated to investigating corporate and financial involvement.

\* \*\*Sentiment \& Emotions:\*\* Sentiment polarity is heavily skewed toward the negative spectrum due to deep emotional triggers (```daños''`, ```fraude''`, ```violación''`). However, the dominant emotion metric is \*\*Confidence/Anticipation\*\*, driven by the formal, institutional language of the lawsuits and the systemic search for judicial restitution and financial indemnification for the victims.

\* \*\*Network Topologies:\*\* Network analysis of bigrams and skipgram clusters isolates Epstein as the only explicit individual within the main connected component, while clustering reveals defined thematic subgraphs: legal/procedural actors (Maxwell/Defendants), institutional actors (the Banks), and victim restitution frameworks.



\---



\## Repository Structure \& Contents



The workflow is organized into the following stages and scripts:



\### 1. Data Preparation \& Exploration

\* Data processing files to clean, tokenize, and prepare the text corpus.

\* \*\*`Descriptive\_statistics\_maps.R`\*\* \*(or your specific exploratory script)\*: Generates the baseline vocabulary counts and text distributions.

\* \*\*Visualizations:\*\* Generates the Word Cloud (`fig:nube conjunto`) and Term Frequency Bar Charts (`fig: bar archivo 11`).



\### 2. Sentiment and Emotion Analytics

\* Scripts dedicated to lexically scoring the corpus using sentiment and emotion dictionaries (such as NRC or Bing).

\* Plots general polarity tendencies (`fig:Sentimiento conjunto`) and captures the emotional spectrum (`fig:Emociones conjunto`) across categories like trust, anger, fear, sadness, and disgust.



\### 3. Network \& Co-occurrence Analysis (Skipgram / Bigram)

\* \*\*`text\_skip\_counts` Evaluation:\*\* Code to extract skipgram frequencies and relative weights to map connections between words that skip immediate adjacencies.

\* \*\*`tabla\_red` \& `centralidad` Calculation:\*\* Computes network statistics including average distance, average degree, degree standard deviation, density, transitivity, and assortativity.

\* \*\*Eigenvector Centrality:\*\* Ranks words based on their structural network influence (`tab:eigen\_red\_skipgramas`), highlighting the dominance of the ```sex''` and ```trafficking''` nodes.



\### 4. Hierarchical Clustering

\* Implementation of community detection and hierarchical clustering on text graphs.

\* Groups the network vertices into semantic clusters (`tab:clusters\_generales`), tracking cluster sizes and internal word strengths to isolate thematic sub-narratives (e.g., corporate liability vs. victim testimonies).



\---



\## Workflow Guide



To replicate the text mining and network metrics presented in the analysis, execute the scripts in the following chronological order:



1\. \*\*Preprocessing:\*\* Run the data cleaning scripts to structure the raw legal PDFs/TXTs into an R-ready tidy text format.

2\. \*\*Exploratory Plots:\*\* Execute the frequency scripts to output the bar charts and word clouds.

3\. \*\*Sentiment Analysis:\*\* Run the sentiment scoring scripts to evaluate polarity and emotion dimensions.

4\. \*\*Graph Generation:\*\* Compute the bigram/skipgram matrices to output network topologies, eigenvector centralities, and graph statistics.

5\. \*\*Clustering:\*\* Execute the hierarchical community grouping script to partition the text network into distinct legal and factual clusters.



\---



\## Requirements



This project was developed and tested using:

\* \*\*R / C++\*\* (Version `4.5.1` / or your current system version)

\* \*\*Key Packages:\*\* `tidytext`, `tidyverse`, `igraph`, `ggraph`, `tm`, `quanteda` \*(adjust based on your exact library calls)\*

