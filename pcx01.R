
library(tidyverse)
library(odbc)

library(e1071)
library(MASS)

library(tidytext)
library(tm)
library(NLP)
# library(openNLP)



# create a principal component calculator.
# given  an index and a list of text :
# calculate the most common words (kill stopwords)
# determine which entries have which of those -- this gives the raw matrix
# determine the principal components of that
# find the projection of the original text onto each of the (top) princ. comp.

pcx <- function( idx, raw_text) {
  
  result <- new.env(emptyenv())
  
  result$my_corpus <- VCorpus(VectorSource(raw_text))
  result$my_corpus <- tm_map(result$my_corpus, stripWhitespace)
  result$my_corpus <- tm_map(result$my_corpus, content_transformer(tolower))
  result$my_corpus <- tm_map(result$my_corpus, removeWords, stopwords("english"))
  result$my_document_term_matrix <- DocumentTermMatrix(result$my_corpus)
  result$my_tidy_dtm <- tidy(result$my_document_term_matrix ) 
  result$my_word_counts <- result$my_tidy_dtm %>% group_by(term) %>% mutate(s0=sum(count))
  result$my_word_counts <- result$my_word_counts[,c(2,4)] %>% unique()
  # we don't care about any word that appears only once.
  result$my_word_counts <- result$my_word_counts[result$my_word_counts$s0>1,]
  result$my_word_counts <- result$my_word_counts[order(result$my_word_counts$s0, decreasing=TRUE),]
  # messowordz <- 
  
  result$get_most_common <- function(a0) {
    a1 <- (a0%>%diff()) %>% accumulate(`+`)
    a2 <- 1:length(a1) %>% map( ~ a1[.]/. ) %>% unlist()
    (which(a2 > -1) %>% min())/2
  }
  
  # take only the most frequently occuring words
  result$my_word_counts <- result$my_word_counts[ 1:result$get_most_common(result$my_word_counts$s0),]
  
  
  # create the document term matrix for those words
  result$my_incidence_matrix <- data.frame(idx=idx)
  for (it in result$my_word_counts$term) {
    x0 <- result$my_tidy_dtm[  result$my_tidy_dtm$term==it, ]$document
    x0 <- 1:nrow(result$my_incidence_matrix) %in% x0
    result$my_incidence_matrix <- result$my_incidence_matrix  %>% cbind(x0) 
  }
  colnames(result$my_incidence_matrix) <- c("idx",  result$my_word_counts$term)
  
  
  
  # get the principal components
  result$my_principal_component_matrix <- result$my_incidence_matrix[,-1] %>% prcomp()
  
  # project the incidence matrix onto the principal components
  
  result$project_row_onto_principal_component <- function(the_row, the_pc) {
    irow <- result$my_incidence_matrix[the_row,-1] %>% as.numeric()
    ipc <- result$get_principal_component(the_pc) %>% as.numeric()
    irow %*% ipc
  }
  result$project_onto_principal_component <- function(the_pc) {
    1:nrow(result$my_incidence_matrix) %>% map( ~result$project_row_onto_principal_component(.,the_pc) ) %>% unlist()
  }
 
  
  
  result$plot_word_counts <- function() {
    plot(result$my_word_counts$s0, main="Word Count")
    legend("topright", legend=result$my_word_counts$term %>% head())
  }
  
  result$get_principal_component <- function(k) result$my_principal_component_matrix $ rotation [ ,k]
  
 
  result$ppc_cat <- function(the_categories) {
    
    plot(result$get_principal_component(2)~ result$get_principal_component(1), col=the_categories)
  }
  
  
  result$get_projection <- function(how_many_columns=(result$my_principal_component_matrix$rotation %>% ncol())/3) {
    df0 <- data.frame(idx=idx)
    for (k in 1:how_many_columns) {
      df0 <- df0 %>%  cbind(result$project_onto_principal_component(k))
    }
    colnames(df0) <- c("idx", 1:how_many_columns %>% map(~paste0("pcx_",.)) %>% unlist() )
    df0
    
  }
  result$my_projection_matrix <- result$get_projection()
  
  
  result
}


