
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

pcx <- function( idx, raw_text, get_most_common_ = NULL) {
  
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
  
  get_most_common_function <- result$get_most_common
  if (!is.null( get_most_common_))  get_most_common_function <- get_most_common_
  
  # take only the most frequently occuring words
  result$my_word_counts <- result$my_word_counts[ 1:get_most_common_function(result$my_word_counts$s0),]
  
  
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
  
  get_how_many_columns <- function(n) {
    result <- n %>% sqrt()
    if (n < 100) {
      result <- ( (100-n)*n + n*result)/100
    }
    result  %>% floor() %>% min(n)
  }
  
  
  result$get_projection <- function(how_many_columns=get_how_many_columns(result$my_principal_component_matrix$rotation %>% ncol())) {   # (result$my_principal_component_matrix$rotation %>% ncol())/3) {
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


# once the above has been created, use if for creating models with the below.


source("some_table_information.R")
source("roc_info.R")

ppcx <- function(thing_produced_from_pcx, the_categories) {
  
  result <- new.env(emptyenv())
  
  result$my_incidence_matrix <- thing_produced_from_pcx$my_incidence_matrix %>% cbind(the_categories)
  result$my_projection_matrix <- thing_produced_from_pcx$my_projection_matrix %>% cbind(the_categories)
  
  result$get_incidence_vector <- function(n) result$my_incidence_matrix[,n+1 ]
  result$get_projection_vector <- function(n) result$my_projection_matrix[,n+1 ]
  
  result$plot_projection <- function(xaxis, yaxis) {
    plot(result$my_projection_matrix[,yaxis+1] ~ result$my_projection_matrix[,xaxis+1], col=result$my_projection_matrix$the_categories+1)
  }
  
  result$plot_incidence <- function(xaxis, yaxis) {
    plot(result$my_incidence_matrix[,yaxis+1] ~ result$my_incidence_matrix[,xaxis+1], col=result$my_incidence_matrix$the_categories+1)
  }
  
  result$incidence_table <- function(xaxis, yaxis) {
    table( result$get_incidence_vector (yaxis), result$get_incidence_vector (xaxis) )
  }  
  
  result$get_incidence_logistic_model <- function() {
    glm(the_categories~.,data=result$my_incidence_matrix[,-1], family=binomial)
  }
  
  result$get_projection_logistic_model_formula <- function(the_formula) {
    glm(the_formula,data=result$my_projection_matrix[,-1], family=binomial)
  }
  
  
  result$get_projection_logistic_model_formula_training_prediction_probabilities <- function(the_formula, the_cutoff) {
    hm0 <- result$get_projection_logistic_model_formula( the_formula )
    x0 <- predict(hm0, result$my_projection_matrix[,-1], type="response")
    data.frame(idx=result$my_projection_matrix[,1], 
               actual=result$my_projection_matrix$the_categories, 
               probs = x0,
               predicted= the_cutoff < x0)
  }
  
  # add probs to the original projection matrix;
  result$get_probs <- function( the_formula) {
    hm0 <- result$get_projection_logistic_model_formula( the_formula )
    x0 <- predict(hm0, result$my_projection_matrix[,-1], type="response")
    result$my_projection_matrix %>% cbind(probs=x0)    
  }  
  
  result$roc <- function(the_formula, the_cutoff) {
    x0 <- result$get_projection_logistic_model_formula_training_prediction_probabilities(the_formula, the_cutoff)
    (table(actual=x0$actual,predicted=x0$predicted) %>% some_table_information())$roc
    
    
  }
  
  result$roc_all <- function(the_formula, the_granularity=100) {
    
    x0 <- data.frame( t(c(cutoff=0,result$roc(the_formula, 0) )))
    for (k in 1:the_granularity) x0 <- x0 %>% rbind( t(c(cutoff=k/the_granularity,result$roc(the_formula, k/the_granularity))))
    x0
  }
  
  
  result$plot_roc_all <- function(the_formula, the_granularity=100) {
    
    x0 <- result$roc_all(the_formula, the_granularity)
    plot(x0$true_positive_rate~x0$false_positive_rate)
    abline(0,1)
  }
  
  
  
  

  result$get_projection_logistic_model <- function() {
    glm(the_categories~.,data=result$my_projection_matrix[,-1], family=binomial)
  }
  
  result$get_test_model_confusion_matrix <- function(the_proportion, the_cutoff)  {
    x0 <- test_model(the_categories~., result$my_projection_matrix[,-1],the_proportion)
    x1 <- x0$test$pred > the_cutoff
    table(actual=x0$test$the_categories, predicted=x1)
  }
  
  result$get_test_model_confusion_matrix_information <- function(the_proportion, the_cutoff) {
    (result$get_test_model_confusion_matrix(the_proportion, the_cutoff) %>% some_table_information() )$ summary()
  }
  
  
  result$get_incidence_test_model_confusion_matrix <- function(the_proportion, the_cutoff)  {
    x0 <- test_model(the_categories~., result$my_incidence_matrix[,-1],the_proportion)
    x1 <- x0$test$pred > the_cutoff
    table(actual=x0$test$the_categories, predicted=x1)
  }
  
  result$get_incidence_test_model_confusion_matrix_information <- function(the_proportion, the_cutoff) {
    (result$get_incidence_test_model_confusion_matrix(the_proportion, the_cutoff) %>% some_table_information() )$ summary()
  }
  
  result$get_roc_info <- function(the_proportion, the_cutoff) {
    ( result$get_test_model_confusion_matrix(the_proportion, the_cutoff) %>% some_table_information() )$ roc
    
  }
  
  
  result
  
  
}



