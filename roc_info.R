

roc_info <- function(the_categories, the_probabilities, the_cutoff) {
  
  result <- new.env(emptyenv())
  
  result$df <- data.frame(actual=the_categories, the_probabilities) %>% cbind( predicted=the_cutoff<  the_probabilities)
  
  result$tbl <- table(actual=result$df$actual, predicted=result$df$predicted)
  cat(the_cutoff)
  result$roc <- c(cutoff=the_cutoff, some_table_information( result$tbl ))
  
  
  result
  
}





roc_info_all <- function(the_categories, the_probabilities) {
  
  result <- new.env(emptyenv())
  
  result$get <- function(the_cutoff) {
    temp_ <- roc_info(the_categories, the_probabilities, the_cutoff)
    temp_$roc$roc
  }
  
  result$df <- data.frame(t(result$get(0)))
  for (k in 1:100) {
    result$df <- result$df %>% rbind(t(result$get(k/100))  )
  }
  
  # put the resultant data frame in a nice order
  result$df <- result$df[order(result$df$false_positive_rate),]
  
  result$plot_me <- function() {
    plot(true_positive_rate~false_positive_rate, data=result$df)
    abline(0,1)
  }
  
  result$get_auc <- function() {
    
    auc <- 0
    for (k in 2:nrow(result$df)) {
      x_0 <- result$df[k-1, "false_positive_rate"] 
      x_1 <- result$df[k, "false_positive_rate"] 
      y_1 <- result$df[k, "true_positive_rate"] 
      auc <- auc + y_1*(x_1-x_0)
    }
    
    auc
  }
  
  
  result
}

