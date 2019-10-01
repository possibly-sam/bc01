

some_table_information <- function(the_table) {
  
  result <- new.env(emptyenv())
  
  the_table_ <- the_table %>% as.data.frame()
  the_table_$actual <- the_table_$actual %>% as.character() %>% as.numeric() 
  the_table_$predicted <- the_table_$predicted  %>% as.logical() %>% as.numeric()
  result$the_table_ <- the_table_
  
  result$true_positive <- the_table_[ the_table_$actual==1 & the_table_$predicted==1, "Freq"]
  result$true_negative <- the_table_[ the_table_$actual==0 & the_table_$predicted==0, "Freq"]
  result$false_positive <- the_table_[ the_table_$actual==0 & the_table_$predicted==1, "Freq"]
  result$false_negative <- the_table_[ the_table_$actual==1 & the_table_$predicted==0, "Freq"]
  
  if (0 == result$true_positive %>% length() ) result$true_positive <- 0
  if (0 == result$true_negative %>% length() ) result$true_negative <- 0
  if (0 == result$false_positive %>% length() ) result$false_positive <- 0
  if (0 == result$false_negative %>% length() ) result$false_negative <- 0
  
  result$positive <- result$true_positive + result$false_negative
  result$negative <- result$true_negative + result$false_positive
  
  result$true_positive_rate <- result$true_positive/result$positive 
  result$false_positive_rate <- result$false_positive/result$negative 

  result$true_negative_rate  <- result$true_negative/result$negative 
  result$false_negative_rate <- result$false_negative/result$ positive
  
  result$roc <- c(false_positive_rate=result$false_positive_rate, true_positive_rate=result$true_positive_rate )  
  
  result$summary <- function() {
    cat(paste("True Positive: ", result$true_positive_rate,  "\n"))
    cat(paste("False Positive:", result$false_positive_rate, "\n"))
    cat(paste("True Negative: ", result$true_negative_rate,  "\n"))
    cat(paste("False Negative:", result$false_negative_rate, "\n"))
    print(the_table )
    
  }
  
  result
  
}
