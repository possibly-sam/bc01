
test_model <- function(the_formula, the_data, the_proportion) {
  result <- new.env(emptyenv())
  
  n <- the_data %>% nrow()
  
  result$tr <- sample(1:n, floor(the_proportion*n) )
  result$train <- the_data[result$tr,]
  result$test <- the_data[-result$tr,]
  
  result$hm <- glm(the_formula, data=result$train, family=binomial)
  
  result$pred <- predict(result$hm, result$test, type="response")
  result$test <- result$test %>% cbind( pred=result$pred ) 
  
  # result$table <- table(pred=predict(result$hm, test)$class, true=the_data$the_categories[-tr])
  # rresult$roc <- some_table_information(rresult$table)
  result
}
