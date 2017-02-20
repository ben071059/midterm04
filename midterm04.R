#盢璹逼ㄧ计decreasing = 把计,琵ㄏノ璹
exchange_sort_func <- function(input,decrease = FALSE){
  if(decrease == FALSE){
    return(sort(input,decreasing = FALSE))
  }else{
    return(sort(input,decreasing = TRUE))
  }
}

my_vec <- round(runif(10)*100)

#decreasing = FALSE
exchange_sort_func(my_vec, decrease = FALSE)

#decreasing = TRUE
exchange_sort_func(my_vec, decrease  = TRUE)


#妓セ夹非畉function

my_sd <- function(input_vec){
  x_means <- mean(input_vec)
  n_minusone <- length(input_vec) - 1
  sum_sampleX <- 0
  
  for(x in input_vec){
    sum_sampleX <- sum_sampleX + (x-x_means)**2 
  }
  return(sqrt(sum_sampleX / n_minusone))
}
