arrival_lambda <- function(t) {
  if (t <= 24) {lambda = 12.09}
  else if (t > 24 & t <= 48) {14.68}
  else if (t > 48 & t <= 72) {12.89}
  else if (t > 72 & t <= 96) {12}
  else if (t > 96 & t <= 120) {11.78}
  else if (t >120 & t <= 144) {12.47}
  else if (t >144 & t <= 168) {11.06}
}

genArrival <- function(s, lambda =14.68){
  t0 <- s ; u <- runif(1)
  t0 <- t0 - (1/lambda )*log(u)
  v <- runif(1)
  while (v <= arrival_lambda(t)/lambda){
   u <- runif(1)
   t0 <- t0 + (-1/lambda)*log(u)
   v <- runif(1)
   }
   t0
}

genServeTime.1 <- rnorm(1, 0.3, 0.1)
genServeTime.2 <- rnorm(1, 0.4, 0.15)

genPois <- function(s , lambda =12.4){
  t0 <- 0; I <- 0
  u <- runif(1)
  t0 <- t0 - (1/lambda)*log(u)
  while (t0<168) {
    t0 <- FALSE
  }
  I <- I+1
  s <- t0
}

customer_arrival <- function() {
  now <<- ta
  n <<- n + 1
  num_a <<- num_a + 1
  arr_lst[num_a] <<- now
  ta <<- gen_arrival(now)
  if (n == 1) {
    i1 <<- num_a
    td <<- now + gen_departure()
    wait_lst[num_a] <<- 0
  }
  if (n==2)
}