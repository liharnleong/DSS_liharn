arrival_lambda <- function(t) {
  if (t <= 24) {lambda = 12.09}
  else if (t > 24 & t <= 48) {14.68}
  else if (t > 48 & t <= 72) {12.89}
  else if (t > 72 & t <= 96) {12}
  else if (t > 96 & t <= 120) {11.78}
  else if (t >120 & t <= 144) {12.47}
  else if (t >144 & t <= 168) {11.06}
}

genArrival <- function(s , lambda =14.68){
  t0 <- s ; u <- runif(1)
  t0 <- t0 - (1/ lambda )*log(u)
  v <- runif(1)
  while (v <= get.lambda(t0)/lambda){
   u <- runif(1)
   t0 <- t0 + (-1/lambda)*log(u)
   v <- runif(1)
   }
   t0
}

genServeTime.1 <- rnorm(1, 0.3, 0.1)
genServeTime.2 <- rnorm(1, 0.4, 0.15)