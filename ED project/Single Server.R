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
genServeTime <- function(){
  rnorm(1, 0.3, 0.1)
}
#initial state
n <- 0; ta <- genPois(s=0); td <- Inf
now.time <- 0
end.time <- 168
arrive.list <- c(); depart.list <- c()

run <- TRUE
#arrival event
patientArrival <- function(){
  now.time <<- ta
  arrive.list <<- append(arrive.list, ta)
  n<<- n + 1
  ta <<- genPois(now.time)
  if(n ==1){
    td <<- now.time + genServeTime()
  }
}

#departure event
patientDeparture <- function (){
  # simulate customer departure
  now.time <<- td
  depart.list <<- append(depart.list, td)
  n <<- n - 1
  if(n==0){
    td <<- Inf
  } else {
      td <<- now.time + genServeTime()
  }
}

while (run){
  if(min (ta,td) <= end.time){
     # before T
     if( ta <= td ){
        # arrival
        patientArrival()
      } else {
        # departure
        patientDeparture()
        }
  } else if ( n > 0){
    # remaining customer , after T
    patientDeparture ()
  } else {
      # no customer , after T
      Tp <- max ( now.time - end.time, 0)
      run <- FALSE # end
  }
}
# removing first arrival time
arrive.list <- arrive.list[-1]

# removing last departure time
depart.new <- length(depart.list)
depart.list <- depart.list[-depart.new]

wait.list <- max(depart.list - arrive.list, 0)