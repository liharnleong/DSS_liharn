genPois <- function(s){
  lambda = 3 #arrival rate is quarter of daily rate
  s + rexp(1, rate = lambda)
}
genServeTime <- function(){
  rnorm(1, 0.2, 0.1)
}
#initial state
n <- 0; ta <- genPois(s=0); td <- Inf; tc <- c()
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

wait.list <- depart.list - arrive.list
plot(arrive.list, wait.list)