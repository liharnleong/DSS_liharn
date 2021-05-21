#initial state
n<- 0; i1 <- 0; i2 <- 0
ta <- genArrival(s=0); t1 <- Inf; t2 <- Inf
now.time <- 0; N<- 0; C1 <- 0; C2 <-0
An <- NULL; Dn <- NULL

#working function

#entering system
updateArrival <- function(){
     now.time <<-ta; N<<- N + 1
     An[N] <<- now.time
     ta <<- genArrival(s=now.time)
     if(n==0){
       n<<- n + 1
       i1 <<- N
       i2 <<- 0
       t1 <<- now.time + genServTime.1()
     } else if (n==1){
       n <<- 2
       if(max(t1, t2)==t1){
         #server 1 is free
         i1 <<- N
         t1 <<- now.time + genServTime.1()
       } else {
               #server 2 is free
               i2 <<- N
               t2 <<- now.time + genServTime.2()
       }
     } else {
             n <<- n + 1
     }
}

#exiting system
updateDeparture.1 <- function(){
        now.time <<- t1; C1 <<- C1 + 1
        Dn[i1] <<- now.time
        if(n==1){
                n <<- 0
                i1 <<- 0; i2 <<- 0
                t1 <<- Inf
        } else if (n==2){
                n <<- 1
                i1 <<- 0
                t1 <<- Inf
        } else {
                m <- max(i1,i2)
                n <<- n - 1
                i1 <<- m + 1
                t1 <<- now.time + genArrival
        }
}
updateDeparture.2 <- function(){
        now.time <<- t2; C2 <<- C2 + 1
        Dn[i1] <<- now.time
        if(n==1){
                n <<- 0
                i1 <<- 0; i2 <<- 0
                t2 <<- Inf
        } else if (n==2){
                n <<- 1
                i2 <<- 0
                t2 <<- Inf
        } else {
                m <- max(i1,i2)
                n <<- n - 1
                i2 <<- m + 1
                t2 <<- now.time + genArrival
        }
}

#simulating system
run <- TRUE
while (run) {
        if(min(ta, t1, t2)<=end.time){
                if(min(ta, t1, t2) == ta){
                        updateArrival()
                } else if (t1 <= t2){
                        updateDeparture.1()
                }else {
                        updateDeparture.2()
                }
        } else if (n>0){
                if(t1 <= t2){
                        updateDeparture.1()
                } else {
                        updateDeparture.2()
                }
        } else {
                run <- FALSE
        }
        
}