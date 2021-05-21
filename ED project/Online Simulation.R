t.end   <- 168  # duration of sim
t.clock <- 0    # simulation time
arrive <- rpois(100,0.2)
Ta <- mean(arrive)      # arrival period
Ts <- 0.2         # service period
t1 <- 0         # time for next arrival
t2 <- t.end     # time for next departure
tn <- t.clock   # tmp var for last event time
tb <- 0         # tmp var for last busy-time start
n <- 0          # number in system
s <- 0          # cumulative number-time product
b <- 0          # total busy time
c <- 0          # total completions
qc <- 0         # plot instantaneous queue size
tc <- 0         # plot time delta
plotSamples <- 100
set.seed(1)
while (t.clock < t.end) {
  if (t1 < t2) {      # arrival event
    t.clock <- t1
    s <- s + n * (t.clock - tn)  # time-weighted number in queue
    n <- n + 1
    if (t.clock < plotSamples) { 
      qc <- append(qc,n)
      tc <- append(tc,t.clock) 
    }
    tn <- t.clock
    t1 <- t.clock + rexp(1, 1/Ta)
    if(n == 1) { 
      tb <- t.clock
      t2 <- t.clock + rexp(1, 1/Ts)  # exponential arrival period
    }
  } else {            # departure event
    t.clock <- t2
    s <- s + n * (t.clock - tn)  # time-weighted number in queue
    n <- n - 1
    if (t.clock < plotSamples) { 
      qc <- append(qc,n)
      tc <- append(tc,t.clock)
    }
    tn <- t.clock
    c <- c + 1
    if (n > 0) { 
      t2 <- t.clock + rexp(1, 1/Ts)  # exponential  service period
    }
    else { 
      t2 <- t.end
      b <- b + t.clock - tb
    }
  }   
}
plot.default(tc,qc)