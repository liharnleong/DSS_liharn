#############################
# Two parallel servers queue
#############################
# Interested in finding the waiting time of each customer
# before closing time T = 168 hours

################
# Problem setup
################
# Arrival time: homogenous Pois with lambda = 6.2/hour
# Serving time: Normal with mean=0.25hour, sd=0.1hour
# customers are labelled by arrival order
# e.g.: customer 1 arrive earlier than customer 2
# if both servers are occupied, customer join queue
# else customer join server 1 (first) if it's free
# else server 2

##########################
# DES variable definition
##########################
# System: two parallel servers

# Entity: server, customer

# Event: arrival, departure of customers

# SS: (n, i1, i2, num_a)
# n := num of customers in system, excluding those who have left,
#      including those at station and queue
# i1 := index for the ith customer is with server 1
# i2 := index for the ith customer is with server 2
# num_a := total num of customers who have arrived

# EL: (ta, t1, t2)
# ta := time of next arrival
# t1 := time of next departure for server 1
# t2 := time of next departure for server 2

# Param: (now, arr_lst, dep_lst, wait_lst)
# now := current time
# arr_lst := vector storing arrival time of customers
# dep_lst := vector storing departure time of customers
# wait_lst := vector storing waiting time of customers

replicate(
    100,
    {
#################
# Helper function
#################
gen_arrival <- function(now) {
    # generate next homogenous poisson arrival time
    lambda <- 6.2
    now + rexp(1, rate = lambda)
}

gen_departure <- function() {
    # generate service time
    rnorm(1, mean = 0.25, sd = 0.1)
}

###############
# Initialising
###############
n <- 0
i1 <- 0
i2 <- 0
num_a <- 0
now <- 0
arr_lst <- NULL
dep_lst <- NULL
wait_lst <- NULL
ta <- gen_arrival(now)
t1 <- Inf
t2 <- Inf

##################
# Updating System
##################

##########################
# Event: customer arrival
##########################
customer_arrival <- function() {
    # simulate customer arrival event
    now <<- ta  # fast forward time
    n <<- n + 1 # add 1 customer to system
    num_a <<- num_a + 1  # add 1 customer to total arrival count
    arr_lst[num_a] <<- now  # record the customer's arrival time
    ta <<- gen_arrival(now)  # generate next customer arrival time

    # allocate customers to different servers
    if (n == 1) {
        i1 <<- num_a  # allocate the customer to server 1
        t1 <<- now + gen_departure()
        # if this customer is the only customer in system
        # no waiting time for current customer
        wait_lst[i1] <<- 0
    } else if (n == 2) {
        # if there is another customer before this customer
        # there is no waiting time for current customer
        wait_lst[num_a] <<- 0
        #######################################
        # How to determine which server is free
        #######################################
        # when there is only 1 customer in the system (excluding new customer)
        # the customer will either be in server 1 or server 2
        # since the customer is still in the system
        # the departure time will be the latest timing
        # therefore, checking the max departure time of all servers
        # we will be able to determine which server is still serving customer
        # alternatively, we can check which server has a departure time
        # later than current time
        if (t1 == Inf) {
            # server 1 is free
            i1 <<- num_a
            t1 <<- now + gen_departure()
        } else {
            # server 2 is free
            i2 <<- num_a
            t2 <<- now + gen_departure()
        }
    }
}

##########################################
# Event: Customer departure from server 1
##########################################
customer_departure1 <- function() {
    # simulate departure event
    now <<- t1  # fast forward time to departure
    n <<- n - 1  # one less customer in the system
    dep_lst[i1] <<- now  # record the departure time for customer in server 1
    if (n <= 1) {
        # if there are no more customer in the system
        # or when there is only 1 customer in system (which will be in server2)
        # server 1 is going to be empty
        i1 <<- 0
        t1 <<- Inf
    } else {
        # if there is more than 1 customers
        # then server 1 still need to serve the next customer
        ############################################
        # determine the next customer to be served
        ############################################
        # since we have ordered the customer
        # where if i1 = 5, then that is the 5th customer arrived
        # then we can check for the max index of all servers
        # to know who is the latest customer being served
        # after this, we just need to call the next customer
        # which is simply the max + 1
        m <- max(i1, i2)
        i1 <<- m + 1  # server 1 serving the next customer
        wait_lst[i1] <<- now - arr_lst[i1]  # update waiting time
        t1 <<- now + gen_departure()
    }
}

##########################################
# Event: Customer departure from server 2
##########################################
# server 2 update is similiar to server 1 update
# even if we expand the server, the problem is not difficult
customer_departure2 <- function() {
    # simulate departure event
    now <<- t2  # fast forward time to departure
    n <<- n - 1  # one less customer in the system
    dep_lst[i2] <<- now  # record the departure time for customer in server 2
    if (n <= 1) {
        # if there are no more customer in the system
        # or when there is only 1 customer in system (which will be in server1)
        # server 2 is going to be empty
        i2 <<- 0
        t2 <<- Inf
    } else {
        # if there is more than 2 customers
        # then server 2 still need to serve the next customer
        ############################################
        # determine the next customer to be served
        ############################################
        # since we have ordered the customer
        # where if i1 = 5, then that is the 5th customer arrived
        # then we can check for the max index of all servers
        # to know who is the latest customer being served
        # after this, we just need to call the next customer
        # which is simply the max + 1
        m <- max(i1, i2)
        i2 <<- m + 1  # server 1 serving the next customer
        wait_lst[i2] <<- now - arr_lst[i2]  # update waiting time
        t2 <<- now + gen_departure()
    }
}

###########
# Running
# Note: this is a single run -> a single week
###########
run <- TRUE  # indicator for if to continue running
end <- 168  # simulate for 168 hours

while (run) {
    if (min(ta, t1, t2) <= end) {
        # before end time
        if (min(ta, t1, t2) == ta) {
            # arrival before end time
            customer_arrival()
        } else if (t1 <= t2) {
            # departure from server 1
            customer_departure1()
        } else {
            # departure from server 2
            customer_departure2()
        }
    } else if (n > 0) {
        # after end time
        # still have customers
        if (t1 <= t2) {
            # departure from server 1
            customer_departure1()
        } else {
            # departure from server 2
            customer_departure2()
        }
    } else {
        # after end time
        # no more customer
        run <- FALSE
    }
}

print(length(wait_lst))
print(median(wait_lst))
    }
)