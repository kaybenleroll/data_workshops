source("lib.R");


#####
##### Exercise 1.1
#####

diseasedata.dt <- generate.disease.test.data(n           = 100000
                                            ,prior.prob  = 0.001
                                            ,hit.rate    = 0.99
                                            ,false.alarm = 0.05);


calculate.disease.test.probabilities(diseasedata.dt)



#####
##### Exercise 1.2
#####

false.alarm.seq <- seq(0, 0.1, by = 0.0005)

infected.prob <- sapply(false.alarm.seq, function(iter.fa) {
    data.dt <- generate.disease.test.data(n           = 1000000
                                         ,prior.prob  = 0.001
                                         ,hit.rate    = 0.99
                                         ,false.alarm = iter.fa);

    calculate.disease.test.probabilities(data.dt)[infected == 1, prop];
})

qplot(false.alarm.seq, infected.prob, geom = 'line')



#####
##### Exercise 1.3
#####

twodisease.dt <- generate.disease.twotest.data(n             = 100000
                                              ,prior.prob    = 0.001
                                              ,hit.rate.1    = 0.99
                                              ,false.alarm.1 = 0.05
                                              ,hit.rate.2    = 0.99
                                              ,false.alarm.2 = 0.05)


false.alarm.seq <- seq(0, 0.1, by = 0.0005)

twotest.infected.prob <- sapply(false.alarm.seq, function(iter.fa) {
    data.dt <- generate.disease.twotest.data(n             = 1000000
                                            ,prior.prob    = 0.001
                                            ,hit.rate.1    = 0.99
                                            ,false.alarm.1 = iter.fa);

    data.dt[test.1 == 1 & test.2 == 1][, .N, by = infected][, .(infected, prop = N / sum(N))][infected == 1, prop]
})

qplot(false.alarm.seq, twotest.infected.prob, geom = 'line')
