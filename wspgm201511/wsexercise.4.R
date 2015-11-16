source("lib.R");


#####
##### Exercise 4.1
#####

# Shortness-of-breath (dyspnoea) may be due to tuberculosis, lung
# cancer or bronchitis, or none of them, or more than one of them.

# A recent visit to Asia increases the chances of tuberculosis, while
# smoking is known to be a risk factor for both lung cancer and
# bronchitis.

# The results of a single chest X-ray do not discriminate between lung
# cancer and tuberculosis, as neither does the presence or absence of
# dyspnoea.

chestclinic.dag <- dag(list(
    "asia"
   ,c("tub", "asia")
   ,"smoke"
   ,c("lung", "smoke")
   ,c("bronc", "smoke")
   ,c("either", "lung", "tub")
   ,c("xray", "either")
   ,c("dysp", "bronc", "either")
    ));

#iplot(chestclinic.dag);



#####
##### Exercise 4.2
#####

data(chestSim500, package = 'gRbase');

chestsim500.grain <- grain(chestclinic.dag, data = chestSim500);
chestsim500.grain <- compile(chestsim500.grain, propagate = TRUE, smooth = 0.1);

data.table(chestSim500)[, .N, by = lung];



#####
##### Exercise 4.3
#####

querygrain(chestsim500.grain
         , nodes = 'lung'
         , evidence = list(asia = 'yes'
                         , dysp = 'yes'));



#####
##### Exercise 4.4
#####

data(chestSim1000, package = 'gRbase');

chestsim1000.grain <- grain(chestclinic.dag, data = chestSim1000, smooth = 0.1);
chestsim1000.grain <- compile(chestsim1000.grain, propagate = TRUE);

querygrain(chestsim1000.grain
         , nodes = 'lung'
         , evidence = list(asia = 'yes'
                         , dysp = 'yes'));



#####
##### Exercise 4.5
#####

data(chestSim10000,  package = 'gRbase');
data(chestSim50000,  package = 'gRbase');
data(chestSim100000, package = 'gRbase');


create.chestclinic.grain <- function(dag, data, smooth = 0.1) {
    chestsim.grain <- grain(dag, data = data, smooth = smooth);
    chestsim.grain <- compile(chestsim.grain, propagate = TRUE);

    return(chestsim.grain);
}

querygrain(create.chestclinic.grain(chestclinic.dag
                                  , chestSim10000
                                  , smooth = 0.1)
         , nodes = 'lung'
         , evidence = list(asia = 'yes'
                         , dysp = 'yes'));

querygrain(create.chestclinic.grain(chestclinic.dag
                                  , chestSim50000
                                  , smooth = 0.1)
         , nodes = 'lung'
         , evidence = list(asia = 'yes'
                         , dysp = 'yes'));

querygrain(create.chestclinic.grain(chestclinic.dag
                                  , chestSim100000
                                  , smooth = 0.1)
         , nodes = 'lung'
         , evidence = list(asia = 'yes'
                         , dysp = 'yes'));



#####
##### Exercise 4.6
#####

querygrain(create.chestclinic.grain(chestclinic.dag
                                  , chestSim500
                                  , smooth = 0.1)
         , nodes = c('lung', 'dysp', 'tub')
         , type = 'marginal');
