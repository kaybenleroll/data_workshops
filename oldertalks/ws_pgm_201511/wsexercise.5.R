source("lib.R");


#####
##### Exercise 5.1
#####

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

data(chestSim500, package = 'gRbase');

chestsim500.grain <- grain(chestclinic.dag, data = chestSim500);
chestsim500.grain <- compile(chestsim500.grain, propagate = TRUE, smooth = 0.1);


#####
##### Exercise 5.2
#####

querygrain(chestsim500.grain
         , nodes = c("lung", "bronc")
         , evidence = list(asia = 'yes'
                         , dysp = 'yes')
         , type = 'marginal');

querygrain(chestsim500.grain
         , nodes = c("lung", "bronc")
         , evidence = list(asia = 'yes'
                         , dysp = 'yes')
         , type = 'joint');

querygrain(chestsim500.grain
         , nodes = c("lung", "bronc")
         , evidence = list(asia = 'yes'
                         , dysp = 'yes')
         , type = 'conditional');


#####
##### Exercise 5.3
#####

chestsim500.ev.grain <- chestsim500.grain;
chestsim500.ev.grain <- setFinding(chestsim500.ev.grain, nodes = 'asia', states = 'yes', propagate = FALSE);
chestsim500.ev.grain <- setFinding(chestsim500.ev.grain, nodes = 'dysp', states = 'yes', propagate = FALSE);
chestsim500.ev.grain <- propagate(chestsim500.ev.grain);


querygrain(chestsim500.ev.grain
         , nodes = c("lung", "bronc")
         , type = 'marginal');

querygrain(chestsim500.ev.grain
         , nodes = c("lung", "bronc")
         , type = 'joint');

querygrain(chestsim500.ev.grain
         , nodes = c("lung", "bronc")
         , type = 'conditional');
