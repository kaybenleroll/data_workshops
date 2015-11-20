source("lib.R");


#####
##### Exercise 3.1
#####

gtvals  <- c("AA", "AB", "BB");
gtprobs <- dbinom(0:2, size = 2, prob = 0.7);



#####
##### Exercise 3.2
#####

## Us a bit of data.table (CJ is cross-join), construct the tables
## of inheritance probabilities (based on 50% chance of passing on
## one alleles
dt_tab <- CJ(child = gtvals, mother = gtvals, father = gtvals);
dt_tab <- dt_tab[, prob := mapply(calculate.allele.prob, child, mother, father)];

## We save the probabilities for use in CPTs
inheritance <- dt_tab[order(father, mother, child)]$prob


mother <- cptable(~mother, values = gtprobs, levels = gtvals);
father <- cptable(~father, values = gtprobs, levels = gtvals);
child  <- cptable(~child | mother + father, values = inheritance, levels = gtvals);



#####
##### Exercise 3.3
#####

cptlist <- compileCPT(list(child, mother, father));

genetic.family <- grain(cptlist);

plot(genetic.family);



#####
##### Exercise 3.4
#####

## Marginal distribution of the father's genotype
querygrain(genetic.family, nodes = "father")



#####
##### Exercise 3.5
#####

## Joint distribution of mother and child
ftable(querygrain(genetic.family, nodes = c("child", "mother"), type = "joint"), col.vars = "child");



#####
##### Exercise 3.6
#####

## Conditional distribution of the father given mother and chil
ftable(querygrain(genetic.family
                , nodes = c("father", "child", "mother")
                , type = "conditional")
     , col.vars = "father");



###
### Exercise 3.7
###

## P(m = BB, c = AB, f = AB)
p.fmc <- pEvidence(setEvidence(genetic.family
                             , evidence = list(mother = "BB"
                                             , child  = "AB"
                                             , father = "AB")));

## P(f = AB)
p.f <- pEvidence(setEvidence(genetic.family
                           , evidence = list(father = "AB")));

L.H1 <- p.fmc / p.f

## P(m = BB, c = AB)
L.H2 <- pEvidence(setEvidence(genetic.family
                            , evidence = list(mother = "BB"
                                            , child = "AB")));

## Likelihood ratio comparing Mr X vs unknown person.
L.H1 / L.H2



#####
##### Exercise 3.8
#####

## Create the network
## p(child | mother, father)
c.mf <- parray(c("child", "mother", "father")
             , levels = rep(list(gtvals), 3)
             , values = inheritance);

## p(father | grandmother, grandfather)
f.gmgf <- parray(c("father", "grandmother", "grandfather")
               , levels = rep(list(gtvals), 3)
               , values = inheritance);

## p(uncle | grandma, grandpa)
u.gmgf <- parray(c("uncle", "grandmother", "grandfather")
               , levels = rep(list(gtvals), 3)
               , values = inheritance);

## p(mother)
m  <- parray("mother", values = gtprobs, levels = list(gtvals));

## p(grandfather)
gf <- parray("grandfather", values = gtprobs, levels = list(gtvals));

## p(grandmother)
gm <- parray("grandmother", values = gtprobs, levels = list(gtvals));

extended.genetic.family.cptlist <- compileCPT(list(c.mf, m, f.gmgf, u.gmgf, gm, gf));

#rm(c.mf, f.gmgf, u.gmgf, m, gf, gm);

extended.genetic.family.grain <- grain(extended.genetic.family.cptlist);




#####
##### Exercise 3.9
#####

## P(m = BB, c = AB, u = AA)
pEvidence(setEvidence(extended.genetic.family.grain
                    , evidence = list(mother = "BB"
                                    , child  = "AB"
                                    , uncle  = "AA")));

### Another way to do this
ftable(querygrain(extended.genetic.family.grain
                , nodes = c("child", "mother", "uncle")
                , type = "joint")
     , col.vars = "uncle");
