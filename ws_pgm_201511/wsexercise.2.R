source("lib.R");

yn <- c("yes", "no");


#####
##### Exercise 2.1
#####


## P(R)
p.R <- parray("Rain", levels = list(yn), values = c(.2, .8));

## P(S|R)
p.S_R <- parray(c("Sprinkler", "Rain")
              , levels = list(yn,yn)
              , values = c(0.01, 0.99, 0.4, 0.6));

## P(G|S,R)
p.G_SR <- parray(c("GrassWet", "Sprinkler", "Rain")
               , levels = list(yn, yn, yn)
               , values = c(0.99, 0.01, 0.8, 0.2, 0.9, 0.1, 0, 1));

ftable(p.G_SR, row.vars = "GrassWet");



#####
##### Exercise 2.2
#####

## P(G,S,R)
p.GSR <- tabListMult( list(p.G_SR, p.S_R, p.R) );
ftable(p.GSR, row.vars = "GrassWet");



#####
##### Exercise 2.3
#####

p.RG  <- tabMarg(p.GSR, c("Rain", "GrassWet"));  ## P(R,G)
p.G   <- tabMarg(p.RG, "GrassWet");              ## P(G)
p.R_G <- tabDiv(p.RG, p.G);                      ## P(R|G)
