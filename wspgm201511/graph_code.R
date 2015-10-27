prob <- function(child, mother, father) {
    child  <- strsplit(child, "")[[1]]
    mother <- strsplit(mother, "")[[1]]
    father <- strsplit(father, "")[[1]]

    ## Probability of inheriting allele a from genotype gt
    P <- function(a, gt) ((a == gt[1]) + (a == gt[2]))/2

    if(child[1] != child[2]) {
        P(child[1], mother) * P(child[2], father) + P(child[1], father) * P(child[2], mother)
    } else {
        P(child[1], mother) * P(child[2], father)
    }
}




gts <- c("AA", "AB", "BB");

gtprobs <- dbinom(0:2, size = 2, prob = c(0.3, 0.7));


tab <- expand.grid(child = gts, mother = gts, father = gts, stringsAsFactors=FALSE)
tab$prob <- mapply(prob, tab$child, tab$mother, tab$father)
## We save the probabilities for use in CPTs
inheritance <- tab$prob







mother <- cptable(~mother, values = gtprobs, levels=gts);
father <- cptable(~father, values = gtprobs, levels=gts);

child <- cptable(~child | mother + father, values = inheritance, levels = gts);

cptlist <- compileCPT(list(child, mother, father));

trio <- grain(cptlist);
