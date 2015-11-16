require(data.table);
require(gRbase);
require(gRain);




calculate.allele.prob <- function(child, mother, father) {
    child  <- strsplit(child,  "")[[1]];
    mother <- strsplit(mother, "")[[1]];
    father <- strsplit(father, "")[[1]];

    ## Probability of inheriting allele a from genotype gt
    P <- function(a, gt) ((a == gt[1]) + (a == gt[2])) / 2;

    if(child[1] != child[2]) {
        P(child[1], mother) * P(child[2], father) + P(child[1], father) * P(child[2], mother)
    } else {
        P(child[1], mother) * P(child[2], father)
    }
}
