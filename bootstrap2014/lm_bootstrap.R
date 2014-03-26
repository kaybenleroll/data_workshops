
cats.lm <-lm(Hwt ~ Bwt, data = catsM);

cats1 <- catsM;

cats1$fit <- fitted(cats.lm);
cats1$res <- resid(cats.lm);

cats.fit <- function(data) {
    mod <- lm(Hwt ~ Bwt, data = data);

    return(c(coef(mod), summary(mod)$coef[, 2]^2));
}

case.fun <- function(d, i) cats.fit(d[i, ])

model.fun <- function(d, i) {
    d$Hwt <- d$fit + d$res[i];

    cats.fit(d)
}

cats.case <- boot(cats1, case.fun,  R = 999);
cats.mod  <- boot(cats1, model.fun, R = 999);
