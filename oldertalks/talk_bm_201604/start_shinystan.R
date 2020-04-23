library(shinystan)

lgm_1_shinystan <- as.shinystan(lgm_1_stanfit)
mi_1_shinystan  <- as.shinystan(mi_1_stanfit)


launch_shinystan(lgm_1_shinystan, port = 4111)
launch_shinystan(mi_1_shinystan, port = 4112)
