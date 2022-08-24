remotes::install_github(
  "paul-buerkner/brms",
  ref     = "v2.17.0",
  upgrade = "always"
)

remotes::install_github(
  "stan-dev/cmdstanr",
  ref     = "v0.5.3",
  upgrade = "never"
)


library(cmdstanr)

cmdstan_flags <- list(
  "CXX"        = "clang++",
  "CXXFLAGS"   = "-Os -mtune=native -march=native  -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined",
  "CXX14"      = "clang++",
  "CXX14FLAGS" = "-Os -mtune=native -march=native  -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined"
)

install_cmdstan(
  cores       = parallel::detectCores(),
  cpp_options = cmdstan_flags,
  quiet       = FALSE,
  overwrite   = FALSE,
  version     = "2.30.1"
)
