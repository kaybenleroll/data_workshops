remotes::install_github(
  "paul-buerkner/brms",
  ref     = "v2.18.0",
  upgrade = "always"
)

remotes::install_github(
  "stan-dev/cmdstanr",
  ref     = "v0.5.3",
  upgrade = "never"
)


remotes::install_github(
  "rmcelreath/rethinking",
  ref     = "v2.2.1",
  upgrade = "never"
)


library(cmdstanr)

cmdstan_flags <- list(
  "CXX"        = "clang++",
  "CXXFLAGS"   = "-Os -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined",
  "CXX14"      = "clang++",
  "CXX14FLAGS" = "-Os -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined"
)

install_cmdstan(
  cores       = parallel::detectCores(),
  cpp_options = cmdstan_flags,
  quiet       = FALSE,
  overwrite   = FALSE,
  version     = "2.31.0"
)
