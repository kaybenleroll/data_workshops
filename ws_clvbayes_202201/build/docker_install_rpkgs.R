remotes::install_github(
  "r-lib/fs",
  ref     = "4cc4b56c26b9d7f177a676fbb331133bb2584b86",
  upgrade = "never"
)

remotes::install_github(
  "business-science/tidyquant",
  ref     = "a1d9b836b0f040cd86cd3b627ddea701b8a91bcf",
  upgrade = "never"
)

remotes::install_github(
  "stan-dev/posterior",
  ref     = "02aaef5806f484e4e3b13aa1ff95124d590a097a",
  upgrade = "never"
)

remotes::install_github(
  "paul-buerkner/brms",
  ref     = "9b72e82941b941e084dc7a3e43e50460599e2639",
  upgrade = "always"
)

remotes::install_github(
  "stan-dev/cmdstanr",
  ref     = "v0.4.0",
  upgrade = "never"
)


library(cmdstanr)

cmdstan_flags <- list(
  "CXX"        = "clang++-9",
  "CXXFLAGS"   = "-Os -mtune=native -march=native  -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined",
  "CXX14"      = "clang++-9",
  "CXX14FLAGS" = "-Os -mtune=native -march=native  -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined"
)

install_cmdstan(
  cores       = parallel::detectCores(),
  cpp_options = cmdstan_flags
)

