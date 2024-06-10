remotes::install_github(
  "stan-dev/cmdstanr",
  ref     = "v0.8.0",
  upgrade = "never"
)


remotes::install_github(
  "rmcelreath/rethinking",
  ref     = "v2.2.1",
  upgrade = "never"
)


library(cmdstanr)

cmdstan_flags <- list(
  "CC"         = "clang",
  "CXX"        = "clang++",
  "CXXFLAGS"   = "-Os -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined",
  "CC11"       = "clang",
  "CXX11"      = "clang++",
  "CXX11FLAGS" = "-Os -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined",
  "CC14"       = "clang",
  "CXX14"      = "clang++",
  "CXX14FLAGS" = "-Os -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined",
  "CC17"       = "clang",
  "CXX17"      = "clang++",
  "CXX17FLAGS" = "-Os -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas -Wno-macro-redefined"
)

install_cmdstan(
  cores       = parallel::detectCores(),
  cpp_options = cmdstan_flags,
  quiet       = FALSE,
  overwrite   = FALSE,
  version     = "2.35.0"
)
