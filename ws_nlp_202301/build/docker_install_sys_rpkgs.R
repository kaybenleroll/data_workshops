remotes::install_github(
  "pommedeterresautee/fastrtext",
  ref     = "b63c5de9a5168378e8e1abfc4a50be7292002bfb",
  upgrade = "never"
)

install.packages("StanfordCoreNLP",
  dependencies = TRUE,
  repos        = "http://datacube.wu.ac.at/"
)

install.packages("openNLPmodels.en",
  dependencies = TRUE,
  repos        = "http://datacube.wu.ac.at/"
)

