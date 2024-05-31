remotes::install_github(
  "pommedeterresautee/fastrtext",
  ref     = "b63c5de9a5168378e8e1abfc4a50be7292002bfb",
  upgrade = "never"
)

remotes::install_github(
  "quanteda/quanteda.corpora",
  ref     = "ec4b76d841afc9a734cc0351b1fa87236c83b456",
  upgrade = "never"
)

remotes::install_github(
  "kbenoit/quanteda.dictionaries",
  ref     = "b3c91606afad56603915fd622ef0aba4cc95135f",
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

