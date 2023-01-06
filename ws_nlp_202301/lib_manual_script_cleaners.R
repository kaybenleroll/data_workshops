manually_clean_scripts <- function(input_file, output_file, film_key) {
  flag_edited <- FALSE

  orig_text <- read_lines(input_file)

  clean_funcname <- glue("clean_{film_key}")

  if(exists(clean_funcname)) {
    flag_edited <- TRUE

    message(glue("Manually cleaning {input_file}..."))

    cleaned_text <- get(clean_funcname)(orig_text)

  } else {
    message(glue("No cleaning function found for {input_file} - skipping..."))

    cleaned_text <- orig_text
  }

  cleaned_text %>% write_lines(file = output_file)

  return(flag_edited)
}


clean_austin_powers_international_man_of_mystery <- function(orig_text) {

  clean_text <- orig_text


  clean_text[2419] <- str_c("          ", orig_text[2419])
  clean_text[2421] <- "                              (beat)"

  clean_text[3049] <- "                              (pause)"

  clean_text[1730] <- str_replace(orig_text[1730], "AUSITN",   "AUSTIN")
  clean_text[3600] <- str_replace(orig_text[3600], "AUSTTIN",  "AUSTIN")
  clean_text[4816] <- str_replace(orig_text[4816], "AUSITN",   "AUSTIN")
  clean_text[5482] <- str_replace(orig_text[5482], "AUSITN",   "AUSTIN")

  clean_text[3147] <- str_replace(orig_text[3147], "BASIL EXPOSITON",  "BASIL EXPOSITION")
  clean_text[3196] <- str_replace(orig_text[3196], "BASIL EXPOSIION",  "BASIL EXPOSITION")

  clean_text[688]  <- str_replace(orig_text[688], "COMMAND GILMOUR", "COMMANDER GILMOUR")

  clean_text[2991] <- str_replace(orig_text[2991], "UNITED NATIONS SECRETATY", "UNITED NATIONS SECRETARY")

  clean_text[4890] <- str_c("               ", str_trim(orig_text[4890]))
  clean_text[4891] <- str_c("               ", str_trim(orig_text[4891]))
  clean_text[4892] <- str_c("               ", str_trim(orig_text[4892]))
  clean_text[4893] <- str_c("               ", str_trim(orig_text[4893]))
  clean_text[4894] <- str_c("               ", str_trim(orig_text[4894]))

  return(clean_text)
}



