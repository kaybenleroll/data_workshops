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


clean_gone_baby_gone <- function(orig_text) {

  dialogue_str <- rep(" ", 50) %>%
    str_c(collapse = "") %>%
    str_c(., "\\1")

  clean_text <- orig_text %>%
    str_replace("^[ ]{30}[ ]*(\\S+)", rep(" ", 50) %>% str_c(collapse = "") %>% str_c(., "\\1"))

  prepend_whitespace <- function(prefix_len, text_str) {
    prep_str <- rep(" ", prefix_len) %>% str_c(collapse = "")

    return(str_c(prep_str, text_str))
  }

  clean_text[302] <- prepend_whitespace(30, "Dot Avenue, where are you?")
  clean_text[307] <- prepend_whitespace(30, "Nguyen's nail salon.")
  clean_text[312] <- prepend_whitespace(30, "That's where she went?")
  clean_text[317] <- prepend_whitespace(30, "Yup. I'm getting my nails done. You still")
  clean_text[318] <- prepend_whitespace(30, "with the father?")
  clean_text[323] <- prepend_whitespace(30, "I lost him.")
  clean_text[328] <- prepend_whitespace(30, "You did?")
  clean_text[333] <- prepend_whitespace(30, "Lincoln Mercury Cougar, ninety-eight.")
  clean_text[334] <- prepend_whitespace(30, "Broadway and L St. Field Sobriety...")
  clean_text[339] <- prepend_whitespace(30, "Whoops, there he is.")
  clean_text[342] <- prepend_whitespace(10, "And he pulls out.")
  clean_text[352] <- prepend_whitespace(10, "On the side of the road, several OFFICERS administer a FIELD")
  clean_text[353] <- prepend_whitespace(10, "SOBRIETY TEST to the MAN.")
  clean_text[356] <- prepend_whitespace(10, "The Man is upset. He gestures at the Police.")
  clean_text[359] <- prepend_whitespace(10, "Patrick pulls up across the street, watching.")
  clean_text[362] <- prepend_whitespace(10, "They let the man go.")
  clean_text[365] <- prepend_whitespace(10, "Patrick eases out after him. He WAVES to the cops, who stare")
  clean_text[367] <- ""
  clean_text[368] <- prepend_whitespace(10, "back, not knowing him or why he is waving.")
  clean_text[376] <- prepend_whitespace(10, "From Patrick's car, we see the Father get out of his car and")
  clean_text[377] <- prepend_whitespace(10, "head in the door, holding his bag.")
  clean_text[380] <- prepend_whitespace(10, "Patrick watches from across the street, in his car.")




  return(clean_text)
}
