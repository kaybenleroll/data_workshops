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

  cleaned_text |> write_lines(file = output_file)

  return(flag_edited)
}


prepend_whitespace <- function(text_str, prefix_len) {
  prep_str <- rep(" ", prefix_len) |> str_c(collapse = "")

  return(str_c(prep_str, text_str))
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


clean_go <- function(orig_text) {

  clean_text <- orig_text



  clean_text <- orig_text |>
    str_replace("^\\s+CUT TO:",         "") |>
    str_replace("^\\s+\\(CONTINUED\\)", "") |>
    str_replace(".*GO.*Revisions.*",    "")


  dialogue_idx <- c(41:42, 52:53, 62:64)

  clean_text[dialogue_idx] <- orig_text[dialogue_idx] |>
    str_trim() |>
    prepend_whitespace(15)


  return(clean_text)
}



clean_gone_baby_gone <- function(orig_text) {

  dialogue_str <- rep(" ", 50) |>
    str_c(collapse = "") |>
    str_c("\\1")

  clean_text <- orig_text |>
    str_replace(
      "^[ ]{30}[ ]*(\\S+)",
      rep(" ", 50) |> str_c(collapse = "") |> str_c("\\1")
      )

  clean_text[302] <- prepend_whitespace("Dot Avenue, where are you?", 30)
  clean_text[307] <- prepend_whitespace("Nguyen's nail salon.", 30)
  clean_text[312] <- prepend_whitespace("That's where she went?", 30)
  clean_text[317] <- prepend_whitespace("Yup. I'm getting my nails done. You still", 30)
  clean_text[318] <- prepend_whitespace("with the father?", 30)
  clean_text[323] <- prepend_whitespace("I lost him.", 30)
  clean_text[328] <- prepend_whitespace("You did?", 30)
  clean_text[333] <- prepend_whitespace("Lincoln Mercury Cougar, ninety-eight.", 30)
  clean_text[334] <- prepend_whitespace("Broadway and L St. Field Sobriety...", 30)
  clean_text[339] <- prepend_whitespace("Whoops, there he is.", 30)
  clean_text[342] <- prepend_whitespace("And he pulls out.", 10)
  clean_text[352] <- prepend_whitespace("On the side of the road, several OFFICERS administer a FIELD", 10)
  clean_text[353] <- prepend_whitespace("SOBRIETY TEST to the MAN.", 10)
  clean_text[356] <- prepend_whitespace("The Man is upset. He gestures at the Police.", 10)
  clean_text[359] <- prepend_whitespace("Patrick pulls up across the street, watching.", 10)
  clean_text[362] <- prepend_whitespace("They let the man go.", 10)
  clean_text[365] <- prepend_whitespace("Patrick eases out after him. He WAVES to the cops, who stare", 10)
  clean_text[367] <- ""
  clean_text[368] <- prepend_whitespace("back, not knowing him or why he is waving.", 10)
  clean_text[376] <- prepend_whitespace("From Patrick's car, we see the Father get out of his car and", 10)
  clean_text[377] <- prepend_whitespace("head in the door, holding his bag.", 10)
  clean_text[380] <- prepend_whitespace("Patrick watches from across the street, in his car.", 10)

  return(clean_text)
}


clean_legally_blonde <- function(orig_text) {

  clean_text <- orig_text

  dialogue_idx <- c(407:413, 421, 431:432, 450, 463, 468)

  clean_text[dialogue_idx] <- orig_text[dialogue_idx] |>
    str_trim() |>
    prepend_whitespace(14)

  clean_text[172] <- "SERENA" |> prepend_whitespace(24)

  clean_text[598] <- "(tearing up)" |> prepend_whitespace(18)

  return(clean_text)
}
