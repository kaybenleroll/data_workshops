construct_film_script_parser <- function(parser_conf) {

  parser_label               <- parser_conf$parser_label
  direction_line_regex       <- parser_conf$direction_line_regex
  character_line_regex       <- parser_conf$character_line_regex
  dialogue_direction_regex   <- parser_conf$dialogue_direction_regex
  dialogue_line_regex        <- parser_conf$dialogue_line_regex


  parse_script_func <- function(script_file) {

    message(glue("{parser_label}: Parsing script file {script_file}"))

    detailed_parsing_tbl <- read_lines(script_file) |>
      enframe(name = NULL, value = "line_text") |>
      mutate(
        line_num = 1:n(),

        .before = 1
      ) |>
      mutate(
        flag_empty_line         = str_detect(line_text, "^\\s*$"),
        flag_direction_line     = str_detect(line_text, direction_line_regex),
        flag_character_line     = str_detect(line_text, character_line_regex),
        flag_dialogue_direction = str_detect(line_text, dialogue_direction_regex),
        flag_dialogue_line      = str_detect(line_text, dialogue_line_regex),

        flag_dialogue_line      = if_else(flag_character_line == TRUE, FALSE, flag_dialogue_line)
      ) |>
      rowwise() |>
      mutate(
        flag_other_line         = !any(c_across(flag_empty_line:flag_dialogue_line)),
        section_character       = if_else(flag_character_line == TRUE, str_trim(line_text), NA_character_),
        section_direction       = if_else(flag_direction_line, "Direction", NA_character_),
        section_title           = coalesce(section_character, section_direction)
      ) |>
      ungroup() |>
      fill(section_title, .direction = "down") |>
      mutate(
        section_title = replace_na(section_title, replace = ""),
        new_grouping  = (section_title != lag(section_title)) |> as.integer() |> replace_na(replace = 0),
        grouping_id   = cumsum(new_grouping)
      )


    aggregated_parsing_tbl <- detailed_parsing_tbl |>
      filter((grouping_id != 0) & (flag_other_line == FALSE)) |>
      group_by(section_title, grouping_id) |>
      summarise(
        .groups = "drop",

        full_text = if_else(
          section_title[1] == "Direction",
          str_trim(line_text)                 |> str_c(collapse = " "),
          str_trim(line_text) |> tail(n = -1) |> str_c(collapse = " ")
          ),
        trimmed_text = full_text |>
          str_replace_all("\\(.*?\\)", "") |>
          str_squish()

        ) |>
      arrange(grouping_id) |>
      mutate(
        flag_dialogue = section_title |>
          str_replace("\\(.*?\\)", "") |>
          str_detect("[a-z]+") %>%
          not()
        )


    script_parsing_tbl <- tibble(
      parsing_detailed   = list(detailed_parsing_tbl),
      parsing_aggregated = list(aggregated_parsing_tbl)
      )

    return(script_parsing_tbl)
  }

  return(parse_script_func)
}




