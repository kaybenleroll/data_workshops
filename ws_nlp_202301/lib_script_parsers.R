parse_film_script_one <- function(script_file) {

  detailed_parsing_tbl <- read_lines(script_file) %>%
    enframe(name = NULL, value = "line_text") %>%
    mutate(
      line_num = 1:n(),

      .before = 1
      ) %>%
    mutate(
      flag_empty_line         = str_detect(line_text, "^\\s*$"),
      flag_direction_line     = str_detect(line_text, "^[ ]{15}\\S"),
      flag_character_line     = str_detect(line_text, "^[ ]{37}\\S"),
      flag_dialogue_direction = str_detect(line_text, "^[ ]{30}\\S"),
      flag_dialogue_line      = str_detect(line_text, "^[ ]{25}\\S")
      ) %>%
    rowwise() %>%
    mutate(
      flag_other_line         = !any(c_across(flag_empty_line:flag_dialogue_line)),
      section_character       = if_else(flag_character_line == TRUE, str_trim(line_text), NA_character_),
      section_direction       = if_else(flag_direction_line, "Direction", NA_character_),
      section_title           = coalesce(section_character, section_direction)
      ) %>%
    ungroup() %>%
    fill(section_title, .direction = "down") %>%
    mutate(
      section_title = replace_na(section_title, replace = ""),
      new_grouping  = (section_title != lag(section_title)) %>% as.integer() %>% replace_na(replace = 0),
      grouping_id      = cumsum(new_grouping)
      )


  aggregated_parsing_tbl <- detailed_parsing_tbl %>%
    filter(grouping_id != 0) %>%
    group_by(section_title, grouping_id) %>%
    summarise(
      .groups = "drop",

      full_text = if_else(
        section_title[1] == "Direction",
        str_trim(line_text)                  %>% str_c(collapse = " "),
        str_trim(line_text) %>% tail(n = -1) %>% str_c(collapse = " ")
        )
      ) %>%
    arrange(grouping_id)


  script_parsing_tbl <- tibble(
    detailed   = list(detailed_parsing_tbl),
    aggregated = list(aggregated_parsing_tbl)
    )


  return(script_parsing_tbl)
}
