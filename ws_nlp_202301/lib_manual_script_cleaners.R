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


clean_snatch <- function(orig_text) {
  clean_text <- orig_text |>
    str_replace("^\\s+\\(CONTINUED\\)",  "") |>
    str_replace("^\\s+\\(MORE\\)",       "") |>
    str_replace("^\\s+CUT TO.*",         "")


  clean_text[36]   <- ""
  clean_text[126]  <- "                                FRANKY FOUR FINGERS"
  clean_text[276]  <- "                                FRANKY FOUR FINGERS"
  clean_text[540]  <- ""
  clean_text[912]  <- "                                ALEX"
  clean_text[998]  <- "                                BAD BOY LINCOLN"
  clean_text[1067] <- "                                SOL"
  clean_text[1360] <- "                                MICKY"
  clean_text[2253] <- "                                MICKY"
  clean_text[2425] <- "                                TURKISH"
  clean_text[2981] <- ""
  clean_text[3124] <- "              Vince? Vince??"
  clean_text[8633] <- "                                KID 1"

  return(clean_text)
}


clean_strangers_on_a_train <- function(orig_text) {
  clean_text <- orig_text |>
    str_replace("^\\s+\\(MORE\\)",               "") |>
    str_replace("^\\s+DISSOLVE TO:",             "") |>
    str_replace(".*PDF by www.screentalk.org.*", "")

  return(clean_text)
}

clean_the_king_of_comedy <- function(orig_text) {

  clean_text <- orig_text |>
    str_replace("^\\s+CUT TO:",         "") |>
    str_replace("^\\s+FADE TO:",         "")


  ### Fix direction lines
  direction_idx <- c(
    821, 822, 828:830, 836, 837, 843:848, 854, 859, 870, 871, 1193:1195, 1223,
    1382, 1394, 1892:1895, 2063, 2109, 2139, 2144, 2196, 2213, 2230, 2278,
    2335, 2343, 2397:2401, 2418, 2419, 2428, 2429, 2449, 2542, 2549, 2558,
    2559, 2596, 2657:2659, 2688, 2713, 2714, 2722, 2723, 2729, 2730, 2738,
    2743:2746, 2925:2927, 2935, 2945, 2963:2972, 3146, 3180, 3181, 3230:3233,
    3348:3351, 3376, 3377, 3415, 3416, 3694, 3695, 3704, 3705, 3780, 3781,
    3811:3822, 3838, 3858, 3859, 3877, 3965, 3976, 4006, 4019, 4097, 4098,
    4161, 4178, 4179, 4193, 4194, 4206, 4224, 4225, 4231, 4232, 4254,
    4286:4288, 4479, 4480, 4485, 4493:4495, 4505, 4506, 4512:4516, 4526, 4576,
    4589, 4590, 4595, 4600, 4611, 4612, 4617, 4643, 4653, 4654, 4672:4674,
    4679, 4684, 4689:4694, 4712:4716, 4721, 4722, 4728, 4729, 4982, 5105,
    5110:5112, 5164, 5165, 5181:5184, 5196, 5207, 5208, 5242, 5263, 5264,
    5270, 5340, 5341, 5377, 5383, 5576, 5577, 5583, 5584, 5591:5594, 5657,
    5686, 5694, 5736, 5737, 5743:5746, 5754, 5781, 5803, 5817, 5828, 5829,
    5858, 5859, 6042, 6043, 6074, 6087, 6119, 6120, 6175, 6249, 6250, 6277,
    6286, 6287, 6293, 6299:6302, 6330, 6331, 6549, 6585:6587, 6611:6613, 6619,
    6620, 6625, 6666, 6667, 6684, 6701, 6717, 6739, 6858, 6868:6873,
    6900:6902, 6907, 6908, 6913:6915, 6940:6945, 7105:7112
    )

  clean_text[direction_idx] <- orig_text[direction_idx] |>
    str_trim() |>
    prepend_whitespace(2)


  ### Fix dialogue lines
  dialogue_idx <- c(
    3512, 3513, 3517, 3520, 3521, 3524, 3527, 3530, 3531, 3534, 3537:3542,
    3545:3548, 3551, 3554, 3555, 3558, 3559, 3562:3567
    )

  clean_text[dialogue_idx] <- orig_text[dialogue_idx] |>
    str_trim() |>
    prepend_whitespace(8)



  ### Make some manual edits
  clean_text[2719] <- "            Langford    asked him to call. (To PUPKIN)"
  clean_text[2720] <- "            Mr. Langford's secretary wants"
  clean_text[6933] <- "                         PUPKIN"


  return(clean_text)
}


clean_the_king_s_speech <- function(orig_text) {

  clean_text <- orig_text |>
    str_replace(".*TKS/Seidler.*", "")


  ### Fix dialogue direction lines
  dialogue_dir_idx <- c(
    184, 185, 233, 302, 405, 437, 454, 487, 511, 512, 692, 831, 847,848, 879,
    963, 977, 986, 1014, 1016, 1025, 1032, 1180, 1234, 1235, 1242, 1304, 1498,
    1499, 1624, 1625, 1626, 1635, 1637, 1849, 1850, 1930, 1964, 2109, 2110,
    2183, 2184, 2191, 2193, 2213, 2255, 2256, 2263, 2268, 2278, 2280, 2313,
    2342, 2377, 2379, 2405, 2407, 2410, 2466, 2467, 2468, 2593, 2627, 2630,
    2657, 2663, 2665, 2675, 2895, 2899, 2978, 2979, 2998, 3114, 3193, 3318,
    3332, 3349, 3397, 3401, 3581, 3585, 3593, 3594, 3922, 3923, 2934, 3944,
    3945, 3994, 4034, 4038, 4255, 4284, 4306, 4352, 4361, 4450, 4657, 4672,
    4763, 4771, 4781, 4803, 4916, 5245, 5411, 5429, 5514, 5632, 5766, 5820,
    5871, 5879, 5880, 5904, 5926, 5991, 6006, 6036, 6052, 6059, 6084, 6186,
    6268, 6270, 6271, 6286, 6296
    )

  clean_text[dialogue_dir_idx] <- orig_text[dialogue_dir_idx] |>
    str_trim() |>
    prepend_whitespace(35)


  char_line_idx <- c(
    2036, 2060, 2063, 2069, 2072, 2095, 2244, 2247, 2250, 2253, 2261, 2266,
    2272, 2275, 2284, 2296, 2678, 2684, 3277, 3365, 3368, 3373, 3376, 3380,
    3383, 3386, 3389, 3392, 3395, 3400, 3638, 3644, 3647, 3650, 3653, 3658,
    3661, 3664, 3668, 3671, 3676, 3684, 3687, 3692, 3697, 3701, 3704, 3707,
    3711, 3717, 3720, 3723, 3726, 3730, 3734, 3737, 3741, 3744, 3749, 3753,
    3758, 3762, 3766, 3769, 3772, 3775, 3778, 3784, 3790, 3793, 3797, 3914,
    3917, 3920, 3928, 3931, 3939, 3942, 3955, 3958, 3965, 4079, 4082, 4086,
    4090, 4094, 4100, 4103, 4107, 4110, 4114, 4118, 4121, 4124, 4128, 4963,
    4966, 4969, 4972, 4975, 4978, 4981, 4985, 4989, 4992, 4997, 5004, 5011,
    5014, 5130, 5133, 5138, 5142, 5145, 5148, 5151, 5155, 5158, 5162, 5167,
    5170, 5174, 5178, 5181, 5186, 5191, 5196, 5199, 5202, 5207, 5210, 5214,
    5217, 5220, 5226, 5230, 5234, 5405, 5410, 5417, 5420, 5428, 5432, 5435,
    5438, 5442, 5448, 5451, 5455, 5460, 5463, 5466, 5472, 5477, 5480, 5492,
    5495, 5502, 5505, 5508, 5511, 5577, 5580, 5583, 5588, 5593, 5597, 5604,
    5608, 5610, 5613, 5620, 5623, 5626, 5777
    )

  clean_text[char_line_idx] <- orig_text[char_line_idx] |>
    str_trim() |>
    prepend_whitespace(25)


  return(clean_text)
}


clean_the_last_samurai <- function(orig_text) {

  clean_text <- orig_text |>
    str_replace("^\\s+CUT TO:",         "") |>
    str_replace("^\\s+\\d+\\.", "")

  clean_text[182] <- ""

  return(clean_text)
}


clean_the_last_boy_scout <- function(orig_text) {

  clean_text <- orig_text |>
    str_replace("^\\s+CUT TO:",         "") |>
    str_replace("^\\s+\\(CONTINUED\\)", "") |>
    str_replace("^CONTINUED:", "") |>
    str_replace("^\\s+\\d+\\.", "")



  return(clean_text)
}
