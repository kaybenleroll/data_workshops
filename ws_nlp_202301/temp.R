library(polite)
library(rvest)

target_url <- "https://imsdb.com/scripts/Austin-Powers---International-Man-of-Mystery.html"



scrape_session <- bow(target_url, force = TRUE)


scraped_page <- nod(scrape_session, target_url, verbose = TRUE) %>%
  scrape(verbose = TRUE, content = "text/html;charset=iso-8859-1")

scraped_text <- scraped_page %>%
  html_elements("pre") %>%
  html_text2() %>%
  extract2(2) %>%
  read_lines()
