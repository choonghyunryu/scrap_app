# attach packages
library("shiny")
library("dplyr")
library("koscrap")
library("reactable")
library("htmltools")
library("tidytext")
library("wordcloud2")
library("colourpicker")

# create UDF
create_wordcloud <- function(data, remove_n = 5, min_freq = 5, background = "white") {
  data %>% 
    filter(nchar(description_text) > 0) %>%   
    unnest_tokens(noun, description_text, bitTA::morpho_mecab, type = "noun") %>% 
    group_by(noun) %>% 
    count() %>% 
    arrange(desc(n)) %>%     
    ungroup() %>%
    filter(n >= min_freq) %>% 
    filter(row_number() > remove_n) %>% 
    wordcloud2::wordcloud2(backgroundColor = background, 
                           fontFamily = "NamumSquare")
}

# Initialize global environments
