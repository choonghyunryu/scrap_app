# Your authorized API keys
client_id <- "t9iL0Yqma3NV8lTJow87"
client_secret <- "QQ3gvfN8CE"

search_list <- koscrap::search_naver(
  "나폴레옹", 
  do_done = TRUE,
  client_id = client_id, client_secret = client_secret
)

library(tidytext)

search_list %>% 
  filter(nchar(description_text) > 0) %>% 
  unnest_tokens(noun, description_text, bitTA::morpho_mecab, type = "noun") %>% 
  group_by(noun) %>% 
  count() %>% 
  arrange(desc(n))

  
search_list %>% 
  filter(nchar(description_text) > 0) %>%   
  unnest_tokens(noun, description_text, bitTA::morpho_mecab, type = "noun") %>% 
  group_by(noun) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  wordcloud2::wordcloud2()

search_list %>% 
  filter(nchar(description_text) > 0) %>%   
  unnest_tokens(noun, description_text, bitTA::morpho_mecab, type = "noun") %>% 
  group_by(noun) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  filter(row_number() >= 10) %>% 
  wordcloud2::wordcloud2(fontFamily = "NamumSquare")
