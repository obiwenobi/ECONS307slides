library(rvest)
source('lib.R')

url <- "https://www.fr.fnac.be/l726/Nouveautes-Livre#bl=%20MMli"

site <- read_html(url)

site %>%
  html_elements(".Article-title.js-Search-hashLink") %>% 
  html_text()

price <- site %>% 
  html_elements("div.blocPriceBorder.blocPriceBorder--bgGrey") %>%   
  html_elements(".price") %>% 
  html_text() %>% 
  str_remove_all("€") %>% 
  tibble

colnames(price) <- "val"
  price %>% 
  mutate(val=str_replace(val,",","."),
          val=str_remove_all(val," ")) %>% 
  mutate(val=as.numeric(val))
  