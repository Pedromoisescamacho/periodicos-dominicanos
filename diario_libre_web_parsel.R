library(xml2)
library(dplyr)
library(rvest)
url <- "http://bit.ly/SKYAS"
noticia <- read_html(url)
titulo <- noticia %>% html_nodes("title") %>% html_text
contenido <- noticia %>% html_nodes("#layout-column_column-4 p") %>% html_text %>% 
                                as.list() %>% do.call(what = paste0)
fecha <- noticia %>% html_nodes(".art-date") %>% html_text
autor <- noticia %>% html_nodes("#layout-column_column-2 a") %>% html_text
resumen <- noticia %>% html_nodes(".art-sub") %>% html_text
data.frame(titulo, contenido, fecha, autor, resumen)