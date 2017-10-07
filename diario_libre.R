library(xml2)
library(dplyr)
library(lubridate)
library(quanteda)

##funcion para quitar los html tags
cleanFun <- function(htmlString) {
        return(gsub("<.*?>", "", htmlString))
}

#### creando vector de las diferentes fuentes del diario libre
diario_libre <- c("https://www.diariolibre.com/rss/portada.xml",
                      "https://www.diariolibre.com/rss/noticias.xml", 
                      "https://www.diariolibre.com/rss/mundo.xml",
                      "https://www.diariolibre.com/rss/economia.xml",
                      "https://www.diariolibre.com/rss/opinion.xml",
                      "https://www.diariolibre.com/rss/ciencia-y-tecnologia.xml",
                      "https://www.diariolibre.com/rss/revista.xml",
                      "https://www.diariolibre.com/rss/deportes.xml",
                      "https://www.diariolibre.com/rss/estilos.xml")

#### lista de palabras adicionales al stopword de español


#funcion para leer el contenido de cada fuente para el diario libre
leer_contenido <- function(fuente){
        xml <- read_xml(fuente)
        titulo <- xml %>% xml_find_all("//item//title") %>% xml_text()
        cuerpo <- xml %>% xml_find_all("//item//description") %>% xml_text(trim = TRUE)
        fecha <- xml %>% xml_find_all("//item//pubDate") %>% xml_text()  %>% strptime( "%a, %d %b %Y %T", tz = "GMT") %>% with_tz("America/Caracas")
        id_noticia <- xml %>% xml_find_all("//item//id") %>% xml_text()
        autor <- xml %>% xml_find_all("//item//author") %>% xml_text()
        data.frame(titulo, cuerpo, fecha, id_noticia, autor, stringsAsFactors = FALSE)
}
#obteniendo lista de dataframes de cada fuente
diario_libre <- lapply(diario_libre,leer_contenido) %>% do.call(what = rbind)
diario_libre$cuerpo <- sapply(diario_libre$cuerpo, cleanFun)

#creating corpus
corpus <- corpus(diario_libre$cuerpo)
docvars(corpus, "fecha") <- diario_libre$fecha
docvars(corpus, "id noticia") <- diario_libre$id_noticia
docvars(corpus, "autor") <- diario_libre$autor
#token1
token1 <- tokens(char_tolower(corpus), what = "word", remove_punct = TRUE, 
                                                        ngrams= 1, 
                                                        remove_numbers = TRUE, 
                                                        remove_symbols = TRUE, 
                                                        remove_twitter = TRUE, 
                                                        remove_url = TRUE)
n1 <- dfm(token1, remove = stopwords("spanish"))
n1top <- dfm_trim(n1, 10) %>% topfeatures(nfeature(n1))
n1top <- data.frame("Palabra" = names(n1top), "Freq" = n1top)

#n_palabras <- sum(sapply(token1,length))


#token2
token2 <- tokens(char_tolower(corpus), what = "word", remove_punct = TRUE,
                 ngrams= 2,
                 remove_numbers = TRUE,
                 remove_symbols = TRUE,
                 remove_twitter = TRUE,
                 remove_url = TRUE)
n2 <- dfm(token2, remove = stopwords("spanish"))
n2top <- dfm_trim(n2, 5) %>% topfeatures(nfeature(n2))

#token3
token3 <- tokens(char_tolower(corpus), what = "word", remove_punct = TRUE,
                 ngrams= 3,
                 remove_numbers = TRUE,
                 remove_symbols = TRUE,
                 remove_twitter = TRUE,
                 remove_url = TRUE)
n3 <- dfm(token3, remove = stopwords("spanish"))
n3top <- dfm_trim(n3, 3) %>% topfeatures(nfeature(n3))

#printing the top used words
head(n1top)
head(n2top)
head(n3top)

#ploting
tokenInfo <- summary(corpus)
if (require(ggplot2))
        ggplot(data=tokenInfo, aes(x = as.Date(fecha), y = Tokens, group = 1)) + geom_line() + geom_point() +
        scale_x_discrete(labels = c(seq(1789,2012,12)), breaks = seq(1789,2012,12) )

##################################agrupando los resultados
list("top1" = n1top,
    "n_palabras" = n_palabras)
