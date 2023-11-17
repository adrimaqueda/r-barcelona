#librerías necesarias para el scrapping
library(tidyverse)
library(rvest)


## TOP 100 FILMS ----

#leemos el html que queremos scrapear
html <- read_html("https://www.imdb.com/chart/moviemeter/")

#creamos un archivo con todas las películas
films <- html |> html_nodes(".ipc-metadata-list-summary-item") |> html_nodes(".cli-children")

#sacamos el título de cada película
filmTitle <- films |> html_elements(".ipc-title__text") |> html_text()

#extraemos el cambio relativo
relativeChange <- films |> html_node(".cli-meter-title-header") |> html_element("span") |> html_attr("aria-label")
relativeChangeDirection <- str_split_i(relativeChange, " ", 2)
relativeChangePositions <- str_split_i(relativeChange, " ", 3)

#extraemos los metadatos
filmYear <- films |> html_node(".cli-title-metadata") |> html_element(xpath = "span[1]") |> html_text2()
filmDuration <- films |> html_node(".cli-title-metadata") |> html_element(xpath = "span[2]") |> html_text2()
filmPG <- films |> html_node(".cli-title-metadata") |> html_element(xpath = "span[3]") |> html_text2()

#extraemos los datos relativos a las valoraciones de cada película
filmRatingContainer <- tibble(
  rating = films |> html_node(".cli-ratings-container > span") |> html_attr("aria-label") |> str_split_i(pattern = " ", i=3),
  votes = films |> html_node(".cli-ratings-container > span > .ipc-rating-star--voteCount") |> html_text2()
  )

#generamos un tibble con toda la información
fullTibble <- tibble(filmTitle, filmYear, filmDuration, relativeChangeDirection, relativeChangePositions, filmPG, filmRatingContainer)

## FRIENDS ----

#guardamos la url base que luego vamos a usar para todas las temporadas
baseUrl <- "https://www.imdb.com/title/tt0108778/episodes/"

#creamos un tibble en el que vamos a guardar luego toda la información de los capítulos
friends <- tibble()

#creamos un loop para ir viendo la información de todas las temporadas

#la secuencia nos sirve para generar los números de todas las temporadas, en este caso, Friends, tiene 10
for (season in seq(1:10)) {
  
  #buscamos los capítulos de cada temporada
  episodesRaw <- read_html(paste0(baseUrl, "?season=", season)) |> 
    html_elements(".episode-item-wrapper") |> 
    html_element(xpath = "div/div/div[3]")
  
  #hacemos otro loop para sacar la información de cada capítulo
  for (episode in episodesRaw) {
    
    #importante calcular el número de nodos (divs) de cada episodio para poder sacar bien la información
    nodeLength <- length(episode |> html_children())
    
    #exraemos la información de cada episodio
    episode <- tibble(
      title = episode |> html_element(xpath = str_glue('div[{nodeLength-2}]/h4')) |> html_text2(),
      date = episode |> html_element(xpath = str_glue('div[{nodeLength-2}]/span')) |> html_text2(),
      rating = episode |> html_element(xpath = str_glue('div[{nodeLength}]/div/span/text()')) |> html_text2(),
      votes = episode |> html_element(xpath = str_glue('div[{nodeLength}]/div/span/span[2]')) |> html_text2()
    )
    
    #la guardamos en el tibble anterior
    friends <- friends |> bind_rows(episode)
    
  }
  
  print(str_glue('Season {season} done ✅'))
  
}

## CUÉNTAME ----

#El anterior código nos sirve para cualquier serie, únicamente adaptando el número de temporadas
#Por ejemplo lo podemos volver a usar para sacar la valoración de los capítulos de Cuéntame

baseUrl <- "https://www.imdb.com/title/tt0302447/episodes/"

#creamos un tibble en el que vamos a guardar luego toda la información de los capítulos
cuentame <- tibble()

#creamos un loop para ir viendo la información de todas las temporadas

#la secuencia nos sirve para generar los números de todas las temporadas, en este caso, Friends, tiene 10
for (season in seq(1:23)) {
  
  #buscamos los capítulos de cada temporada
  episodesRaw <- read_html(paste0(baseUrl, "?season=", season)) |> 
    html_elements(".episode-item-wrapper") |> 
    html_element(xpath = "div/div/div[3]")
  
  #hacemos otro loop para sacar la información de cada capítulo
  for (episode in episodesRaw) {
    
    #importante calcular el número de nodos (divs) de cada episodio para poder sacar bien la información
    nodeLength <- length(episode |> html_children())
    
    #exraemos la información de cada episodio
    episode <- tibble(
      title = episode |> html_element(xpath = str_glue('div[{nodeLength-2}]/h4')) |> html_text2(),
      date = episode |> html_element(xpath = str_glue('div[{nodeLength-2}]/span')) |> html_text2(),
      rating = episode |> html_element(xpath = str_glue('div[{nodeLength}]/div/span/text()')) |> html_text2(),
      votes = episode |> html_element(xpath = str_glue('div[{nodeLength}]/div/span/span[2]')) |> html_text2()
    )
    
    #la guardamos en el tibble anterior
    cuentame <- cuentame |> bind_rows(episode)
    
  }
  
  print(str_glue('Season {season} done ✅'))
  
}


## Otra opción es converir ese código en una función para no tener
## que estar copiando y pegando contínuamente 