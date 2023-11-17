#librerías necesarias para el scrapping
library(tidyverse)
library(rvest)

##SCRAPING ----

#leemos el html que queremos scrapear
html <- read_html("https://kingsleague.pro/partidos-split2/")

#accedemos a la información de cada jornada mediante el XPath
jornadas <- html |> html_nodes(xpath = "/html/body/div[1]/div[4]/div/div[1]/div/div")

#generamos un tibble vacío para guardar luego la información de cada jornada
tibble <- tibble()

#hacemos un loop para extraer la información
for (jornada in jornadas) {
  
  #sacamos la fecha de cada jornada
  fecha <-  jornada |> html_elements("h3") |> html_text2()
  
  #extraemos los datos de cada jornada, que, como está dentro de una tabla html, la podemos sacar usando html_table()
  partidos <- jornada |> html_table() |> mutate(fecha = fecha)
  
  #guardamos la información
  tibble <- tibble |> bind_rows(partidos)
  
  print(paste0("jornada ", fecha, " hecha ✅"))
}

##DATA WRANGLING ----
fullTibble <- tibble |> 
  rename(
      equipo1 = X1,
      hora = X4,
      equipo2 = X7,
      resultado = X8
    ) |> 
  select(!starts_with("X")) |>
  relocate(hora) |> 
  separate_wider_delim(fecha, " – ", names = c("jornada", "día")) |> 
  separate_wider_delim(resultado, " – ", names = c("goles1", "goles2")) |> 
  mutate(día = str_sub(día, 0, 10)) |> 
  mutate(
    penaltis1 = if_else(str_length(goles1) > 2, str_split_i(goles1, " ", 1) |> str_sub(2,2), "-"),
    penaltis2 = if_else(str_length(goles2) > 2, str_split_i(goles2, " ", 2) |> str_sub(2,2), "-"),
    goles1 = if_else(str_length(goles1) > 2, str_split_i(goles1, " ", 2), goles1),
    goles2 = if_else(str_length(goles2) > 2, str_split_i(goles2, " ", 1), goles2),
    winner = case_when(
      goles1 > goles2 ~ equipo1,
      goles2 > goles1 ~ equipo2,
      goles1 == goles2 & penaltis1 > penaltis2 ~ equipo1,
      T ~ equipo2
    ),
    loser = if_else(winner == equipo1, equipo2, equipo1)
  )

ranking <- fullTibble |> 
  filter(str_detect(jornada, "Jornada")) |> 
  mutate(penaltis = if_else(penaltis1 == "-", F, T)) |> 
  select(jornada, día, winner, loser, penaltis) |> 
  pivot_longer(!c(jornada, día, penaltis), names_to = "result", values_to = "team") |> 
  mutate(
    points = case_when(
      result == "winner" & penaltis == FALSE ~ 3,
      result == "winner" & penaltis == TRUE ~ 2,
      result == "loser" & penaltis == TRUE ~ 1,
      T ~ 0
    ),
    día = dmy(día)
  ) |>
  select(día, jornada, team, points) |> 
  arrange(día) |>
  group_by(team) |> 
  mutate(
    total_points = cumsum(points),
  ) |>
  ungroup() |> 
  group_by(día) |>
  arrange(desc(total_points)) |> 
  mutate(rank = row_number()) |>
  ungroup() |>
  arrange(desc(día), desc(total_points)) |>
  group_by(team) |> 
  mutate(
    variation = lead(rank) - rank
  ) |> 
  ungroup() |> 
  filter(día == max(día))

#guardamos el archivo del ranking
write_csv(ranking, "ejemplos-scrapping/rankingKL.csv")








