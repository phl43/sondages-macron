library(tidyverse)
library(lubridate)
library(scales)
library(RSelenium)
library(rvest)

# NOTE: pour que ce script marche, il faut que Docker (https://www.docker.com) soit installé
# sur votre ordinateur et il faut qu'il soit ouvert avant d'exécuter le script

# cette fonction analyse le contenu d'un tweet et renvoie une structure de données qui contient la date
# à laquelle le sondage a été réalisé et la popularité de Macron d'après celui-ci
récupérer_infos_sondage <- function(tweet) {
  # exclut le tweet qui compare la popularité de Macron avant et après la révélation de l'affaire Benalla
  # pour éviter de compter les sondages en question deux fois
  if (str_detect(tweet, "benalla")) {
    return(tibble())
  }
  
  # met toutes les dates au même format dans le tweet, ce qui est nécessaire car
  # @EuropeElects n'utilise pas toujours le même format dans ses tweets
  months_full <- str_c(month.name, collapse = "|")
  months_abb <- str_c(month.abb, "\\.", collapse = "|")
  tweet <- str_replace_all(tweet,
                           months_full,
                           function(month) {str_pad(match(month, month.name), width = 2, pad = "0")})
  tweet <- str_replace_all(tweet,
                           months_abb,
                           function(month) {str_pad(match(month, str_c(month.abb, ".")), width = 2, pad = "0")})
  tweet <- str_replace_all(tweet,
                           "\\'(\\d{2})",
                           "\\1")
  if (!str_detect(tweet, "\\d{1,2}/\\d{2}/\\d{2,4}\\s*(-|–)\\s*\\d{1,2}/\\d{2}/\\d{2,4}")) {
    tweet <- str_replace_all(tweet,
                             "(\\d{1,2})\\s*(-|–)\\s*(\\d{1,2})/(\\d{2})/(\\d{2,4})",
                             "\\1/\\4/\\5 - \\3/\\4/\\5")
  }
  tweet <- str_replace_all(tweet,
                           "(\\d{1,2})\\s+(\\d{2})\\s*(-|–)\\s*(\\d{1,2})\\s+(\\d{2})\\s+(\\d{2,4})",
                           "\\1/\\2/\\6 - \\4/\\5/\\6")
  tweet <- str_replace_all(tweet,
                           "(\\d{1,2})\\s*(-|–)\\s*(\\d{1,2})\\s+(\\d{2})\\s+(\\d{2,4})",
                           "\\1/\\4/\\5 - \\3/\\4/\\5")
  tweet <- str_replace_all(tweet,
                           "(\\d{1,2})\\s+(\\d{2})\\s+(\\d{2,4})",
                           "\\1/\\2/\\3")
  
  # détermine la date à laquelle le sondage a été réalisé en prenant la moyenne du début et de la fin
  # quand le terrain s'est déroulé sur plusieurs jours
  date <- today()
  if (str_detect(tweet, "Field work[^\\d]*(\\d{1,2}/\\d{2}/\\d{2,4})\\s*(-|–)\\s*(\\d{1,2}/\\d{2}/\\d{2,4})")) {
    match <- str_match(tweet, "Field work[^\\d]*(\\d{1,2}/\\d{2}/\\d{2,4})\\s*(-|–)\\s*(\\d{1,2}/\\d{2}/\\d{2,4})")
    début <- dmy(match[1,2])
    fin <- dmy(match[1,4])
    intervalle <- interval(début, fin)
    date <- date(intervalle@start + as.duration(intervalle) / 2)
  } else if (str_detect(tweet, "Field work[^\\d]*\\d{1,2}/\\d{2}/\\d{2,4}")) {
    date <- dmy(str_match(tweet, "Field work[^\\d]*(\\d{1,2}/\\d{2}/\\d{2,4})")[1,2])
  } else {
    return(tibble())
  }
  
  # détermine le taux de popularité d'après le sondage
  popularité <- as.integer(str_match(tweet, "Approve[^\\d]*(\\d{1,2})%?")[1,2])
  
  # renvoie une structure avec les informations qui ont été récupérées
  return(tibble(date = c(date), popularité = c(popularité)))
}
 
# ouvre Chrome avec Selenium sur Docker, qui doit être déjà en train de tourner (j'utilise l'option -v /dev/shm:/dev/shm
# parce que sinon Chrome plante, cf. https://github.com/SeleniumHQ/docker-selenium/issues/79#issuecomment-133083785 pour
# l'explication)
system("docker run -v /dev/shm:/dev/shm -d -p 4445:4444 selenium/standalone-chrome")

# démarre RSelenium
rd <- remoteDriver(remoteServerAddr = "localhost",
                   port = 4445L,
                   browserName = "chrome")

# démarre une session (pour une raison que j'ignore, ça ne marche souvent pas la première fois, mais ça marche
# quand j'exécute le script une deuxième fois)
rd$open()

# url de la recherche sur Twitter des tweets de @EuropeElects qui donnent les résultats de sondages de
# popularité de Macron en tant que président
url <- "https://twitter.com/search?f=tweets&vertical=default&q=%22President%20Macron%20Approval%20Rating%22%20from%3AEuropeElects"

# ouvre l'url dans Chrome
rd$navigate(url)

# scroll vers le bas jusqu'à ce que tous les tweets soient affichés
last_page_length <- 0
page_length <- as.integer(rd$executeScript("return document.body.scrollHeight;"))
while(page_length != last_page_length) {
  last_page_length <- page_length
  rd$executeScript("window.scroll(0, document.body.scrollHeight);")
  Sys.sleep(1)
  page_length <- as.integer(rd$executeScript("return document.body.scrollHeight;"))
}

# récupère le code source de la page et lit celui-ci avec rvest
html <- rd$getPageSource()[[1]] %>% read_html()

# ferme la session
rd$close()

# arrête le serveur sur Docker
system("docker stop $(docker ps -q)")

# récupère les informations sur chaque sondage et crée une structure de données pour les stocker
sondages <- html %>%
  html_nodes(".TweetTextSize.js-tweet-text.tweet-text") %>%
  html_text() %>%
  map_df(récupérer_infos_sondage)

# produit un graphique avec un point pour chaque sondage et une estimation
# de la popularité de Macron au cours du temps par LOESS
ggplot(data = sondages, mapping = aes(x = date, y = popularité / 100)) +
  geom_point() + 
  geom_smooth(color = "orange", span = 0.15, method.args = list(degree = 1)) +
  theme_bw() +
  ggtitle("Popularité de Macron d'après les sondages publiés par @EuropeElects") +
  xlab("Date") +
  ylab("Popularité de Macron") +
  scale_x_date(labels = date_format("%m-%Y")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
