#-----------------------------------
# Kode til kapittel 9 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("frostr", "dplyr", "lubridate", "ggplot2", "PxWebApiData", "stortingscrape", "rvest", "RSelenium", "devtools"))
devtools::install_github(c("hegghammer/rforalle", "ropensci/internetarchive"))

# 9.1 Automatisert nedlasting ----------------------------------------

stamme <- "http://runeberg.org/img/kierlighed/00"
suffiks <- ".1.jpg"

url10_99 <- paste0(stamme, 10:99, suffiks)

url1_9 <- paste0(stamme, "0", 1:9, suffiks)

urler <- c(url1_9, url10_99)

dir.create("kierlighet")

for (lenke in urler) {
   filnavn <- basename(lenke)
   mappenavn <- "kierlighet"
   download.file(url = lenke, destfile = file.path(mappenavn, filnavn))
}

# 9.2 APIer ----------------------------------------

## Åpne `.Renviron`:
# usethis::edit_r_environ()

## Definér følgende miljøvariabel i `.Renviron`:
## FROST_CLIENT_ID="<DIN_CLIENT_ID>"
## Lagre og restart RStudio

min_id <- usethis::get_r_environ("FROST_CLIENT_ID")

library(frostr)
stasjoner <- get_sources(client_id = min_id)

library(dplyr)
eldste_stasjoner %>%
  arrange(validFrom) %>%
  head()
View(eldste_stasjoner)

vardø_id <- "SN98550"
utsira_id <- "SN47300"

sensorer <- get_elements(client_id = min_id)
View(sensorer)

årstemp <- "mean(air_temperature P1Y)"

tidsramme <- "1863-01-01/2023-01-01"

df <- get_observations(client_id = min_id,
                       sources = c(vardø_id, utsira_id),
                       elements = årstemp,
                       reference_time = tidsramme)
View(df)

library(lubridate)
library(dplyr)
df$år <- year(df$referenceTime)
df$sted <- case_match(df$sourceId, "SN47300:0" = "Utsira", "SN98550:0" = "Vardø")

df <- read.csv("data/værdata.csv")

library(ggplot2)
ggplot(df, aes(år, value, color = sted)) +
geom_line() +
geom_smooth() +
labs(title = "Gjennomsnittlig årstemperatur på Utsira og i Vardø, 1863-2023", x = "", y = "C")

library(PxWebApiData)
tabellnr <- "05803"
url_base <- "http://data.ssb.no/api/v0/no/table/"
url <- paste0(url_base, tabellnr)
df <- ApiData1(url, Tid = as.character(1735:2022), ContentsCode = TRUE)

library(dplyr)
bare_befolkning_df <- df %>%
  filter(statistikkvariabel == "Befolkning 1. januar")

library(stortingscrape)
get_parlperiods()$id

periode_id <- "1985-89"

person_id <- get_parlperiod_mps(periode_id) %>% 
  filter(lastname == "Kvanmo") %>% 
  select(mp_id) %>% 
  pull()

get_mp_pic(person_id, destfile = "images/kvanmo.jpg")

bio <- get_mp_bio(person_id)
names(bio)

alle <- get_parlperiod_mps(periode_id)

table(alle$gender)

table(alle$party_id)

library(internetarchive)
resultater <- ia_keyword_search("asbjørnsen og moe", num_results = 10)

oppføring <- ia_get_items(resultater[1])
filer <- ia_files(oppføring)
View(filer)

pdf_fil <- filer %>%
  filter(type == "pdf")
ia_download(pdf_fil)

# 9.3 Nettskraping

library(rvest)
url <- "https://no.wikipedia.org/wiki/Adolph_Tidemand"
tidemand <- read_html(url)

library(xml2)
write_xml(tidemand, "tidemand.html")

tidemand <- read_html("tidemand.html")

avsnittsnoder <- html_elements(tidemand, "p")

tekstbiter <- html_text(avsnittsnoder)

tekst <- paste(tekstbiter, collapse = " ")
tekst

bildenoder <- html_elements(tidemand, "img")

bildelenker <- html_attr(bildenoder, "src")

bildelenker_tidemand <- grep("upload", bildelenker, value = TRUE)

gyldige_urler <- paste0("https:", bildelenker_tidemand)

library(RSelenium)

# [Følgende kode fordrer fungerende RSelenium-oppsett (se boken).]
rD <- rsDriver()

rD <- rsDriver(browser = "firefox", chromever = NULL, phantomver = NULL, check = FALSE)

rD <- rsDriver(browser = "chrome", geckover = NULL, phantomver = NULL, check = FALSE)

remDr <- rD[["client"]]

remDr$navigate("https://fagbokforlaget.no")

remDr$navigate("https://nrk.no")
remDr$goBack()
remDr$goForward()
remDr$getCurrentUrl()
remDr$refresh()
remDr$screenshot(file = "skjermbilde.png")

remDr$navigate("https://media.digitalarkivet.no/kb/browse")

sti <- "/html/body/div[2]/div/div[1]/div[1]/div[2]/form/div[4]/span[2]/span[1]/span/ul/li/input"

felt <- remDr$findElement("xpath", sti)

input <- list("Lødingen")
felt$sendKeysToElement(input)

input <- list(key = "enter")
felt$sendKeysToElement(input)

input <- list("Lødingen", key = "enter")

remDr$findElement("xpath", "/html/body/div[2]/div/div[1]/div[1]/div[2]/form/
                  div[4]/span[2]/span[1]/span/ul/li/input")$sendKeysToElement(list("Lødingen", key = "enter"))

sti <- "//*[@id="end_year"]"
remDr$findElement("xpath", sti)$sendKeysToElement(list("1700"))

sti <- "//*[@id="searchBtn"]"
remDr$findElement("xpath", sti)$clickElement()

sti <- "/html/body/div[2]/div/div[2]/div/div[2]/div[2]/div/div/div/table/tbody/tr[1]/td[6]/a"
remDr$findElement("xpath", sti)$clickElement()

sti <- "/html/body/div[2]/div/div/div/div[3]/img"
bildeelement <- remDr$findElement("xpath", sti)

url_liste <- bildeelement$getElementAttribute("src")
url <- url_liste[[1]]
url

urler <- paste0("https://media.digitalarkivet.no/view/16671/", 1:105)

dir.create("kirkebok")
for (i in seq_along(urler)) {
  remDr$navigate(urler[i])
  Sys.sleep(2)
  elem <- remDr$findElement("xpath", "/html/body/div[2]/div/div/div/div[3]/img")
  url <- elem$getElementAttribute("src")[[1]]
  download.file(url, destfile = paste0("kirkebok/", i, ".jpg"))
}

remDr$close()

# Opprensking
kan_slettes <- list.files(pattern = "jpg$|pdf$|png$|html$")
file.remove(kan_slettes)
unlink("kierlighet", recursive = TRUE)
unlink("kirkebok", recursive = TRUE)
