#-----------------------------------
# Kode til kapittel 15 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("purrr", "ggplot2", "reprex", "rstudioapi", "grid", "glue", "devtools", "rollama")) 
devtools::install_github("hegghammer/rforalle")

# 15.1 Robust kode ----------------------------------------

navn <- c("Eva", "Pål", "Liv", "Tor", "Mia")

library(purrr)
map(navn, print)

printe_og_lagre <- function(inndata) {
  print(inndata)
  write(inndata, paste0(inndata, ".txt"))
}

printe_og_lagre("Eva")

map(navn, printe_og_lagre)

hent_a_stor <- function(vektor) {
  treff <- grep("a", vektor, value = TRUE)
  toupper(treff)
}

a_stor <- hent_a_stor(navn)
a_stor

hent_x_stor <- function(vektor, søk) {
  treff <- grep(søk, vektor, value = TRUE)
  toupper(treff)
}

i_stor <- hent_x_stor(navn, "i")
i_stor

hent_x_stor <- function(vektor, søk) {
  message(glue::glue("Ser etter elementer med bokstaven {søk} .."))
  treff <- grep(søk, vektor, value = TRUE)
  message(glue::glue("Fant {length(treff)} elementer."))
  toupper(treff)
}

hent_x_stor(navn, "l")

hent_x_stor <- function(vektor, søk) {
  if (! is.character(søk)) {
    stop("Søk-parameteret må være en tekstvektor.")
  }
  message(glue::glue("Ser etter elementer med bokstaven {søk} .."))
  treff <- grep(søk, vektor, value = TRUE)
  message(glue::glue("Fant {length(treff)} elementer."))
  toupper(treff)
}

hent_x_stor(navn, 4)

library(rforalle)
hent_data("kap05_befolkning.csv")
df <- read.csv("befolkning.csv")

library(ggplot2)
ggplot(df, aes(år, inngåtte_ekteskap)) +
  geom_line(color = "red", linewidth = 2) +
  ggtitle("Ekteskap") +
  theme_classic() +  
  theme(axis.title = element_blank(), 
        plot.title = element_text(size = 30))

ggplot(df, aes(år, levendefødte_i_alt)) +
  geom_line(color = "red", linewidth = 2) +
  ggtitle("Fødsler") +
  theme_classic() +  
  theme(axis.title = element_blank(), 
        plot.title = element_text(size = 30))

farge <- "red"
tykkelse <- 1.5
styling <- theme_classic() +
           theme(axis.title = element_blank(), 
                 plot.title = element_text(size = 20))
           
ggplot(df, aes(år, inngåtte_ekteskap)) +
  geom_line(color = farge, linewidth = tykkelse) +
  ggtitle("Ekteskap") +
  styling

ggplot(df, aes(år, levendefødte_i_alt)) +
  geom_line(color = farge, linewidth = tykkelse) +
  ggtitle("Fødsler") +
  styling

lag_figur <- function(variabel, tittel) {
  ggplot(df, aes(år, variabel)) +
  geom_line(color = "red", linewidth = 2) +
  ggtitle(tittel) +
  theme_classic() +  
  theme(axis.title = element_blank(), 
        plot.title = element_text(size = 20))  
}

lag_figur(df$inngåtte_ekteskap, "Ekteskap")
lag_figur(df$levendefødte_i_alt, "Fødsler")

# 15.2 Feilsøking ----------------------------------------

tilfeldige_tall <- runif(n = 10000, min = 0, max = 100)

identiske_tall <- rep(0, 10000)

frukt <- c("eple", "pære", "appelsin")
utvalg <- sample(frukt, 10000, replace = TRUE)

library(reprex)

print(1:10)

reprex()

reprex(print(1:10))

kode <- "print(1:10)"
writeLines(kode, "skript.R")
reprex("skript.R")

reprex(print(1:10), venue = "html")

# 15.3 God bruk av RStudio ----------------------------------------

# [NB: Ment å settes inn i .Rprofile]
message("Velkommen til RStudio. Jobb godt!")
options(scipen=999)
library(usethis)

# [NB: Ment å settes inn i .Renviron]
R_LIBS_USER="/Users/kristin/rpackages/"
GITHUB_PAT="xxxx"
OPENAI_API_KEY="xxxx"
MAMMAS_TLF="98765432"

mammas_nr <- Sys.getenv("MAMMAS_TLF")

Sys.setenv(BURSDAG = "1. januar")

library(rstudioapi)
file.create("dokument.qmd")
navigateToFile("dokument.qmd")

initializeProject("mitt_prosjekt")
openProject("mitt_prosjekt")

temaer <- getThemes()
names(temaer)

utgangspunkt <- getThemeInfo()$editor
applyTheme("vibrant ink")
applyTheme(utgangspunkt)

save.image("mandag.RData") 

load("mandag.RData")

df <- mtcars
saveRDS(df, "cars.rds")
df2 <- readRDS("cars.rds")

restartSession()
q()

# 15.4 R utenfor RStudio ----------------------------------------

# [NB: Følgende kommandoer er ment å kjøres i en terminal på Windows]
#----- START TERMINAL WINDOWS -----
setx path "%PATH%;<SØKESTI>"
setx path "%PATH%;C:\Program Files\R\R-4.3.2\bin\x64"
python -c "import os, sys; print(os.path.dirname(sys.executable))"
#----- SLUTT TERMINAL WINDOWS -----

# 15.5 Store språkmodeller og R ----------------------------------------

# [NB: Den dobbelt utkommenterte teksten nedenfor er instruksjoner til ChatGPT.]

## Jeg har en dataramme som ser slik ut:
##
## kolonne1 <- c("A", "B", "C")
## kolonne2 <- c(1, 2, 3)
## df <- data.frame(kolonne1, kolonne2)
##
## Hvordan gjør jeg den om til en tabell?

## Kan du omforme denne listen til en R-vektor?
## 
## - epler
## - pærer
## - appelsiner

## Kan du gjøre om denne litteraturreferansen til Bibtex-format?
## 
## Hermansen, Silje Synnøve Lyder. 2019. Lær deg R: En innføring i statistikkprogrammets muligheter. Oslo: Fagbokforlaget.

## Les teksten på disse to nettsidene:
## - https://no.wikipedia.org/wiki/Ole_Bull
## - https://no.wikipedia.org/wiki/Agathe_Backer_Gr%C3%B8ndahl
## 
## Jeg er interessert i følgende informasjon om personene:
## - navn
## - fødselsår
## - fødested
## - dødsår
## - yrke
## 
## Jeg vil ha disse dataene inn i en R-dataramme, slik at jeg kan lage en tabell. Jeg vil navnene som rader og informasjonstypen som kolonner. Kan du gi meg R-kode med informasjonen fra nettsidene ferdig utfylt?

## Jeg trenger et enkelt prosessdiagram som går fra venstre mot høyre. Det skal bestå av fire bokser som representerer suksessive stadier i livet: "Barndom", "Ungdomstid", "Voksenliv" og "Alderdom". Navnene på stadiene skal stå inni boksene. Boksene skal være henholdsvis lyserød, lysegrønn, gul og lyseblå. Svar med Mermaidkode.

## Lag et diagram med årsakene til andre verdenskrig. Svar med Mermaid-kode.

# [NB: Sett inn i .Renviron]
OPENAI_API_KEY="<DIN_APINØKKEL>"


library(purrr)
library(glue)

land <- c("Norge", "Sverige", "Danmark")

finn_hovedstad <- function(land) {
  spørsmål <- glue("Hva er hovedstaden i {land}? Svar med kun ett ord.")
  ask_chatgpt(spørsmål)
  Sys.sleep(20)
}

byer <- map_chr(land, finn_hovedstad)
byer

# [NB: Følgende kommandoer er ment å kjøres i terminalen] 
#----- START TERMINALEN
ollama run orca-mini
ollama pull mistral
# ----- SLUTT TERMINALEN -----

library(rollama)
prompt <- "I hvilket fylke ligger Bergen?"
svar <- query(prompt, model = "orca-mini")

svar$message$content

library(purrr)
library(glue)

land <- c("Norge", "Sverige", "Danmark")

finn_hovedstad <- function(land) {
  spørsmål <- glue("Hva er hovedstaden i {land}? Svar med kun ett ord.")
  svar <- query(spørsmål, "mistral")
  svar$message$content
}

byer <- map_chr(land, finn_hovedstad)
byer

finn_hovedstad <- function(land) {
  spørsmål <- glue("Hva er hovedstaden i {land}? Svar med kun ett ord.")
  svar <- query(spørsmål, "mistral")
  str_extract(svar$message$content, ".*(?=\\.)") |> str_squish()
}
byer <- map_chr(land, finn_hovedstad)
byer

# Opprensking
kan_slettes <- list.files(pattern = "txt$|csv$")
file.remove(c(kan_slettes, "mandag.RData", "cars.rds"))
