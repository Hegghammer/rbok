#-----------------------------------
# Kode til kapittel 15 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("purrr", "ggplot2", "reprex", "rstudioapi", "grid", "glue", "devtools"))
devtools::install_github("hegghammer/rforalle")

# 15.1 Robust kode ----------------------------------------

navn <- c("Eva", "Pål", "Liv", "Tor", "Mia")

library(purrr)
map(navn, print)

printe_og_lagre <- function(input) {
  print(input)
  write(input, paste0(input, ".txt"))
}

printe_og_lagre("Eva")

map(navn, printe_og_lagre)

hent_a_stor <- function(vektor) {
  treff <- grep("a", vektor, value = TRUE)
  treff_versal <- toupper(treff)
  return(treff_versal)
}

a_stor <- hent_a_stor(navn)
a_stor

hent_x_stor <- function(vektor, søk) {
  treff <- grep(søk, vektor, value = TRUE)
  treff_versal <- toupper(treff)
  return(treff_versal)
}

i_stor <- hent_x_stor(navn, "i")
i_stor

hent_x_stor <- function(vektor, søk) {
  message(glue::glue("Ser etter elementer med bokstaven {søk} .."))
  treff <- grep(søk, vektor, value = TRUE)
  treff_versal <- toupper(treff)
  message(glue::glue("Fant {length(treff)} elementer."))
  return(treff_versal)
}

hent_x_stor(navn, "l")

hent_x_stor <- function(vektor, søk) {
  if (! is.character(søk)) {
    stop("Søk-parameteret må være en karaktervektor.")
  }
  message(glue::glue("Ser etter elementer med bokstaven {søk} .."))
  treff <- grep(søk, vektor, value = TRUE)
  treff_versal <- toupper(treff)
  message(glue::glue("Fant {length(treff)} elementer."))
  return(treff_versal)
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

styling <- function() {
  theme_classic() +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 20))
  }

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

# [NB Denne kommandoen fordrer at du har definert miljøvariabelen MAMMAS_TLF i .Renviron.]
mammas_nr <- Sys.getenv("MAMMAS_TLF")

Sys.setenv(BURSDAG = "1. januar")

# [NB: Følgende kommandoer fungerer bare i RStudio, ikke i VSCode eller andre steder.]
library(rstudioapi)
file.create("dokument.Rmd")
navigateToFile("dokument.Rmd")
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

# [NB: Følgende kommandoer er ment å kjøres i terminalen på Windows.]
setx path "%PATH%;<SØKESTI>"
setx path "%PATH%;C:\Program Files\R\R-4.3.2\bin\x64"
python -c "import os, sys; print(os.path.dirname(sys.executable))"

# 15.5 ChatGPT og R ----------------------------------------

# [Den dobbelt utkommenterte teksten nedenfor er instruksjoner til ChatGPT.]

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

## Jeg trenger et enkelt prosessdiagram med fire bokser som representerer suksessive stadier fra venstre mot høyre.
## - Stadiene er "Barndom", "Pubertet", "Voksenliv" og "Alderdom".
## - Boksene skal være kvadratiske, ha ulik farge (rød, grønn, lyseblå og oransje) og runde kanter.
## - Navnene på stadiene skal stå inni boksene.
## - Det skal være tykke piler mellom alle boksene.
## - Diagrammet skal tittelen "Menneskets livsløp", i stor font.
## - Nede til høyre i liten skrift skal det stå "Laget med ChatGPT".
## Svar med R-kode som kan generere diagrammet.

library(ggplot2)
library(grid)

# Definerer stadiene og deres egenskaper
stages <- data.frame(
  x = c(1, 3, 5, 7),  # X-posisjonene for boksene
  y = rep(2, 4),       # Y-posisjonene for boksene
  label = c("Barndom", "Pubertet", "Voksenliv", "Alderdom"),
  color = c("red", "green", "lightblue", "orange")
)

# Oppretter grunnlaget for plottet
p <- ggplot() +
  geom_rect(data = stages, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = y - 0.5, ymax = y + 0.5, fill = color), color = "black", size = 1) +
  geom_text(data = stages, aes(x = x, y = y, label = label), size = 6) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_fill_identity() +
  labs(title = "Menneskets livsløp") +
  theme(plot.title = element_text(size = 20))

# Legger til piler
for (i in 1:(nrow(stages) - 1)) {
  p <- p + geom_segment(aes(x = stages$x[i] + 0.5, y = stages$y[i], xend = stages$x[i + 1] - 0.5, yend = stages$y[i]),
                        arrow = arrow(type = "open", length = unit(0.15, "inches")), size = 1.5)
}

# Legger til fotnote
p <- p + annotate("text", x = 7, y = 1.5, label = "Laget med ChatGPT", size = 4)

# Viser plottet
print(p)

library(purrr)
library(glue)
land <- c("Norge", "Sverige", "Danmark")
finn_hovedstad <- function(land) {
  by <- ask_chatgpt(glue("Hva er hovedstaden i {land}? Svar med kun ett ord."))
  return(by)
}
byer <- map(land, finn_hovedstad)
byer

# Opprensking
kan_slettes <- list.files(pattern = "txt$|csv$")

file.remove(c(kan_slettes, "mandag.RData", "cars.rds"))
