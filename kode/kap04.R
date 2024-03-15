#-----------------------------------
# Kode til kapittel 4 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("readxl", "writexl", "dplyr", "devtools"))
devtools::install_github("hegghammer/rforalle")
rforalle::hent_kode("kap04.R")

# 4.2 Funksjoner ----------------------------------------

plot(mtcars$hp, mtcars$mpg, col = "red")

# 4.3 Objekter, datatyper og strukturer ----------------------------------------

konvolutt <- "brev"

konvolutt

beskjed <- "Hello world!"
beskjed

print(beskjed)

print("Hello world!")
print("Hello world!")
print("Hello world!")

beskjed <- "Hello world!"
print(beskjed)
print(beskjed)
print(beskjed)

dikt <- "Kringsatt av fiender, gå inn i din tid!
Under en blodig storm vi dig til strid!
Kanskje du spør i angst, udekket, åpen:
hvad skal jeg kjempe med, hvad er mitt våpen?"

konvolutt <- c("brev", "passbilde")

tallrekke <- c(10, 20, 30, 40)

potpourri <- list("bil", 76, TRUE, 3.14)

land <- c("Norge", "Sverige", "Danmark")
hovedstad <- c("Oslo", "Stockholm", "København")
mill_innbyggere <- c(5, 10, 5)

df_skand <- data.frame(land, hovedstad, mill_innbyggere)

# 4.4 Vektorer ----------------------------------------

innbyggere_doblet <- mill_innbyggere * 2
innbyggere_pluss_en <- mill_innbyggere + 1

hovedstad_med_utropstegn <- paste(hovedstad, "!!")

falsk_tallrekke <- c("1", "2", "3")

ekte_tallrekke <- as.numeric(falsk_tallrekke)

norge_og_sverige <- land[1:2]

sum(df_skand$mill_innbyggere)

mean(df_skand$mill_innbyggere)

gjennomsnitt <- mean(df_skand$mill_innbyggere)

df_skand$km2 <- c(385000, 450000, 43000)

df_skand$km2[3]

# 4.5 Strenger ----------------------------------------

streng <- "Oslo er den vakreste byen i Skandinavia."

streng

print(streng)

View(streng)

cat(streng)

cat(dikt)

print(dikt)

paste(substr(dikt, 1, 56), "...")

cat("Oslo er den flotteste\nbyen i Skandinavia.")

streng_skiftet <- gsub("Oslo", "Stockholm", streng)
cat(streng_skiftet)

streng_fjernet <- gsub(" i Skandinavia", "", streng)
cat(streng_fjernet)

streng2 <- "Den hyggeligste er København."
streng_sammen <- paste(streng, streng2)
cat(streng_sammen)

streng_linjeskift <- paste(streng, "\n", streng2)
cat(streng_linjeskift)

streng_biter <- strsplit(streng, " ")

unlist(streng_biter)

substr(streng, 1, 3)

nchar(streng)

length(streng)

siste <- nchar(streng)
substr(streng, siste - 2, siste)

ord_liste <- strsplit(streng, " ")
ord_vektor <- unlist(ord_liste)
length(ord_vektor)

# 4.6 Filimport og -eksport ----------------------------------------

library(rforalle)
hent_data("kap04_malere.xlsx")

include_graphics(here("images", "excel.png"))

library(readxl)

df_malere <- read_excel("malere.xlsx")

library(writexl)
write_xlsx(df_skand, "skand_info.xlsx")

write.csv(df_skand, "skand_info.csv", row.names = FALSE)

"land","hovedstad","mill_innbyggere","km2"
"Norge","Oslo",5,385000
"Sverige","Stockholm",10,450000
"Danmark","København",5,43000

df_skand <- read.csv("skand_info.csv")

df <- read.csv("https://filesamples.com/samples/document/csv/sample4.csv")

# [NB: følgende to kommandoer er ment å gi feilmelding]
print ("Hello world")

data frame(vektor1, vektor2)

# 4.7 "Pen håndskrift" i R ----------------------------------------

plot(
  x = mtcars$hp,
  y = mtcars$mpg,
  col="red"
)

plot(
  x = mtcars$hp,
  y = mtcars$mpg,
  #col = "red"
)

if (4 > 3) {
  print("hello")
}

library(dplyr)
df <- mtcars |>
  select(cyl) |>
  filter(cyl > 6)

#-----------------------------------
# Notater fra "R for alle", kap. 4
# Thomas Hegghammer
# 22/9/2023
#-----------------------------------

print("Hello world!")

# 1. Modifisere datarammen ----------------------------------------

# 2. Tegne grafer -------------------------------------------------

# .............

# _____________

# ~~~~~~~~~~~~~

# *************

# =============

# ''''''''''''''

# ~~~~~~ PAKKER ~~~~~~~

# -------------- PAKKER

##############
# Del 1      #
##############

#~~~~~~~~~~~~~
# Del 1      |
#~~~~~~~~~~~~~

#-----------------------------
#            Del 1
#-----------------------------

#----------------------------------------
# Notater fra "R for alle", kap. 4
# Thomas Hegghammer
# 22/9/2023
#----------------------------------------

# Pakker --------------------------------
library(readxl)

# Data ----------------------------------
df_skand <- read_excel("skand_info.xlsx")

# Objekter -----------------------------
farge <- "red"

# Tegn graf -----------------------------
plot(df_skand$mill_innbyggere, col = farge)

# Opprensking
kan_slettes <- list.files(pattern = 'xlsx$')
file.remove(kan_slettes)
