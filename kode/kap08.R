#-----------------------------------
# Kode til kapittel 7 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("stringr", "dplyr", "tidyr", "ggplot2", "purrr", "devtools"))
devtools::install_github("hegghammer/rforalle")

# 8.1 Tidyverse

library(stringr)
hilsen1 <- "Hello World"
hilsen2 <- str_replace(hilsen1, "World", "there")
hilsen3 <- tolower(hilsen2)
print(hilsen3)

print(tolower(str_replace(hilsen1, "World", "There")))

"Hello World" |>
  str_replace("World", "there") |>
  tolower() |>
  print()

"Hello World" |>
  str_replace("World", "there") |>
  tolower() |>
  print()

library(rforalle)
hent_data("kap05_befolkning.csv")
df_bef <- read.csv("befolkning.csv")

library(dplyr)
df_utvalg <- df_bef |> 
  select(år, befolkning_1_januar)

df_1800 <- df_bef |> 
  filter(år > 1799) |> 
  filter(år < 1900)

df_uten_år <- df_bef |> 
  select(!år)

df_uten_år_skils <- df_bef |> 
  select(!c(år, skilsmisser))

df_uten_1735 <- df_bef |> 
  filter(!(år == 1735))

df_skils_1990 <- df_bef |> 
  select(år, skilsmisser) |> 
  filter(år > 1990) |> 
  arrange(skilsmisser)

måned <- c("januar", "januar", "februar", "februar", "mars", "mars")
dag <- c(10, 20, 10, 20, 10, 20)
temperatur <- c(3, 12, 4, 2, 11, 4)
df_temp <- data.frame(måned, dag, temperatur)

df_snitt <- df_temp |> 
  group_by(måned) |> 
  summarize(temperatur = mean(temperatur))

df_snitt

library(tidyr)
df_temp_bred <- df_temp |> 
  pivot_wider(names_from = måned, values_from = temperatur)
df_temp_bred

df_temp_lang <- df_temp_bred |> 
  pivot_longer(cols = c(januar, februar, mars), names_to = "måned", values_to = "temperatur") 
df_temp_lang

df_temp_lang <- df_temp_bred |> 
  pivot_longer(cols = c(januar, februar, mars), names_to = "måned", values_to = "temperatur") |> 
  mutate(måned = factor(måned, levels = c("januar", "februar", "mars"))) |> 
  arrange(måned)
df_temp_lang

df_linjer <- df_bef |> 
  select(år, levendefødte_i_alt, døde_i_alt) |> 
  pivot_longer(cols = c(levendefødte_i_alt, døde_i_alt), names_to = "hendelse", values_to = "antall")

head(df_linjer)

library(ggplot2)
ggplot(df_linjer, aes(år, antall, color = hendelse)) +
  geom_line()

df_bef <- df_bef |> 
  mutate(innenfor = levendefødte_i_alt - fødte_utenfor_ekteskap) |> 
  rename(utenfor = fødte_utenfor_ekteskap)

df_bef_lang <- df_bef |> 
  select(år, innenfor, utenfor) |> 
  pivot_longer(cols = c(innenfor, utenfor), names_to = "type", values_to = "antall")

ggplot(df_bef_lang, aes(år, antall, fill = type)) +
  geom_bar(stat = "identity")

# 8.2 Iterasjon ----------------------------------------

navn <- c("Eva", "Pål", "Liv", "Tor", "Mia")

print(navn[1])
print(navn[2])
print(navn[3])
print(navn[4])
print(navn[5])

for (i in navn) { print(i) }

for (i in navn) {
# en kommentar
             # tidelibom
  print(i)

  }

for (i in navn) {
  print(i)
  }

denne_serien <- navn

for (hvert_element in denne_serien) {
  print(hvert_element)
}

# Bevisst feil
for (i in navn) {
  print(x)
}

for (i in navn) {
  print(i)
  filename <- paste0(i, ".txt")
  write(i, filename)
}

for (i in navn) {
  fil <- paste0(i, ".txt")
  file.remove(fil)
}

for (i in 1:length(navn)) {
  print(navn[i])
}

for (i in 1:5) {
  print(navn[i])
}

for (i in 1:3) {
  print(i)
  for (j in LETTERS[1:3]) {
    print(j)
  }
}

library(purrr)
map(navn, print)

print("Hello", quote = FALSE)

map(navn, ~ print(.x, quote = FALSE))

# 8.3 Betingelser ----------------------------------------

4 == 5

6 + 6 != 4 * 3

4 > 2

class(FALSE)

ole_saldo <- 6289.4
kari_saldo <- 7392.8
ole_saldo > kari_saldo

fruktkurv <- c("pærer", "bananer", "epler", "jordbær")
"epler" %in% fruktkurv

is.character(fruktkurv)

kari_saldo > 7000 && is.integer(kari_saldo)

"blåbær" %in% fruktkurv || "jordbær" %in% fruktkurv 

which(fruktkurv == "bananer")

df_bef <- read.csv("befolkning.csv")
which(df_bef$befolkning_1_januar > 5000000)

which(df_bef$befolkning_1_januar > 5000000) |> length()

df_5mill <- df_bef[which(df_bef$befolkning_1_januar > 5000000), ]

library(dplyr)
df_5mill_alt <- df_bef |>
  filter(befolkning_1_januar > 5000000)

df_bef$år[which.max(df_bef$skilsmisser)]

saldo <- ole_saldo

if (saldo > 7000) {
  print("Hurra!")
}

saldo <- kari_saldo

if (saldo > 7000) {
  print("Hurra!")
}

saldo <- ole_saldo

if (saldo > 7000) {
  print("Hurra!")
} else {
  print("Sorry Mac")
}

saldo <- ole_saldo

if (saldo > 7000) {
  print("Hurra!")
} else if (saldo <= 7000 && saldo > 5000) {
  print("Nesten")
} else {
  print("Sorry Mac")
}

alder <- c(14, 78, 59)
df <- data.frame(alder)

df$merkelapp <- ifelse(df$alder > 50, "gammel", "ung")

df

# 8.4 Lister ----------------------------------------

karaktervektor <- c("Hello world", 42, TRUE)
class(karaktervektor)

karaktervektor

liste <- list("Hello world", 42, TRUE)
liste

liste_nøstet <- list("Hello world", karaktervektor, liste)
liste_nøstet

liste_verdipar <- list(A = "Hello world", B = 42)
liste_verdipar

liste_verdipar_nøstet <- list("Hello world", liste_nøstet, A = 42, B = liste_verdipar)
liste_verdipar_nøstet

karaktervektor[2]

liste_verdipar_nøstet[[1]]

liste_verdipar_nøstet[[2]][[1]]

liste_verdipar_nøstet[[2]][[1]][[1]]

liste_verdipar_nøstet[[4]][[1]]

liste_verdipar_nøstet[["B"]]

liste_verdipar_nøstet$B

liste_verdipar_nøstet$B[["A"]][[1]]

for (i in liste) {
  print(i)
}

for (i in 1:length(liste)) {
  print(liste[[i]])
}

map(liste, print)

liste_tall <- list(3, 4)
liste_tall_nøstet <- list(1, 2, liste_tall)

tallvektor <- c(1, 2, 3, 4)
tallvektor * 2

liste_tall_nøstet * 2

utdata <- map(karaktervektor, print)
class(utdata)

utdata <- unlist(utdata)
class(utdata)

# Opprensking
kan_slettes <- list.files(pattern = "txt$|csv$|png$")
file.remove(kan_slettes)
