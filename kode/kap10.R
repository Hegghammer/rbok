#-----------------------------------
# Kode til kapittel 10 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("pdftools", "stringr", "readr", "staplr", "officer", "epubr", "magick", "tesseract", "glue", "hunspell", "daiR", "devtools"))
devtools::install_github(c("hegghammer/rforalle", "ropensci/tabulizer", "sckott/pdfimager"))

# 10.1 Tekst fra digitalfødte dokumenter ----------------------------------------

library(rforalle)
hent_data("kap10_bok.pdf")

library(pdftools)
tekst <- pdf_text("bok.pdf")

length(tekst)

tekst_samlet <- paste(tekst, collapse = " ")
cat(tekst_samlet)

library(stringr)
tekst_komprimert <- str_squish(tekst)
cat(tekst_komprimert)

write(tekst_samlet, "bok.txt")

library(readr)
tekst_fra_fil <- read_file("bok.txt")
cat(tekst_fra_fil)

hent_data("kap10_nou.pdf")

library(staplr)
select_pages(13, "nou.pdf", "nou_s13.pdf")

tekst_pdftools <- pdf_text("nou_s13.pdf")
cat(tekst_pdftools)

cat(str_squish(tekst_pdftools))

library(tabulizer)
tekst_tabulizer <- extract_text("nou_s13.pdf")
cat(tekst_tabulizer)

hent_data("kap10_strategi.docx")

library(officer)
doc <- read_docx("strategi.docx")

class(doc)

df_doc <- docx_summary(doc)

tekstkolonne <- df_doc$text

tekst_word <- paste(tekstkolonne, collapse = "\n")
cat(tekst_word)

hent_data("kap10_noveller.epub")

library(epubr)
ebok <- epub("noveller.epub")

ebok_data  <- ebok$data
class(ebok_data)

tekst_ebok <- ebok_data[[1]][["text"]]

tekst_ebok_samlet <- paste(tekst_ebok, collapse = " ")

substr(tekst_ebok_samlet, 1, 500)

word(tekst_ebok_samlet, 100, 200)

# 10.2 Andre typer data fra PDFer ----------------------------------------

info <- pdf_info("nou.pdf")

info$pages

info$keys

info$created

library(pdfimager)
pdimg_images("nou.pdf", base_dir = getwd())

sokestier <- list.files("nou", full.names = TRUE)

library(magick)

for (file in sokestier) {
  bilde <- image_read(file)
  nytt_navn <- gsub("ppm", "jpg", file)
  image_write(bilde, nytt_navn, format = "jpeg")
}

tabeller <- extract_tables("nou.pdf", pages = 214, output = "data.frame")

str(tabeller)

tabell <- tabeller[[2]]

tabell <- tabell[-c(1), ]

names(tabell) <- c("fag", "vekting_dagens", "vekting_timetall")

tabell$vekting_dagens <- gsub(",", ".", tabell$vekting_dagens)
tabell$vekting_timetall <- gsub(",", ".", tabell$vekting_timetall)

tabell$vekting_dagens <- as.numeric(tabell$vekting_dagens)
tabell$vekting_timetall <- as.numeric(tabell$vekting_timetall)

tabell <- na.omit(tabell)
tabell$fag[1] <- "Engelsk standpunkt (skriftlig og muntlig)"
tabell$fag[8] <- "Norsk standpunkt (hovedmål, sidemål og muntlig)"

tabell

# 10.3 Tekst fra bilder ----------------------------------------

hent_data("kap10_collett.pdf")

select_pages(15:17, "collett.pdf", "collett_utdrag.pdf")

library(tesseract)
# [NB: følgende er bare for Windows og Mac. På Linux installeres språkpakken med `sudo apt/dnf/pacman install tesseract-ocr-nor`]
tesseract_download("nor")

sider <- pdf_convert("collett_utdrag.pdf", dpi = 300)

tekst <- ocr(sider, engine = tesseract("nor"))

library(magick)
img <- image_read("collett_utdrag_1.png")
img <- image_colorize(img, 75, "green")
image_write(img, "green.png")

img <- image_read("collett_utdrag_1.png")
img <- image_trim(img)
img <- image_despeckle(img, times = 40)
img <- image_threshold(img, type = c("black", "white"), threshold = "60%")
image_write(img, "collett_utdrag_1_mod.png")

img <- image_read("collett_utdrag_1.png")
image_trim(img) %>%
  image_despeckle(times = 40) %>%
  image_threshold(type = c("black", "white"), threshold = "60%") %>%
  image_write("collett_utdrag_1_mod.png")

sider <- pdf_convert("collett_utdrag.pdf", dpi = 300)
tekst <- character()
for (i in seq_along(sider)) {
  img <- image_read(sider[i])
  txt <- image_trim(img) %>%
    image_despeckle(times = 40) %>%
    image_threshold(type = c("black", "white"), threshold = "60%") %>%
    image_write() %>%
    ocr(engine = tesseract("nor"))
  tekst <- paste(tekst, txt, collapse = "\n")
}
write(tekst, "collett_utdrag.txt")

library(glue)

pdfer <- pdf_split("collett_utdrag.pdf")

for (i in seq_along(pdfer)) {
  print(glue("Prosesserer PDF {i} av {length(pdfer)} .."))
  sider <- pdf_convert(pdfer[i], dpi = 300)
  tekst <- character()
  for (j in seq_along(sider)) {
    print(glue("Prosesserer side {j} av {length(sider)} .."))
    img <- image_read(sider[j])
    txt <- image_trim(img) %>%
      image_despeckle(times = 40) %>%
      image_threshold(type = c("black", "white"), threshold = "60%") %>%
      image_write() %>%
      ocr(engine = tesseract("nor"))
    tekst <- paste(tekst, txt, collapse = "\n")
  }
  filnavn <- basename(gsub( "pdf", "txt", pdfer[i]))
  write(tekst, filnavn)
}

tekst <- readLines("collett_utdrag.txt")
tekst <- tekst[-c(1:3)]
tekst <- gsub("\\||\\*|\\]|\\d+", "", tekst)
cat(tekst)

hent_data("kap10_bokmal.dic")
hent_data("kap10_bokmal.aff")

library(hunspell)
feil <- hunspell(tekst, dict = "bokmal.dic")
sort(unlist(feil))

tekst <- gsub("BRetragtninger", "Betragtninger", tekst)
tekst <- gsub("veålig", "venlig", tekst)

write(tekst, "collett_utdrag_ferdig.txt")

# [NB: Følgende kode fordrer konto i Google Cloud Services og oppsett for autentisering(se boken).]
library(daiR)
resp <- dai_sync("collett_utdrag.pdf")
tekst <- text_from_dai_response(resp)
cat(tekst)

library(staplr)
select_pages(8, "nou.pdf", "nou_s8.pdf")
resp <- dai_sync("nou_s8.pdf")

tekst <- text_from_dai_response(resp)
cat(tekst)

draw_blocks(type = "sync", output = resp)

# Opprensking
kan_slettes <- list.files(pattern = "png$|pdf$|txt$|epub$|docx$|dic$|aff$")
file.remove(kan_slettes)
