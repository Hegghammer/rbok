# Kilder

## Kapittel 10

- `kap09_1.pdf`: https://oa.fagbokforlaget.no/index.php/vboa/catalog/download/29/44/550 (lastet ned 1. oktober 2023).
- `kap09_2.pdf`: https://www.regjeringen.no/contentassets/8b06e9565c9e403497cc79b9fdf5e177/no/pdfs/nou201920190003000dddpdfs.pdf (lastet ned 1. oktober 2023).
- `kap09_3.docx`: https://harstad.kommune.no/eknet/docs/pub/DOK01920.docx (lastet ned 1. oktober 2023).
- `kap09_4.epub: http://www.bokselskap.no/wp-content/themes/bokselskap2/tekster/epub/somkvinderer.epub` (lastet ned 1. oktober 2023).
- `kap09_5.pdf`: https://archive.org/download/erindringerogbe00collgoog/erindringerogbe00collgoog.pdf (lastet ned 1. oktober 2023).

## Kapittel 11

- `kap11_program.csv`: Data fra https://www.nsd.no/data/individ/publikasjoner/Partidokumentarkivet/parti.zip (lastet ned 12. oktober 2023). Data hentet ut med følgende skript: 

```r
#----------------------------------------
# Kode for å trekke ut partiprogrammene til 
# Ap, H, V, Sp og KrF 1945-2021 fra 
# https://www.nsd.no/data/individ/publikasjoner/Partidokumentarkivet/parti.zip (lastet ned 12. oktober 2023)
# og lage filen kap11_program.csv
#----------------------------------------

library(xlsx)
library(readxl)
library(dplyr)
library(striprtf)
library(rvest)
library(tabulizer)
library(purrr)
library(tidyr)

dir.create("parti")
download.file("https://www.nsd.no/data/individ/publikasjoner/Partidokumentarkivet/parti.zip", destfile = "parti/parti.zip")
system("unzip parti/parti.zip")

excelfil <- "parti/partidokumentinfo.xlsx"
df <- read_excel(excelfil)
moderne_partier <- c("H", "Ap", "V", "Sp", "KrF")
valgår <- seq(1945, 2021, 4)

df2 <- df %>% 
    filter(PartiEintaltekst_forkorting %in% moderne_partier2) %>%
    filter(eintaltekst == "Valgprogram") %>%
    filter(aar %in% valgår) %>% 
    arrange(aar)
length(unique(df2$aar)) # 20 valg. Burde bli 100 programmer
# Men 125 dokumenter pga sametingsprogram etc. Må renske opp manuelt
write.xlsx(df2, "parti/til_rensking.xlsx")

# Manuell rensking her, lagret ny fil som rensket.xlsx

df_ny <- read_excel("parti/rensket.xlsx")
df_ny <- df_ny %>% select(dok_nr, tittel, aar, PartiEintaltekst_forkorting, filenameformat)

# Sp 2021 mangler. Hvor er det?
df2021 <- df %>% 
    select(dok_nr, tittel, aar, PartiEintaltekst_forkorting, filenameformat) %>% 
    filter(aar == 2021)
# Ah, det er feilkodet som "Prinsipprogram"

df_final <- rbind(df_ny, df2021[1,])
write.csv(df_final, "parti/partiprogram.csv", row.names = FALSE)

################

df <- df_final
# Evt:
# df <- read.csv("parti/partiprogram.csv")

df$sti <- paste0("parti/parti/", df$filenameformat)

get_text <- function(path) {
    if (grepl("rtf$", path)) {
        tk <- read_rtf(path)
        tekst <- paste0(tk, collapse = "\n\n")   
    } else if (grepl("html$", path)) {
        rå <- read_html(path)
        noder <- html_elements(rå, "p")
        tk <- html_text(noder)
        tekst <- paste0(tk, collapse = "\n\n")
    } else if (grepl("pdf$", path)) {
        tekst <- extract_text(path)
    }    
    return(tekst)
}

df <- df %>% 
    rowwise() %>% 
    mutate(tekst = get_text(sti))
df <- unnest(df)

write.csv(df, "parti/kap11_program.csv", row.names = FALSE)
```

- `stopwords_nrk.txt`: https://github.com/nrkno/samnorsk/blob/master/dict/stopwords/stopwords.txt.gz (lastet ned 17. oktober 2023). Kun utpakket, ingen endringer.

## Kapittel 12

- `kap12_filmavis.ogv`: https://archive.org/download/Filmavisen_1941_08_11/Filmavisen_1941_11_08_1941.ogv (lastet ned 24. oktober 2023).
- `kap12_plakat.jpg`: https://commons.wikimedia.org/wiki/File:Propaganda_poster_of_Nasjonal_Samling_NS,_Norw._fascist_party._%22Norge_kaller%22._Unk._designer-illustrator._Approx._500_000_copies_1936-1945._Suncross_eagle._Exhibition_in_Justismuseet,_Trondheim_2019-03-07_DSC08611_cropped_cutout.jpg (27. oktober 2023).

## Kapittel 13

- `kap13_bilde.jpg`: https://pxhere.com/nl/photo/905172 (lastet ned 5. november 2023). 
- `kap13_bull.jpg`: https://no.wikipedia.org/wiki/Ole_Bull#/media/Fil:Ole_Bull_cabinet_card_portrait.jpg (lastet ned 6. november 2023).
- `kap13_grieg.jpg`: https://no.wikipedia.org/wiki/Edvard_Grieg#/media/Fil:Eilif_Peterssen_-_Portrait_of_the_Composer_Edvard_Grieg_-_NG.M.00396_-_National_Museum_of_Art,_Architecture_and_Design.jpg (lastet ned 6. november 2023).
- `kap13_grøndahl.jpg`: https://no.wikipedia.org/wiki/Agathe_Backer_Gr%C3%B8ndahl#/media/Fil:Agathe_Backer_Gr%C3%B8ndahl.jpg (lastet ned 6. november 2023).
- `kap13_nordheim.jpg`: https://no.wikipedia.org/wiki/Arne_Nordheim#/media/Fil:Arne_Nordheim_(1968).jpg (lastet ned 6. november 2023).

## Kapittel 14

- `kap14_nefertiti.obj`: https://sketchfab.com/3d-models/nefertitis-bust-like-in-the-museum-ce5b14926e494558ab584375a8d63ca7 (lastet ned 10. november 2023).
- `kap14_texture.png`: https://sketchfab.com/3d-models/nefertitis-bust-like-in-the-museum-ce5b14926e494558ab584375a8d63ca7 (lastet ned 10. november 2023).