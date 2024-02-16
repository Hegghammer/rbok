# Kilder

## Kapittel 4

- `kap04_malere.xlsx`: Bygget av Thomas Hegghammer med informasjon fra Wikipedia og Store norske leksikon (november 2023). 

## Kapittel 5

- `kap05_befolkning.csv`: https://www.ssb.no/statbank/table/05803. Lastet ned 9. desember 2023 og lettere bearbeidet med følgende kode: 

```r
#----------------------------------------
# Kode for å laste ned SSBs datasett
# "05803: Endringer i befolkningen i løpet av året 1735 - 2023",
# forenkle kolonnenavnene og lage filen kap05_befolkning.csv
#----------------------------------------

library(PxWebApiData)
library(tidyverse)

url <- "http://data.ssb.no/api/v0/no/table/05803"
df <- ApiData1(url, Tid = as.character(1735:2022), ContentsCode = TRUE) 

df <- df %>% 
    select(-NAstatus) %>% 
    pivot_wider(names_from = statistikkvariabel)

nye_kolonnenavn <- colnames(df) %>% 
   tolower() %>% 
   str_replace_all("1 000", "1000") %>% 
   str_replace_all(" ", "_") %>% 
   str_replace_all("\\.|\\(|\\)|,", "")

colnames(df) <- nye_kolonnenavn

write.csv(df, "kap05_befolkning.csv", row.names = FALSE)
```

## Kapittel 6

- `kap06_slag.csv`: Bygget av Thomas Hegghammer med informasjon fra Wikipedia (november 2023). 
- `kap06_konger.csv`: Bygget av Thomas Hegghammer med informasjon fra Wikipedia (november 2023). 
- `kap06_slekt.csv`: Bygget av Thomas Hegghammer med informasjon fra Wikipedia (november 2023). 
- `kap06_nettverk.csv`: Bygget av Thomas Hegghammer med informasjon fra Wikipedia (november 2023).

## Kapittel 7

- `kap07_fylker.shp`: https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_ISL_shp.zip (lastet ned 8. desember 2023).
- `kap07_fylker.shx`: https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_ISL_shp.zip (lastet ned 8. desember 2023).
- `kap07_fylker.dbf`: https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_ISL_shp.zip (lastet ned 8. desember 2023).
- `kap07_fylker.prj`: https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_ISL_shp.zip (lastet ned 8. desember 2023).
- `kap07_skjelv.txt`: http://hraun.vedur.is/ja/viku/2022/vika_36/listi (lastet ned 9. desember 2023).
- `kap07_folketall.csv`: https://px.hagstofa.is/pxen/pxweb/en/Ibuar/Ibuar__mannfjoldi__2_byggdir__Byggdakjarnarhverfi/MAN03250.px/. Lastet ned 14. desember 2023 med følgende kode:

```r
#----------------------------------------
# Kode for å laste ned datasettet
# "Population by regions, sex and age 1 January 1998-2023"
# fra Statistics Iceland via `pxweb`
#----------------------------------------

library(pxweb)

pxweb_query_list <- 
  list("Landshlutar"=c("Total","Capital region","Southwest","West","Westfjords","Northwest","Northeast","East","South"),
       "Aldur"=c("-1"),
       "Ár"= sapply(1998:2023, as.character),
       "Kyn"=c("0","1","2","3"))

px_data <- 
  pxweb_get(url = "http://px.hagstofa.is/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/Byggdakjarnarhverfi/MAN03250.px",
            query = pxweb_query_list)

df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

df <- df %>% 
  rename(Mannfjöldi = "Mannfjöldi eftir landshlutum, kyni og aldri 1. janúar 1998-2023")

write.csv(df, "kap07_folketall.csv", row.names = FALSE)
```

## Kapittel 10

- `kap10_bok.pdf`: https://oa.fagbokforlaget.no/index.php/vboa/catalog/download/29/44/550 (lastet ned 1. oktober 2023).
- `kap10_nou.pdf`: https://www.regjeringen.no/contentassets/8b06e9565c9e403497cc79b9fdf5e177/no/pdfs/nou201920190003000dddpdfs.pdf (lastet ned 1. oktober 2023).
- `kap10_strategi.docx`: https://harstad.kommune.no/eknet/docs/pub/DOK01920.docx (lastet ned 1. oktober 2023).
- `kap10_noveller.epub: http://www.bokselskap.no/wp-content/themes/bokselskap2/tekster/epub/somkvinderer.epub` (lastet ned 1. oktober 2023).
- `kap10_collett.pdf`: https://archive.org/download/erindringerogbe00collgoog/erindringerogbe00collgoog.pdf (lastet ned 1. oktober 2023).
- `kap10_bokmal.dic`: https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/nb/index.dic (lastet ned 13. november 2023).
- `kap10_bokmal.aff`: https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/nb/index.aff (lastet ned 13. november 2023).

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

- `kap11_stop_nrk.txt`: https://github.com/nrkno/samnorsk/blob/master/dict/stopwords/stopwords.txt.gz (lastet ned 17. oktober 2023). Kun utpakket, ingen endringer.
- `kap11_positiv.txt`: https://raw.githubusercontent.com/ltgoslo/norsentlex/master/Fullform/Fullform_Positive_lexicon.txt (lastet ned 13. november 2023). 
- `kap11_negativ.txt`: https://raw.githubusercontent.com/ltgoslo/norsentlex/master/Fullform/Fullform_Negative_lexicon.txt (lastet ned 13. november 2023).


## Kapittel 12

- `kap12_filmavis.ogv`: https://archive.org/download/Filmavisen_1941_08_11/Filmavisen_1941_11_08_1941.ogv (lastet ned 24. oktober 2023).
- `kap12_plakat.jpg`: https://commons.wikimedia.org/wiki/File:Propaganda_poster_of_Nasjonal_Samling_NS,_Norw._fascist_party._%22Norge_kaller%22._Unk._designer-illustrator._Approx._500_000_copies_1936-1945._Suncross_eagle._Exhibition_in_Justismuseet,_Trondheim_2019-03-07_DSC08611_cropped_cutout.jpg (27. oktober 2023).

## Kapittel 13

- `kap13_bull.jpg`: https://no.wikipedia.org/wiki/Ole_Bull#/media/Fil:Ole_Bull_cabinet_card_portrait.jpg (lastet ned 6. november 2023).
- `kap13_grieg.jpg`: https://no.wikipedia.org/wiki/Edvard_Grieg#/media/Fil:Eilif_Peterssen_-_Portrait_of_the_Composer_Edvard_Grieg_-_NG.M.00396_-_National_Museum_of_Art,_Architecture_and_Design.jpg (lastet ned 6. november 2023).
- `kap13_grøndahl.jpg`: https://no.wikipedia.org/wiki/Agathe_Backer_Gr%C3%B8ndahl#/media/Fil:Agathe_Backer_Gr%C3%B8ndahl.jpg (lastet ned 6. november 2023).
- `kap13_nordheim.jpg`: https://no.wikipedia.org/wiki/Arne_Nordheim#/media/Fil:Arne_Nordheim_(1968).jpg (lastet ned 6. november 2023).
- `kap13_test.bib`:
  
## Kapittel 14

- `kap14_komponister.Rmd`: Laget av Thomas Hegghammer (oktober 2023).
- `kap14_nefertiti.obj`: https://sketchfab.com/3d-models/nefertitis-bust-like-in-the-museum-ce5b14926e494558ab584375a8d63ca7 (lastet ned 10. november 2023).
- `kap14_texture.png`: https://sketchfab.com/3d-models/nefertitis-bust-like-in-the-museum-ce5b14926e494558ab584375a8d63ca7 (lastet ned 10. november 2023).