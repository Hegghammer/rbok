# Kilder

## Kapittel 10

- `kap09_1.pdf`: https://oa.fagbokforlaget.no/index.php/vboa/catalog/download/29/44/550
- `kap09_2.pdf`: https://www.regjeringen.no/contentassets/8b06e9565c9e403497cc79b9fdf5e177/no/pdfs/nou201920190003000dddpdfs.pdf
- `kap09_3.docx`: https://harstad.kommune.no/eknet/docs/pub/DOK01920.docx
- `kap09_4.epub: http://www.bokselskap.no/wp-content/themes/bokselskap2/tekster/epub/somkvinderer.epub`
- `kap09_5.pdf`: https://archive.org/download/erindringerogbe00collgoog/erindringerogbe00collgoog.pdf

## Kapittel 11

- `kap11_program.csv`: Data fra https://www.nsd.no/data/individ/publikasjoner/Partidokumentarkivet/parti.zip. Data hentet ut med følgende skript: 

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

write.csv(df, "parti/pprogram.csv", row.names = FALSE)
```