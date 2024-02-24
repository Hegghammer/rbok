#-----------------------------------
# Kode til kapittel 13 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("readxl", "knitr", "lorem", "kableExtra", "dplyr", "rmarkdown", "tinytex", "devtools"))
devtools::install_github("hegghammer/rforalle", "rfortherestofus/pagedreport")

# [Følgende kommando må også kjøres for at du skal kunne strikke .Rmd-filer til PDF.]
tinytex::install_tinytex()

# [NB: Mye av materialet i dette skriptet er ikke R-kode, men RMarkdown, Markdown, HTML, LaTeX eller BibTeX. Dette er ment å settes inn i .Rmd-dokumenter og er derfor kommentert ut med doble nummertegn.]

# 13.3 RMarkdown-formatets bestanddeler ----------------------------------------

## ---
## title: Mitt første RMarkdown-dokument
## author: Meg selv
## date: 2025-01-01
## output: word_document
## ---

## ---
## title: Mitt første RMarkdown-dokument
## output:
##   pdf_document:
##     number_sections: true
## ---

## ---
## title: Mitt første RMarkdown-dokument
## date: \today
## ---

## ---
## title: Mitt første RMarkdown-dokument
## date: "`r format(Sys.time(), '%d. %B %Y, kl. %H:%M')`"
## ---

## # En overskrift på nivå 1
## 
## ## En underoverskrift (nivå 2)
## 
## ### En under-underoverskrift (nivå 3)

## Her er *noe veldig viktig*.

## Dette er **noe som fortjener utheving**.

## 1. Første element
##     a. Del en
##     b. Del to
## 2. Andre element

## - En poeng
## - Enda et poeng

## ![](bilde.jpg)

## Jeg anbefaler å lese [denne nettsiden](https://nrk.no).

## Jeg liker iterasjon med funksjonen `print()`.
## 
## ```
## for (i in 1:10) {
##   print(i)
## }
## ```

## Denne påstanden trenger en referanse.[^ref]
## 
## [^ref]: Her er kildehenvisningen.

## Dette blir avsnitt 1.
## 
## Dette blir avsnitt 2.
## 
## Denne setningen har to mellomrom etter seg.
## Da får jeg linjeskift med neste setning rett under.

## ```{r}
## plot(mtcars$mpg)
## ```

## ```{r mpg}
## plot(mtcars$mpg)
## ```

## ```{r mpg, fig.cap = "Miles per gallon"}
## plot(mtcars$mpg)
## ```

## ```{r mpg, echo = FALSE, fig.cap = "Miles per gallon"}
## plot(mtcars$mpg)
## ```

## ```{r mpg}
## #| echo = FALSE,
## #| fig.cap = "Miles per gallon"
## plot(mtcars$mpg)
## ```

## ```{r setup}
## #| include = FALSE
## knitr::opts_chunk$set(echo = FALSE)
## ```

# 13.4 Rå Latex og HTML ----------------------------------------

## <span style="color: red;">Denne teksten blir rød.</span>

# 13.5 Dokumenttyper ----------------------------------------

library(rforalle)
library(readxl)
hent_data("kap04_malere.xlsx")
df_malere <- read_excel("malere.xlsx")

download.file(df_malere$lenke[1], destfile = "blått_interiør.jpg")
download.file(df_malere$lenke[2], destfile = "bondebegravelse.jpg")
download.file(df_malere$lenke[3], destfile = "dødsdom.jpg")
download.file(df_malere$lenke[4], destfile = "seljefløyten.jpg")
download.file(df_malere$lenke[5], destfile = "sommernatt.jpg")

## ---
## output: pdf_document
## fontsize: 12pt
## fontfamily: dejavu
## linestretch: 1.25
## pagestyle: empty
## ---

## Kunsthandler Andersen
## Blindgata 10
## 5001 Bergen \hfill 1. januar 2025
## 
## Kunsthandler Pettersen
## Oppoverbakken 13
## 7010 Trondheim
## 
## \vspace{2cm}
## 
## **Salg av maleri**
## 
## Jeg viser til deres brev av 1. desember 2024.
## 
## Vi er åpne for å selge maleriet for den tilbudte prisen. Vedlagt er teknisk rapport og offisiell verdivurdering. Vi ser fram til å høre fra dere.
## 
## Med vennlig hilsen
## 
## \vspace{1cm}
## 
## \rule{5cm}{.3mm}
## A. Andersen
## 
## \vspace{2cm}
## 
## 2 vedlegg

## ![](mappe/signatur.png){width=250}

## ---
## documentclass: report
## output: pdf_document
## lang: no_nb
## title: \Huge Harriet Backers interiørmalerier
## author: A. Andersen
## date: Januar 2025
## toc: true
## fontfamily: tgtermes
## ---
## 
## ```{r oppsett}
## #| include=FALSE
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## # Om Harriet Backer
## 
## ```{r fyll}
## ipsum(paragraphs = 4)
## ```
## 
## # Historien bak *Blått interiør*
## 
## ```{r bilde}
## #| out.width = "60%",
## #| fig.align = "center",
## #| fig.cap = "Blått interiør, 1883"
## ipsum(paragraphs = 1)
## include_graphics("blått_interiør.jpg")
## ipsum(paragraphs = 1)
## ```

## ---
## documentclass: report
## output: pdf_document
## lang: no_nb
## title: |
##   | \pagecolor{blue} \Huge\textcolor{yellow}{Harriet Backers interiørmalerier}
## author: \textcolor{white}{A. Andersen}
## date: \textcolor{white}{Januar 2025}
## toc: true
## fontfamily: tgtermes
## header-includes:
##   - \usepackage{titlepic}
##   - \titlepic{\includegraphics[width=7cm]{blått_interiør.jpg}}
## ---
## \nopagecolor
## 

## ---
## title: Harriet Backers interiørmalerier
## author: A. Andersen
## date: Januar 2025
## output:
##   pagedreport::paged_windmill:
##     front_img: "blått_interiør.jpg"
##     toc: true
## toc-title: "Innhold"
## main-color: "darkslategray"
## ---
## 
## ```{r oppsett}
## #| include=FALSE
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## # Om Harriet Backer
## 
## ```{r fyll}
## ipsum(paragraphs = 4)
## ```
## 
## # Historien bak *Blått interiør*
## 
## ```{r bilde}
## #| out.width = "60%",
## #| fig.align = "center",
## #| fig.cap = "Blått interiør, 1883"
## ipsum(paragraphs = 1)
## include_graphics("blått_interiør.jpg")
## ipsum(paragraphs = 1)
## ```

## ---
## output: pdf_document
## lang: no_nb
## title: Fra München til Gvarv
## subtitle: Europeiske innflytelser i Erik Werenskiolds naturalisme
## author: P. Pettersen
## date: Januar 2025
## abstract: "\\noindent `r lorem::ipsum(paragraphs = 2)`"
## ---
## 
## ```{r oppsett}
## #| include=FALSE
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## ## Innledning
## 
## ```{r fyll1}
## ipsum(paragraphs = 1)
## ```
## 
## ## Analyse
## 
## ```{r fyll2}
## ipsum(paragraphs = 1)
## ```
## 
## ```{r bilde}
## #| out.width = "50%",
## #| fig.align = "center",
## #| fig.cap = "En bondebegravelse (1885)."
## include_graphics("bondebegravelse.jpg")
## ```
## 
## ## Konklusjon
## 
## ```{r fyll3}
## ipsum(paragraphs = 1)
## ```

## ---
## output:
##   pdf_document:
##     number_sections: true
## lang: no_nb
## title: Fra München til Gvarv^[Takk til Ole Olsen for nyttige kommentarer.]
## subtitle: Europeiske innflytelser i Erik Werenskiolds naturalisme
## author:
## - "Anne Andersen^[Universitetet i Bergen. Epost: a.andersen@epost.no.]"
## - "Per Pettersen^[Høgskolen i Volda. Epost: p.pettersen@epost.no.]"
## date: Januar 2025\vspace{2cm}
## abstract: "\\noindent `r lorem::ipsum(paragraphs = 4)`"
## ---
## 
## \newpage
## 
## ```{r oppsett}
## #| include=FALSE
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## # Innledning
## 
## ```{r fyll1}
## ipsum(paragraphs = 2)
## ```
## 
## # Analyse
## 
## ```{r fyll2}
## ipsum(paragraphs = 2)
## ```
## 
## ```{r bilde}
## #| out.width = "50%",
## #| fig.align = "center",
## #| fig.cap = "En bondebegravelse (1885)."
## include_graphics("bondebegravelse.jpg")
## ```
## 
## # Konklusjon
## 
## ```{r fyll3}
## ipsum(paragraphs = 2)
## ```

## ---
## output: stevetemplates::article
## fontfamily: mathpazo
## lang: no_nb
## title: Fra München til Gvarv
## subtitle: Europeiske innflytelser i Erik Werenskiolds naturalisme
## author:
## - name: Anne Andersen
##   affiliation: Universitetet i Bergen
## - name: Per Pettersen
##   affiliation: Høgskolen i Volda
## abstract: "\\noindent `r lorem::ipsum(paragraphs = 4)`"
## thanks: "Kontaktepost: a.andersen@epost.no. Siste utkast datert 1. januar 2025. Vi retter en takk til Ole Olsen for nyttige kommentarer."
## keywords: "rmarkdown, akademia"
## ---
## 
## ```{r oppsett}
## #| include=FALSE
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## # Innledning
## 
## ```{r fyll1}
## ipsum(paragraphs = 2)
## ```
## 
## # Analyse
## 
## ```{r fyll2}
## ipsum(paragraphs = 2)
## ```
## 
## ![](bondebegravelse.jpg)
## 
## # Konklusjon
## 
## ```{r fyll3}
## ipsum(paragraphs = 2)
## ```

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 1. januar 2025
## output: powerpoint_presentation
## ---
## 
## ## Biografi
## 
## - Født 1852 i Christiania
## - Utdannet i København, Karlsruhe og München
## - Kjent for portretter og naturmalerier

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 1. januar 2025
## output:
##   beamer_presentation:
##     theme: "Malmoe"
##     colortheme: "spruce"
## ---
## ## Biografi
## 
## - Født 1852 i Christiania
## - Utdannet i København, Karlsruhe og München
## - Kjent for portretter og naturmalerier

## ## Christian II undertegner dødsdommen over Torben Oxe
## 
## :::: {.columns}
## 
## ::: {.column width="50%"}
## - Malt 1875-1876
## - Skildrer historisk hendelse i 1517
## - Påvirket av München-skolen og venetiansk renessansemaleri
## :::
## 
## ::: {.column width="50%"}
## ![](dødsdom.jpg)
## :::
## 
## ::::

## :::: {.columns}
## KOLONNER
## ::::

## ::: {.column width="50%"}
## MARKDOWN
## :::

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 1. januar 2025
## output: beamer_presentation
## header-includes:
##   - \usebackgroundtemplate{\color{pink}\rule{\paperwidth}{\paperheight}}
## ---
## 
## ## Biografi
## 
## - Født 1852 i Christiania
## - Utdannet i København, Karlsruhe og München
## - Kjent for portretter og naturmalerier

hent_data("kap13_bakgrunn.png")

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 1. januar 2025
## output: beamer_presentation
## header-includes:
##   - \usebackgroundtemplate{\includegraphics[width=\paperwidth]{bakgrunn.png}}
## ---
## 
## ## Biografi
## - Født 1852 i Christiania
## - Utdannet i København, Karlsruhe og München
## - Kjent for portretter og naturmalerier

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 1. januar 2025
## output: beamer_presentation
## header-includes:
##   - \usebackgroundtemplate{\includegraphics[width=\paperwidth]{bakgrunn.png}}
##   - \setbeamercolor{title}{fg=purple}
##   - \setbeamercolor{author}{fg=teal}
##   - \setbeamercolor{frametitle}{fg=white}
## ---
## 
## ## Biografi
## - Født 1852 i Christiania
## - Utdannet i København, Karlsruhe og München
## - Kjent for portretter og naturmalerier

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 1. januar 2025
## output: beamer_presentation
## header-includes:
## - \usepackage{tikz}
## ---
## 
## ##
## \begin{tikzpicture}[remember picture, overlay]
## \node at (current page.center) {\includegraphics[width=\paperwidth, height=\paperheight]{dødsdom.jpg}};
## \end{tikzpicture}

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## output: beamer_presentation
## header-includes:
##   - \usepackage{tikz}
##   - \setbeamercolor{title}{fg=white,bg=black}
##   - \setbeamercolor{author}{fg=white}
##   - |
##     \AtBeginDocument{
##       {
##         \begin{tikzpicture}[remember picture, overlay]
##         \node at (current page.center) {\includegraphics[width=\paperwidth, height=\paperheight]{dødsdom.jpg}};
##         \end{tikzpicture}
##         \titlepage
##       }
##     }
## ---
## ## Biografi
## - Født 1852 i Christiania
## - Utdannet i København, Karlsruhe og München
## - Kjent for portretter og naturmalerier

# 13.6 Flaskehalser ----------------------------------------

## @book{danbolt1997,
## 	title = {{Norsk kunsthistorie: Bilde og skulptur frå vikingtida til i dag}},
## 	author = {Danbolt, Gunnar},
## 	publisher = {Det Norske Samlaget},
##   address = {Oslo},
## 	year = 1997,
## },

hent_data("kap13_test.bib")

## ---
## output: pdf_document
## title: BibTex-test
## bibliography: test.bib
## ---
## Norsk billedkunst har alltid vært påvirket utenfra [@danbolt1997].
## 
## ## Litteraturliste

hent_data("kap13_fullnote.csl")

## ---
## output: pdf_document
## title: BibTex-test
## bibliography: test.bib
## csl: fullnote.csl
## ---
## Norsk billedkunst har alltid vært påvirket utenfra.[^1]
## 
## [^1]: Se @danbolt1997.
## 
## ## Litteraturliste

## ---
## output: pdf_document
## title: Tabeller 1
## ---
## 
## | Navn                | Født | Død  | Kjent for                                           |
## |---------------------|------|------|-----------------------------------------------------|
## | Harriet Backer      | 1845 | 1932 | Blått interiør                                      |
## | Erik Werenskiold    | 1855 | 1938 | En bondebegravelse                                  |
## | Eilif Peterssen     | 1852 | 1928 | Christian II undertegner dødsdommen over Torben Oxe |
## | Christian Skredsvig | 1854 | 1924 | Seljefløyten                                        |
## | Kitty Kielland      | 1843 | 1914 | Sommernatt                                          |
## | Gerhard Munthe      | 1849 | 1929 | Aften i Eggedal                                     |
## | Christian Krohg     | 1852 | 1925 | Albertine i politilegens venteværelse               |
## | Frits Thaulow       | 1847 | 1906 | Vinter Vestre Aker                                  |

## ---
## output: pdf_document
## title: Tabeller 2
## fontfamily: charter
## ---
## 
## ```{r setup}
## #| include = FALSE
## library(knitr)
## library(kableExtra)
## library(dplyr)
## library(readxl)
## opts_chunk$set(echo = FALSE)
## ```
## 
## ```{r data}
## df_malere <- read_excel("malere.xlsx")
## df <- df_malere |>
##   select(!lenke)
## ```
## 
## \newpage
## 
## ```{r tab1}
## kable(df, booktabs = TRUE, linesep = "", caption = "Norske 1880-tallsmalere")
## ```
## 
## ```{r tab2}
## kable(df, booktabs = TRUE, linesep = "", caption = "Norske 1880-tallsmalere") |>
##   kable_styling(latex_options = "striped", font_size = 6)
## ```
## 
## \renewcommand{\arraystretch}{2}
## ```{r tab3}
## kable(df, booktabs = TRUE, linesep = "", caption = "Norske 1880-tallsmalere") |>
##   kable_styling(latex_options = "striped",
##                 stripe_color = "pink",
##                 font_size = 10
##                 )
## ```

## ---
## output: pdf_document
## title: Tabeller 3
## ---
## 
## ```{r setup}
## #| include = FALSE
## library(knitr)
## library(kableExtra)
## library(dplyr)
## library(readxl)
## opts_chunk$set(echo = FALSE)
## ```
## 
## ```{r data}
## df_malere <- read_excel("malere.xlsx")
## df <- df_malere |>
##   select(!lenke)
## ```
## 
## ```{r tab1}
## tabell <- flextable(df) |>
##   autofit ()
## tabell
## ```
## 
## ```{r tab2}
## tabell |>
##   theme_alafoli()
## ```
## 
## ```{r tab3}
## tabell |>
##   theme_zebra()
## ```

## ---
## output: pdf_document
## title: Seljefløyten
## lang: no_nb
## header-includes:
##    - \usepackage[singlelinecheck=false]{caption}
## ---
## ```{r}
## #| echo = FALSE,
## #| out.width = "70%",
## #| fig.cap = "Christian Skredsvig, \\textit{Seljefløyten} (1889)"
## library(knitr)
## include_graphics("seljefløyten.jpg")
## ```

## ---
## output: pdf_document
## title: Seljefløyten og Sommernatt
## header-includes:
##    - \usepackage{wrapfig}
## ---
## 
## # Bilde på høyre side
## 
## \begin{wrapfigure}{r}{0.4\textwidth}
## \includegraphics{seljefløyten.jpg}
## \caption{Christian Skredsvig, Seljefløyten (1889).}
## \end{wrapfigure}
## 
## ```{r}
## #| echo = FALSE
## library(lorem)
## ipsum(4)
## ```
## 
## # Bilde på venstre side, med mer luft
## 
## \begin{wrapfigure}{l}{0.5\textwidth}
## \centering
## \includegraphics[width=0.4\textwidth]{sommernatt.jpg}
## \caption{Kitty Kielland, Sommernatt (1886).}
## \end{wrapfigure}
## 
## ```{r}
## #| echo = FALSE
## ipsum(2)
## ```

## ---
## output: pdf_document
## title: Seljefløyten og Sommernatt
## lang: no_nb
## ---
## 
## # Bevar ulike størrelser
## ```{r}
## #| echo = FALSE,
## #| fig.show = "hold",
## #| fig.align = "center",
## #| fig.cap = "Seljefløyten (1889) og Sommernatt (1886).",
## #| out.width = "40%",
## library(knitr)
## include_graphics(c("seljefløyten.jpg", "sommernatt.jpg"))
## ```
## 
## # Jevn ut høydeforskjellen
## ```{r}
## #| echo = FALSE,
## #| fig.show = "hold",
## #| fig.align = "center",
## #| fig.cap = "Seljefløyten (1889) og Sommernatt (1886).",
## #| out.width = c("50%", "44%")
## include_graphics(c("seljefløyten.jpg", "sommernatt.jpg"))
## ```

## ---
## output: pdf_document
## title: Norske 1880-tallsmalere
## lang: no_nb
## ---
## Se figur \ref{fig:selje} og tabell \ref{tab:malere}.
## 
## ```{r oppsett}
## #| include = FALSE
## library(knitr)
## library(readxl)
## library(dplyr)
## df_malere <- read_excel("malere.xlsx")
## df <- df_malere |>
##   select(!lenke)
## ```
## 
## ```{r bilde}
## #| echo = FALSE,
## #| out.width = "50%",
## #| fig.align = "center",
## #| fig.cap = "\\label{fig:selje} Christian Skredsvig, Seljefløyten (1889)."
## include_graphics("seljefløyten.jpg")
## ```
## 
## ```{r tabell}
## #| echo = FALSE
## kable(df, caption = "\\label{tab:malere} Norske 1880-tallsmalere.")
## ```

yaml <- c("---", "output: pdf_document", "---")
markdown <- "# Test\n\nSlik lager vi en .Rmd med R-kode."
rmd <- c(yaml, markdown)
writeLines(rmd, "test.Rmd")

## library(rmarkdown)
## render("test.Rmd")

## library(readxl)
## library(rmarkdown)
## 
## df_malere <- read_excel("malere.xlsx")
## 
## yaml <- c("---", "output: pdf_document", "---")
## 
## for (i in df_malere$navn) {
##   overskrift <- paste("# ", i, "\n\n")
##   tekst <- as.character(lorem::ipsum(6))
##   rmd <- c(yaml, overskrift, tekst)
##   navn <- gsub(" ", "_", tolower(i))
##   filnavn <- paste0(navn, ".Rmd")
##   writeLines(rmd, filnavn)
##   render(filnavn)
## }

# Opprensking
kan_slettes <- list.files(
  pattern = "png$|pdf$|jpg$|html$|xlsx$"
  )
file.remove(c(kan_slettes, "test.Rmd", "komponister.Rmd", "test.bib", "fullnote.csl"))
