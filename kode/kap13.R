#-----------------------------------
# Kode til kapittel 13 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("quarto", "readxl", "knitr", "systemfonts", "lorem", "dplyr", "tinytex", "devtools"))
devtools::install_github("hegghammer/rforalle")

# [Følgende kommando må også kjøres for at du skal kunne generere PDF-filer.]
tinytex::install_tinytex()

# [NB: Mye av materialet i dette skriptet er ikke R-kode, men Quarto-kode. Den er ment å settes inn i .qmd-dokumenter og er derfor kommentert ut med doble nummertegn.]

# 13.2 Quarto ----------------------------------------

quarto_render("test.qmd")

# 13.4 Eksempler ----------------------------------------

## ---
## format:
##   pdf:
##     fontsize: 12pt
##     mainfont: DejaVu Serif
##     linestretch: 1.25
##     pagestyle: empty
## ---

fonts <- systemfonts::system_fonts()
fonts$family

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

## ---
## format:
##   typst:
##     mainfont: DejaVu Serif
##     fontsize: 12pt
## ---
## 
## ```{=typst}
## #set par(leading: 1.25em)
## #set page(numbering: none)
## 
## Kunsthandler Andersen \
## Blindgata 10 \
## 5001 Bergen #h(1fr) 23. februar 2024 \
## Norway \
## \
## Kunsthandler Pettersen \
## Oppoverbakken 13 \
## 7010 Trondheim \
## #v(2cm)
## ```
## 
## **Salg av maleri**
## 
## Jeg viser til deres brev av 1. desember 2024.
## 
## Vi er åpne for å selge maleriet for den tilbudte prisen. Vedlagt er
## teknisk rapport og offisiell verdivurdering. Vi ser fram til å høre
## fra dere.
## 
## Med vennlig hilsen
## 
## ```{=typst}
## #v(1.5cm)
## #line(length: 5cm, stroke: .5pt)
## A. Andersen
## #v(1.5cm)
## ```
## 2 vedlegg

library(rforalle)
library(readxl)
hent_data("kap04_malere.xlsx")
df_malere <- read_excel("malere.xlsx")

download.file(df_malere$lenke[1], destfile = "blått_interiør.jpg", mode = "wb")
download.file(df_malere$lenke[2], destfile = "bondebegravelse.jpg", mode = "wb")
download.file(df_malere$lenke[3], destfile = "dødsdom.jpg", mode = "wb")
download.file(df_malere$lenke[4], destfile = "seljefløyten.jpg", mode = "wb")
download.file(df_malere$lenke[5], destfile = "sommernatt.jpg", mode = "wb")

## ---
## title: \Huge Harriet Backers interiørmalerier
## author: A. Andersen
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format:
##   pdf:
##     documentclass: report
##     toc: true
## ---

## # Om Harriet Backer
## 
## ```{r}
## #| label: tekst1
## #| echo: false
## lorem::ipsum(paragraphs = 4)
## ```
## 
## # Historien bak *Blått interiør*
## 
## ```{r}
## #| label: tekst2
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```
## 
## ![Blått interiør, 1883.](blått_interiør.jpg){width=60%}

## ---
## title: \pagecolor{blue} \Huge \textcolor{yellow}{Harriet Backers interiørmalerier}
## author: \textcolor{white}{A. Andersen}
## lang: nb
## format:
##   pdf:
##     documentclass: report
##     toc: true
##     mainfont: DejaVu Serif
##     include-in-header:
##       text: |
##         \usepackage{titlepic}
##         \titlepic{\includegraphics[width=7cm]{blått_interiør.jpg}}
## ---
## \nopagecolor

## ---
## title: Fra München til Gvarv
## subtitle: Europeiske innflytelser i Erik Werenskiolds naturalisme
## author: Per Pettersen (Høgskolen i Volda)
## lang: nb
## format: pdf
## abstract: "`r lorem::ipsum(paragraphs = 2)`"
## ---
## 
## ## Innledning
## 
## ```{r}
## #| label: tekst1
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```
## 
## ## Analyse
## 
## ```{r}
## #| label: tekst2
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```

## ---
## title: Fra München til Gvarv^[Takk til Ole Olsen for nyttige kommentarer.]
## subtitle: Europeiske innflytelser i Erik Werenskiolds naturalisme
## author:
## - "Anne Andersen^[Universitetet i Bergen. Epost: a.andersen@epost.no.]"
## - "Per Pettersen^[Høgskolen i Volda. Epost: p.pettersen@epost.no.]"
## lang: nb
## format:
##   pdf:
##     sansfont: Latin Modern Roman
## abstract: "\\noindent\\textbf{Sammendrag:} `r lorem::ipsum(paragraphs = 3)` \\newline \\newline \\textbf{Antall ord: 345}"
## ---
## 
## \newpage
## 
## ## Innledning
## 
## ```{r}
## #| label: tekst1
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```
## 
## ## Analyse
## 
## ```{r}
## #| label: tekst2
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```
## 
## ![En bondebegravelse (1885).](bondebegravelse.jpg){width=50%}
## 
## # Konklusjon
## 
## ```{r}
## #| label: tekst3
## #| echo: false
## lorem::ipsum(paragraphs = 2)
## ```

## ---
## title: "Fra München til Gvarv: Europeiske innflytelser i Erik Werenskiolds naturalisme"
## author: "**Anne Andersen**\n\nUniversitetet i Bergen\n\na.andersen@epost.no"
## format:
##   typst:
##     columns: 2
## ---
## 
## ## Innledning
## 
## ```{r}
## #| label: tekst1
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```
## 
## ## Analyse
## 
## ```{r}
## #| label: tekst2
## #| echo: false
## lorem::ipsum(paragraphs = 7)
## ```

# [NB: Følgende to kommandoer er ment å kjøres i terminalen]
quarto install extension cmarquardt/quarto-simple-article

quarto use template cmarquardt/quarto-simple-article

## ---
## title: Fra München til Gvarv
## subtitle: Europeiske innflytelser i Erik Werenskiolds naturalisme
## author:
##   - name: Anne Andersen
##     affiliation: Universitetet i Bergen
##     email: a.andersen@epost.no
##   - name: Per Pettersen
##     affiliation: Høgskolen i Volda
##     email: p.pettersen@epost.no
## crossref:
##   fig-title: Figur
## format:
##   simple-article-pdf:
##     pdf-engine: pdflatex
##     indent: false
##     classoption:
##      - twocolumn
## abstract: "`r lorem::ipsum(paragraphs = 3)`"
## ---
## 
## # Innledning
## 
## ```{r}
## #| label: tekst1
## #| echo: false
## lorem::ipsum(paragraphs = 1)
## ```
## 
## # Analyse
## 
## ```{r}
## #| label: tekst2
## #| echo: false
## lorem::ipsum(paragraphs = 4)
## ```
## 
## ![En bondebegravelse (1885).](bondebegravelse.jpg){width=50%}
## 
## # Konklusjon
## 
## ```{r}
## #| label: tekst3
## #| echo: false
## lorem::ipsum(paragraphs = 2)
## ```

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format: pptx
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
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format:
##   beamer:
##     theme: Malmoe
##     colortheme: spruce
## ---
## 
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

## ---
## title: Eilif Peterssens billedkunst
## author: Ole Olsen
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format:
##   beamer:
##     include-in-header:
##       text: |
##         \usebackgroundtemplate{\color{pink}\rule{\paperwidth}{\paperheight}}
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
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format:
##   beamer:
##     include-in-header:
##       text: |
##         \usebackgroundtemplate{\includegraphics[width=\paperwidth]{bakgrunn.png}}
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
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format:
##   beamer:
##     include-in-header:
##       text: |
##         \usebackgroundtemplate{\includegraphics[width=\paperwidth]{bakgrunn.png}}
##         \setbeamercolor{title}{fg=purple}
##         \setbeamercolor{author}{fg=teal}
##         \setbeamercolor{frametitle}{fg=white}
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
## date: 2025-01-01
## date-format: "D. MMMM YYYY"
## lang: nb
## format:
##   beamer:
##     include-in-header:
##       text: |
##         \usepackage{tikz}
## ---
## 
## ##
## \begin{tikzpicture}[remember picture, overlay]
## \node at (current page.center) {\includegraphics[width=\paperwidth, height=\paperheight]{dødsdom.jpg}};
## \end{tikzpicture}

# 13.5 Flaskehalser ----------------------------------------

## @book{danbolt1997,
##   title = {{Norsk kunsthistorie: Bilde og skulptur frå vikingtida til i dag}},
##   author = {Danbolt, Gunnar},
##   publisher = {Det Norske Samlaget},
##   address = {Oslo},
##   year = 1997,
## },

hent_data("kap13_test.bib")

## ---
## title: BibTex-test
## format: pdf
## bibliography: test.bib
## ---
## Norsk billedkunst har alltid vært påvirket utenfra [@danbolt1997].
## 
## ## Litteraturliste

hent_data("kap13_fullnote.csl")

## ---
## title: BibTex-test
## format: pdf
## bibliography: test.bib
## csl: fullnote.csl
## ---
## Norsk billedkunst har alltid vært påvirket utenfra.[^1]
## 
## [^1]: Se @danbolt1997.
## 
## ## Litteraturliste

## ---
## format: pdf
## ---
## 
## | **Navn**         | **Født** | **Død** |
## |------------------|----------|---------|
## | Harriet Backer   | 1845     | 1932    |
## | Erik Werenskiold | 1855     | 1938    |
## | Eilif Peterssen  | 1852     | 1928    |

## ---
## format: pdf
## ---
## 
## ```{r}
## #| label: tabell
## #| echo: false
## #| message: false
## library(readxl)
## library(dplyr)
## library(knitr)
## df <- read_excel("malere.xlsx") |> select(!lenke)
## kable(df)
## ```

readxl::read_excel("malere.xlsx") |> 
  dplyr::select(!lenke) |>  
  knitr::kable()

## library(readxl)
## library(dplyr)
## library(knitr)
## read_excel("malere.xlsx") |>
##   select(!lenke) |>
##   kable()

## format:
##   pdf:
##     pdf-engine: pdflatex

## ---
## format: pdf
## ---
## 
## ```{r}
## #| label: test_asis
## #| echo: false
## #| results: asis
## knitr::kable(mtcars)
## ```

## ---
## title: Seljefløyten
## format: pdf
## lang: nb
## ---
## 
## ![Christian Skredsvig, *Seljefløyten* (1889)](seljefløyten.jpg){width=70%}

## ---
## title: Seljefløyten
## lang: nb
## fig-align: left
## fig-cap-location: top
## format:
##   pdf:
##     include-before-body:
##       text: |
##         \captionsetup{justification=raggedright,singlelinecheck=false}
## ---
## 
## ![Christian Skredsvig, *Seljefløyten* (1889)](seljefløyten.jpg){width=70%}

## ---
## title: Seljefløyten og Sommernatt
## format: pdf
## ---
## ::: {layout-ncol=2}
## ![Christian Skredsvig, *Seljefløyten* (1889)](seljefløyten.jpg){height=180}
## 
## ![Kitty Kielland, *Sommernatt* (1886)](sommernatt.jpg){height=180}
## :::

## ---
## title: Seljefløyten og Sommernatt
## format: pdf
## ---
## ::: {layout="[10,-5,9]"}
## ![Christian Skredsvig, *Seljefløyten* (1889)](seljefløyten.jpg){height=180}
## 
## ![Kitty Kielland, *Sommernatt* (1886)](sommernatt.jpg){height=180}
## :::

## ---
## title: Seljefløyten og Sommernatt
## lang: nb
## format:
##   pdf:
##     include-in-header:
##       text: \usepackage{wrapfig}
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
## #| label: tekst1
## #| echo: false
## lorem::ipsum(2)
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
## #| label: tekst1
## #| echo: false
## lorem::ipsum(2)
## ```

## ---
## title: Kodegenererte figurer
## lang: nb
## format: pdf
## ---
## 
## ```{r}
## #| label: graf1
## #| echo: false
## #| fig-width: 7
## #| fig-height: 3
## #| fig-cap: Miles per gallon
## plot(mtcars$mpg)
## ```
## 
## ```{r}
## #| label: graf2
## #| echo: false
## #| fig-width: 2
## #| fig-height: 4
## #| fig-cap: Miles per gallon
## plot(mtcars$mpg)
## ```

## ---
## title: Bilde med kryssreferanse.
## lang: nb
## format: pdf
## ---
## 
## Se @fig-selje.
## 
## ![Christian Skredsvig, *Seljefløyten* (1889)](seljefløyten.jpg){width=70% #fig-selje}

## ---
## title: Kodegenerert figur med kryssreferanse.
## lang: nb
## format: pdf
## ---
## 
## Se @fig-graf.
## 
## ```{r}
## #| label: fig-graf
## #| echo: false
## #| fig-cap: Miles per gallon.
## plot(mtcars$mpg)
## ```

## ---
## title: Markdown-tabell med kryssreferanse.
## lang: nb
## format: pdf
## ---
## 
## Se @tbl-malere.
## 
## | Navn             | Født |
## |------------------|------|
## | Harriet Backer   | 1845 |
## | Erik Werenskiold | 1855 |
## 
## : Norske malere. {#tbl-malere}

## ---
## title: Kodegenerert tabell med kryssreferanse
## lang: nb
## format: pdf
## ---
## 
## Se @tbl-malere.
## 
## ```{r}
## #| echo: false
## #| label: tbl-malere
## #| tbl-cap: Norske malere.
## df <- data.frame(Navn = c("Harriet Backer", "Erik Werenskiold"),
##                  Født = c(1845, 1855)
##                  )
## knitr::kable(df)
## ```

# 13.6 Automatisering ----------------------------------------

yaml <- c("---", "format: pdf", "---")
markdown <- "# Test\n\nSlik lager vi en .qmd med R-kode."
rmd <- c(yaml, markdown)
writeLines(rmd, "test.qmd")

library(quarto)
quarto_render("test.qmd")

library(readxl)
df_malere <- read_excel("malere.xlsx")

yaml <- c("---", "format: pdf", "---")

for (i in seq_along(df_malere$navn)) {
  overskrift <- paste("# ", df_malere$navn[i], "\n\n")
  tekst <- as.character(lorem::ipsum(6))
  qmd <- c(yaml, overskrift, tekst)
  navn <- gsub(" ", "_", tolower(df_malere$navn[i]))
  filnavn <- paste0(i, "_", navn, ".qmd")
  writeLines(qmd, filnavn)
  quarto_render(filnavn)
}

# Opprensking
kan_slettes <- list.files(
  pattern = "png$|pdf$|jpg$|html$"
  )
file.remove(c(kan_slettes, "test.qmd", "test.bib"))
