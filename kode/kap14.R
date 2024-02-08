#-----------------------------------
# Kode til kapittel 14 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("markdown", "knitr", "lorem", "usethis", "ggplot2", "gganimate", "rnaturalearth", "ggplot2", "data.table", "plotly", "leaflet", "rgl", "readobj", "devtools"))
devtools::install_github("hegghammer/rforalle", "MilesMcBain/gistfo")

# [NB: Mye av materialet i dette skriptet er ikke R-kode, men RMarkdown eller Quarto-kode. Dette er ment å settes inn i .Rmd- og .qmd-dokumenter og er derfor kommentert ut med doble nummertegn.]

# 14.1 Enkeltdokumenter ----------------------------------------

library(rforalle)
hent_data("kap14_komponister.Rmd")

hent_data("kap14_bull.jpg")
hent_data("kap14_grondahl.jpg")

library(markdown)
# [NB: Følgende kommando fordrer at du er registrert på https://rpubs.com og at du har strikket .Rmd-filen til HTML. Kjør eventuelt `rmarkdown::render("komponister.Rmd")`.]

rpubsUpload(title = "Komponister", htmlFile = "komponister.html")

dir.create("netlify_drop")
file.copy("komponister.html", "netlify_drop/index.html")
file.copy(c("bull.jpg", "grondahl.jpg"), "netlify_drop")  

# 14.2 Nettsteder ----------------------------------------

## ---
## title: "Velkommen"
## ---
## 
## Dette er en nettside om tidlige norske komponister.

## ---
## title: "Ole Bull"
## ---
## 
## ```{r setup, include=FALSE}
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## ```{r}
## ipsum(2)
## ```
## 
## ```{r}
## #| echo: false
## #| fig.cap: Ole Bull ca. 1875.
## #| fig.align: left
## #| out.width: 150
## #| out.extra: style="padding:10px"
## include_graphics("bull.jpg")
## ```
## 
## ```{r}
## ipsum(2)
## ```

## ---
## title: "Agathe Grøndahl"
## ---
## 
## ```{r setup, include=FALSE}
## library(knitr)
## library(lorem)
## opts_chunk$set(echo = FALSE)
## ```
## 
## ```{r}
## ipsum(2)
## ```
## 
## ```{r}
## #| echo: false
## #| fig.cap: Agathe Grøndahl ca. 1865.
## #| fig.align: left
## #| out.width: 150
## #| out.extra: style="padding:10px"
## include_graphics("grondahl.jpg")
## ```
## 
## ```{r}
## ipsum(2)
## ```

## project:
##   type: website
## 
## website:
##   title: Komponister
##   navbar:
##     left:
##       - href: bull.qmd
##         text: Ole Bull
##       - href: grøndahl.qmd
##         text: Agathe Grøndahl
## 
## format:
##   html:
##     theme: cosmo

## format:
##   html:
##     theme: slate
##     backgroundcolor: pink
##     fontcolor: purple

## /*-- scss:defaults --*/
## $navbar-bg: orange;

## format:
##   html:
##     theme:
##       - slate
##       - custom.scss
##     backgroundcolor: pink
##     fontcolor: purple

# [NB: Følgende kommando er ment å kjøres i terminalen. Den fordrer også registrering på https://quartopub.com/.]
quarto publish quarto-pub

# 14.3 Github ----------------------------------------

# =============================================================================
# [NB: Linjene herfra og til neste linje med likhetstegn (===) er ment å kjøres i terminalen.]

git config --global user.name "Mitt Navn"
git config --global user.email mittnavn@epost.no

mkdir test
cd test
git init
echo "Hei hei" > fil.txt
git add .
git commit -m "min første commit"

echo ekstralinje >> fil.txt
git add .
git commit -m "min andre commit"

git log --oneline

git reset --hard <XXXXXX>

# Windows:
rmdir .git /s

# Mac/Linux:
rm -rf .git

#=============================================================================

library(usethis)
use_git()

usethis::edit_r_environ()

# [NB: Følgende kommando fordrer at du har konto på https://github.com.]
use_github(private = TRUE)

# 14.4 Animasjon og interaktivitet ----------------------------------------

library(ggplot2)

hent_data("kap05_befolkning.csv")
df <- read.csv("befolkning.csv")

graf <-  ggplot(df) +
	geom_line(aes(år, levendefødte_i_alt), colour = "green4") +
	geom_line(aes(år, døde_i_alt), colour = "red") +
	theme_classic() +
	labs(title = "Fødte og døde i Norge, 1735-2020",
			 subtitle = "(Fødte i grønt)",
			 x = "", y = "Personer")
graf

# [NB: Plott og figurer laget med gganimate vil ikke vises i IDEer andre enn RStudio.  Hvis du bruker VSCode, Radian eller andre programmer må du lagre animasjonen til fil og spille av filen fra filutforskeren.]

library(gganimate)
graf +
  transition_reveal(år)

anim <- ggplot(df) +
	geom_point(aes(år, levendefødte_i_alt), colour = "green4", size = 5) +
	geom_point(aes(år, døde_i_alt), colour = "red", size = 5) +
	theme_classic() +
	labs(title = "Fødte og døde i Norge, 1735-2020",
			 subtitle = "(Fødte i grønt)",
			 x = "", y = "Personer") +
	transition_states(år) +
	shadow_mark(size = 1, colour = "grey")
anim

animate(anim, fps=4)

anim_save("anim.gif")

animate(anim, renderer = av_renderer("anim.mp4"))

library(rnaturalearth)
library(ggplot2)

vektor_island <- ne_countries(country = "iceland",
                              scale = "medium",
                              returnclass = "sf")

kart_island <- ggplot(vektor_island) +
  geom_sf(size = .2) +
  theme_void()

library(rforalle)
library(data.table)
hent_data("kap07_skjelv.txt")
df_skjelv <- read.table("skjelv.txt", header = TRUE)

anim2 <- kart_isl +
  geom_point(data = df_skjelv,
  aes(x = Lengd, y = Breidd, size = ML), color = "red") +
  scale_radius(range = c(0,10)) +
  labs(size = "Size on\nRichter's\nScale") +
  transition_states(Nr) +
	shadow_mark(size = 4, colour = "darkgrey", alpha = .5)

animate(anim2, renderer = av_renderer("anim3.mp4"), fps = 4)

library(plotly)
df_bef <- read.csv("befolkning.csv")

graf <- ggplot(df_bef) +
	geom_line(aes(år, levendefødte_i_alt), colour = "green4") +
	geom_line(aes(år, døde_i_alt), colour = "red") +
	theme_classic() +
	labs(title = "Fødte og døde i Norge, 1735-2020",
			 subtitle = "(Fødte i grønt)",
			 x = "", y = "Personer")

ggplotly(graf)

plot_ly(df, x = ~ år, y = ~ inngåtte_ekteskap,
       type = "scatter",
       mode = "lines",
       color = I("orange")
       ) %>%
  rangeslider(start = 1735, end = 2020) %>%
  layout(
    title = "\nEkteskap, skilsmisser og barn født utenfor ekteskap i Norge 1735-2020\n",
    xaxis = list(title = ""),
    yaxis = list (title = "Antall"),
    updatemenus = list(
       list(
         y = 0.8,
         buttons = list(

           list(method = "restyle",
                args = list("y", list(df$inngåtte_ekteskap)),
                label = "Inngåtte ekteskap"),

           list(method = "restyle",
                args = list("y", list(df$skilsmisser)),
                label = "Skilsmisser"),

           list(method = "restyle",
                args = list("y", list(df$fødte_utenfor_ekteskap)),
                label = "Fødte utenfor ekteskap")
           )
         )
       )
    )

library(leaflet)
kart <- leaflet() %>%
  addTiles() %>%
  setView(lng = -18.1, lat =  65.685, zoom = 12)
kart

leaflet() %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  setView(lng = -18.1, lat =  65.685, zoom = 12)

kart  %>%
  addMarkers(lng = -18.123, lat =  65.684,
             label = "Universitetet i Akureyri",
             labelOptions = labelOptions(noHide = TRUE,
                                         textsize = "15px",
                                         direction = "left")) %>%
  addCircles(lng = -18.086, lat = 65.683, radius = 400,
             popup = "Havna <img src='https://upload.wikimedia.org/wikipedia/commons/c/c1/Akureyri_-_Skapti_Hallgr%C3%ADmsson.jpg'>",
             popupOptions = popupOptions(minWidth = 150,
                                         direction = "bottom")
             ) %>%
  addRectangles(lng1 = -18.08, lat1 = 65.66,
                lng2 = -18.06, lat2 = 65.64,
                color = "red",
                label = "Flyplassen",
                labelOptions = labelOptions(noHide = TRUE,
                                         textsize = "15px",
                                         direction = "right"))

df_skjelv <- read.table("skjelv.txt", header = TRUE)

leaflet(data = df_skjelv) %>%
    addTiles() %>%
    addMarkers(~ Lengd, ~ Breidd, popup = ~ Dags., label = ~ ML)

leaflet(data = df_skjelv) %>%
    addTiles() %>%
    addMarkers(~ Lengd, ~ Breidd, popup = ~ Dags., label = ~ ML,
               clusterOptions = markerClusterOptions(freezeAtZoom = 6))

library(rgl)
x <- sample(100, 100)
y <- sample(100, 100)
z <- sample(100, 100)
plot3d(x, y, z, type = "s", col = "gold3")

## ---
## output: html_document
## ---
## 
## ```{r include=FALSE}
## rgl::setupKnitr(autoprint = TRUE)
## ```
## 
## ```{r}
## #| echo: false
## library(rgl)
## x <- sample(100, 100)
## y <- sample(100, 100)
## z <- sample(100, 100)
## plot3d(x, y, z, type = "s", col = "gold3")
## ```

plot3d(x, y, z, type = "s", col = "gold3")
play3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 10)

movie3d(spin3d(axis = c(0, 0, 1), rpm = 10),
				duration = 6,
				dir = getwd(),
				movie = "3d_animasjon",
				fps = 10)

system("ffmpeg -i 3d_animasjon.gif -loop 0 3d_animasjon_loop.gif")

hent_data("kap14_nefertiti.obj")
hent_data("kap14_texture.png")

library(readobj)
mesh <- read.obj("nefertiti.obj", convert.rgl = TRUE)

shade3d(mesh, col = "white", texture = "texture.png")

shade3d(mesh, col = "orange2")

shade3d(mesh, col = "orange2", shininess = 10)
bg3d(col = "black")

view3d(theta = -50, phi = 10, zoom = 1)

# Opprensking
kan_slettes <- list.files(
  pattern = "png$|obj$|jpg$|mp4$|csv$"
  )
file.remove(kan_slettes)

unlink("netlify_drop", recursive = TRUE)