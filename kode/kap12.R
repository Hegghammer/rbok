#-----------------------------------
# Kode til kapittel 12 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("av", "tidyverse", "magick", "tiff", "jpeg", "imager", "knitr", "colorfindr", "googleCloudVisionR", "tuneR", "seewave", "glue", "magick", "devtools"))
devtools::install_github("hegghammer/rforalle")
devtools::install_github("bnosac/audio.whisper")
devtools::install_github("paleolimbot/exifr")
install.packages("image.darknet", repos =
"https://bnosac.github.io/drat")

# 12.2 Utskilling og konvertering ----------------------------------------

library(rforalle)
hent_data("kap12_filmavis.ogv")

videofil <- "filmavis.ogv"
filinfo <- file.info(videofil)
filinfo$size / 1000000

library(av)
dir.create("filmavis_bilder")
av_video_images(videofil, destdir = "filmavis_bilder", fps = 0.2)

library(tidyverse)
library(magick)
bildefiler <- list.files("filmavis_bilder", full.names = TRUE)
image_read(bildefiler) |>
  image_montage(tile = "6", geometry = "x150+5+5") 

av_audio_convert(videofil, "filmavis_lyd.wav")

av_video_convert(videofil, "filmavis.mp4")

av_audio_convert("filmavis_lyd.wav", "filmavis_lyd.mp3")

library(tiff)
library(jpeg)
img <- readJPEG(bildefiler[1])
writeTIFF(img, "test.tiff")

img <- image_read(bildefiler[1])
image_write(img, "test.gif", format = "gif")

# 12.3 Bilder ----------------------------------------

image_read(bildefiler[19])

img <- image_read(bildefiler[19])
info <- image_info(img)

library(exifr)
exifdata <- read_exif(bildefiler[1])

library(imager)
bildefiler <- list.files("filmavis_bilder", full.names = TRUE)
df_img <- load.image(bildefiler[19]) |> 
  as.data.frame()

img <- image_read(bildefiler[19])
img_utsnitt <- image_crop(img, geometry = "150x100+250+100")
img_utsnitt

image_write(img_utsnitt, "utsnitt.jpg")

image_modulate(img, brightness = 150)

image_negate(img)

image_negate(img) |> 
  image_border("black", "1x1")

image_colorize(img, opacity = 30, color = "red")

image_convert(img, type = "Grayscale")

image_convert(img, type = "Bilevel")

image_oilpaint(img, 2)

image_blur(img, 3, 10)

img <- image_read(bildefiler[19])
image_annotate(img, 
               "R FOR ALLE", 
               location = "+80+250", 
               size = 30,
               degrees = -35,
               weight = 1000,  # skrifttykkelse
               style = "italic", 
               strokecolor = "red",           
               color = "gold")

gg <- image_ggplot(img)

gg + 
  geom_hline(yintercept = 100, color = "red") +
  geom_vline(xintercept = 250, color = "red") +
  annotate(
           "rect",
           xmin = 250, 
           xmax = 400, 
           ymin = 100, 
           ymax = 200,
           alpha = .4,
           fill = "gold"
           ) 

ggsave("gg_redigert.jpg")
knitr::plot_crop("gg_redigert.jpg")

bilde2 <- image_read(bildefiler[1]) |> 
  image_scale("x75") |> 
  image_border("red", "2x2")
image_composite(img, bilde2, offset = "+375+200")

hent_data("kap12_plakat.jpg")

image_read("plakat.jpg") |> plot()

library(colorfindr)
df_farger <- get_colors("plakat.jpg", min_share = 0.001)

plot_colors(df_farger)

make_palette(df_farger, n = 3)

liste_alle <- map(bildefiler, ~ get_colors(.x, min_share = 0.001))
df_alle <- bind_rows(liste_alle)

df_konsolidert <- df_alle |> 
  group_by(col_hex) |> 
  summarise(col_freq = sum(col_freq)) |> 
  mutate(col_share = col_freq/sum(col_freq))

plot_colors(df_konsolidert)

make_palette(df_konsolidert, n = 4)

A <- image_read(bildefiler[16])
B <- image_read(bildefiler[18])
C <- image_read(bildefiler[19])

image_compare(B, A, metric = "MAE") |> 
  attributes() |> 
  pluck("distortion")

image_compare(C, A, metric = "MAE") |> 
  attributes() |> 
  pluck("distortion")

library(image.darknet)
mclass <- image_darknet_model(
  type = "classify",
  model = "tiny.cfg",
  weights = system.file(package = "image.darknet", 
                                  "models", 
                                  "tiny.weights"
                                  ),
  labels = system.file(package = "image.darknet", 
                                 "include", 
                                 "darknet", 
                                 "data", 
                                 "imagenet.shortnames.list"
                                 )
  )

sti_plakat <- file.path(getwd(), "plakat.jpg")

image_darknet_classify(file = sti_plakat, object = mclass)

sti_onsager <- file.path(getwd(), bildefiler[19])
image_darknet_classify(file = sti_onsager, object = mclass)

sti_onsager <- file.path(getwd(), bildefiler[19])

mdetect <- image_darknet_model(
  type = "detect",
  model = "tiny-yolo-voc.cfg",
  weights = system.file(package = "image.darknet", 
                                  "models", 
                                  "tiny-yolo-voc.weights"
                                  ),
  labels = system.file(package = "image.darknet", 
                                 "include", 
                                 "darknet", 
                                 "data", 
                                 "voc.names"
                                 )
  )

image_darknet_detect(file = sti_onsager, object = mdetect)

# [NB: Følgende kode fordrer konto i Google Cloud Services og oppsett for autentisering (se boken).]
library(googleCloudVisionR)
prediksjoner <- gcv_get_image_annotations(
    imagePaths = sti_plakat,
    feature = "LABEL_DETECTION",
    maxNumResults = 7
)
prediksjoner$description

prediksjoner2 <- gcv_get_image_annotations(
    imagePaths = sti_onsager,
    feature = "FACE_DETECTION",
    maxNumResults = 7
)

x1 <- strsplit(prediksjoner2$x[1], ", ") |> 
  unlist() |> 
  as.numeric()

x2 <- strsplit(prediksjoner2$x[2], ", ") |> 
  unlist() |> 
  as.numeric()
  
y1 <- strsplit(prediksjoner2$y[1], ", ") |> 
  unlist() |> 
  as.numeric()

y1 <- strsplit(prediksjoner2$y[2], ", ") |> 
  unlist() |> 
  as.numeric()

img <- image_read(bildefiler[19])
gg <- image_ggplot(img)
gg + 
  annotate(
           "rect",
           xmin = min(x1), 
           xmax = max(x1), 
           ymin = min(y1), 
           ymax = max(y1),
           color = "red",
           alpha = 0,
           linewidth = 1.5
           ) +
  annotate(
           "rect",
           xmin = min(x2), 
           xmax = max(x2), 
           ymin = min(y2), 
           ymax = max(y2),
           color = "red",
           alpha = 0,
           linewidth = 1.5
           )

# Mac/Linux:
system("echo Hello world!")

# Windows:
shell("echo Hello world!")

# Mac/Linux:
message <- system("echo Hello world!", intern = TRUE)
message

# Windows:
message <- shell("echo Hello world!", intern = TRUE)
message

# [NB: Følgende kommando fordrer programmet Rclip (se boken).]
# Mac/Linux:
flaggbilder <- system('cd filmavis_bilder && rclip "flag"')
# Windows:
flaggbilder <- shell('cd filmavis_bilder && rclip "flag"')

library(purrr)
relevante_elementer <- flaggbilder[3:12]
treffstier <- map_chr(relevante_elementer,
                      ~ str_sub(string = .x, start = 8, end = -2))

utils::browseURL(treffstier[1])

# 12.4 Lyd ----------------------------------------

library(av)
lydfil <- "filmavis_lyd.mp3"
info_lyd <- av_media_info(lydfil)

file.info(lydfil)$size / 1000000 # for megabytes

av_audio_convert(lydfil, 
                 output = "filmavis_lyd_20.mp3", 
                 start_time = 0, 
                 total_time = 20
                 )

dir.create("filmavis_lyd")
starttider <- seq(0, info_lyd$duration, 60)
for (i in seq_along(starttider)) {
  av_audio_convert(lydfil, 
                   output = paste0("filmavis_lyd/del_", i, ".mp3"),
                   start_time = starttider[i],
                   total_time = 60
                  )
}

lydfiler <- list.files("filmavis_lyd", full.names = TRUE)

library(tuneR)
deler_wav <- map(lydfiler, readMP3)

samlet <- do.call(bind, deler_wav)
writeWave(samlet, "filmavis_samlet.wav")

ffmpeg -i INPUT.mp3 -af "LYDFILTER" output.mp3

# Mac/Linux:
system('ffmpeg -i filmavis_lyd.mp3 -af "lowpass=f=1200"
       filmavis_filtrert.mp3')

# Windows:
shell('ffmpeg -i filmavis_lyd.mp3 -af "lowpass=f=1200"
       filmavis_filtrert.mp3')

# Mac/Linux:
system('ffmpeg -i filmavis_lyd.mp3 -af "lowpass=f=1000, volume=5"
       filmavis_filtrert_høy.mp3')

# Windows:
shell('ffmpeg -i filmavis_lyd.mp3 -af "lowpass=f=1000, volume=5"
       filmavis_filtrert_høy.mp3')

library(audio.whisper)
# [NB: Følgende kommando laster ned en modell på 465MB.]
modell <- whisper("small")

av_audio_convert("filmavis_lyd.wav",
                 output = "filmavis_lyd16.wav",
                 sample_rate = 16000
                 )

# [NB: Følgende kommando kan ta flere minutter å prosessere.]
transkripsjon <- predict(modell,
                         "filmavis_lyd16.wav",
                         language = "no"
                         )

transkripsjon <- readRDS("data/whisper_transkripsjon.rds")

head(transkripsjon$data)

library(seewave)
lyd_wav <- tuneR::readMP3("filmavis_lyd_20.mp3")
spectro(lyd_wav, flim = c(0, 4))

# 12.5 Video ----------------------------------------

videofil <- "filmavis.ogv"
vidinfo <- av_media_info(videofil)

vidinfo$video$framerate * vidinfo$duration

exif_film <- read_exif("filmavis.ogv")

# Mac/Linux:
system("ffmpeg -i filmavis.mp4 -ss 00:01:30
       -vframes 1 skjermbilde.png")

# Windows:
shell("ffmpeg -i filmavis.mp4 -ss 00:01:30
       -vframes 1 skjermbilde.png")

# Mac/Linux:
system("ffmpeg -i filmavis.ogv -ss 0
       -to 10 filmavis_10s.mp4")

# Windows:
shell("ffmpeg -i filmavis.ogv -ss 0
       -to 10 filmavis_10s.mp4")

library(glue)
dir.create("filmavis_video")
startpunkter <- seq(0, vidinfo$duration, 60)
for (i in seq_along(startpunkter)) {
  filnavn <- glue("filmavis_video/del_{i}.mp4")
  kommando <- glue("ffmpeg -i filmavis.ogv
                   -ss {startpunkter[i]} -t 60 {filnavn}")
  system(kommando) # Mac/Linux
  # shell(kommando) # Windows
}

library(glue)
dir.create("filmavis_video")
startpunkter <- seq(0, vidinfo$duration, 60)
for (i in seq_along(startpunkter)) {
  filnavn <- glue("filmavis_video/del_{i}.mp4")
  kommando <- glue("ffmpeg -i filmavis.ogv -ss {startpunkter[i]} -t 60 {filnavn}")
  system(kommando) # Mac/Linux
}

biter <- list.files("filmavis_video", full.names = TRUE)
biter <- paste0("file '", biter, "'")
write(biter, "biter.txt")

# Mac/Linux:
system("ffmpeg -f concat -i biter.txt samlet.mp4")

# Windows:
shell("ffmpeg -f concat -i biter.txt samlet.mp4")

system("ffmpeg -f concat -i biter.txt samlet.mp4")

testfil <- "filmavis_video/del_1.mp4"
av_encode_video(testfil,
                output = "film_lys.mp4",
                vfilter = "eq=brightness=0.2",
                audio = testfil
)

av_encode_video(testfil,
                output = "film_beskåret.mp4",
                vfilter = "crop=395:300:70:0",
                audio = testfil
)

av_encode_video(testfil,
                output = "film_tekst.mp4",
                vfilter = "drawtext=fontfile=arial.ttf : 
                          text='Filmavisen 11. august 1941' : 
                          fontcolor=orange : fontsize=20 : 
                          x=150 : y=50",
                audio = testfil
)

# Mac/Linux:
system("ffmpeg -copyts -ss 00:01:30 -i filmavis.mp4
       -vf 'drawtext=fontfile=arial.ttf : fontcolor=yellow :
       fontsize=20 : text=%{pts\\\\:hms} : x=70 : y=280'
       -vframes 1 screenshot.png")

# Windows:
shell("ffmpeg -copyts -ss 00:01:30 -i filmavis.mp4
       -vf 'drawtext=fontfile=arial.ttf : fontcolor=yellow :
       fontsize=20 : text=%{pts\\\\:hms} : x=70 : y=280'
       -vframes 1 screenshot.png")

system("ffmpeg -copyts -ss 00:01:30 -i filmavis.mp4 -vf 'drawtext=fontfile=arial.ttf : fontcolor=yellow : fontsize=20 : text=%{pts\\\\:hms} : x=70 : y=280' -vframes 1 screenshot.png")

dir.create("filmavis_scener")

# Mac/Linux:
system('ffmpeg -i filmavis.ogv -vf "select=gt(scene\\\\,0.4)"
       -vsync vfr filmavis_scener/img%03d.png')

# Windows:
shell('ffmpeg -i filmavis.ogv -vf "select=gt(scene\\\\,0.4)"
       -vsync vfr filmavis_scener/img%03d.png')

dir.create("filmavis_scener")
system('ffmpeg -i filmavis.ogv -vf "select=gt(scene\\\\,0.4)" 
       -vsync vfr filmavis_scener/img%03d.png')

library(magick)
library(dplyr)
scenefiler <- list.files("filmavis_scener", full.names = TRUE)
image_read(scenefiler) |>
  image_montage(tile = "6", geometry = "x150+5+5") 

# Mac/Linux:
system('ffmpeg -i filmavis.ogv -vf "select=gt(scene\\\\,0.4),
       metadata=print:file=tider.txt" -vsync vfr
       filmavis_scener/img%03d.png')

# Windows:
shell('ffmpeg -i filmavis.ogv -vf "select=gt(scene\\\\,0.4),
       metadata=print:file=tider.txt" -vsync vfr
       filmavis_scener/img%03d.png')

system('ffmpeg -i filmavis.ogv -vf "select=gt(scene\\\\,0.4), metadata=print:file=tider.txt" -vsync vfr filmavis_scener/img%03d.png')

frame:0    pts:113     pts_time:4.52
lavfi.scene_score=0.779216

library(stringr)
linjer <- readLines("tider.txt")
oddetallslinjer <- linjer[seq(1, length(linjer), 2)]
index <- str_extract(oddetallslinjer, "(?<=frame:)\\d+")
pts <- str_extract(oddetallslinjer, "(?<=pts:)\\d+")
pts_time <- str_extract(oddetallslinjer, "(?<=pts_time:)[\\d\\.]+")
df <- data.frame(index, pts, pts_time)

dir.create("film_objekt_inn")
dir.create("film_objekt_ut")

av_video_images("filmavis_video/del_1.mp4", 
                destdir = "film_objekt_inn", fps = 5)
bilder <- list.files("film_objekt_inn", full.names = TRUE)

mdetect <- image_darknet_model(type = "detect",
  model = "tiny-yolo-voc.cfg",
  weights = system.file(package = "image.darknet", 
                        "models", 
                        "tiny-yolo-voc.weights"
                        ),
  labels = system.file(package = "image.darknet", 
                       "include", 
                       "darknet", 
                       "data", 
                       "voc.names"
                       )
  )

# [NB: Følgende kommando kan ta flere minutter å prosessere.]
for (i in seq_along(bilder)) {
  image_darknet_detect(file = bilder[i], object = mdetect)
  destinasjon <- paste0("film_objekt_ut/", basename(bilder[i]))
  file.copy("predictions.png", destinasjon)
}

bilder_objekt <- list.files("film_objekt_ut", full.names = TRUE)
av_encode_video(bilder, "film_objekt.mp4", framerate = 5)

# Opprensking
kan_slettes <- list.files(
  pattern = "ogv$|wav$|mp3$|mp4$|jpg$|png$|tiff$|gif$|bin$|txt$"
  )
file.remove(kan_slettes)
unlink("film_objekt_inn", recursive = TRUE)
unlink("film_objekt_ut", recursive = TRUE)
unlink("filmavis_bilder", recursive = TRUE)
unlink("filmavis_lyd", recursive = TRUE)
unlink("filmavis_scener", recursive = TRUE)
unlink("filmavis_video", recursive = TRUE)
