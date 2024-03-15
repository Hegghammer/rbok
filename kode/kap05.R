#-----------------------------------
# Kode til kapittel 5 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("ggplot2", "ggthemes", "patchwork", "devtools"))
devtools::install_github("hegghammer/rforalle")

# 5.1 Alt er datarammer ----------------------------------------
library(rforalle)
hent_data("kap05_befolkning.csv")
df_bef <- read.csv("befolkning.csv")

class(df_bef)

head(df_bef)

str(df_bef)

nrow(df_bef)

ncol(df_bef)

names(df_bef)

# 5.2 Grafer med grunninstallasjonen ----------------------------------------

plot(x = df_bef$år, y = df_bef$befolkning_1_januar)

plot(x = df_bef$år, y = df_bef$befolkning_1_januar, type = "l")

options(scipen = "999")

plot(x = df_bef$år, 
     y = df_bef$befolkning_1_januar, 
     type="l", 
     xlab = "", # To anførselstegn uten noe i mellom fjerner tittelen
     ylab = "Folketall"
     )

plot(x = df_bef$år, 
     y = df_bef$befolkning_1_januar, 
     type="l", 
     xlab = "",
     ylab = "Folketall",
     main = "Norges befolkning, 1735-2022",
     sub = "Kilde: SSB"
     )

plot(x = df_bef$år,
     y = df_bef$befolkning_1_januar, 
     type = "l", 
     xlab = "",
     ylab = "Folketall",
     main = "Norges befolkning, 1735-2022",
     sub = "Kilde: SSB",
     col = "red",        # Rød linje
     col.lab = "grey50", # Grå aksetitler
     lwd = "2",          # 2x linjetykkelse
     cex.main = 1.5,     # 1.5x fontstørrelse på hovedtittelen
     cex.sub = 0.7,      # 0.7x fontstørrelse på undertittelen
     cex.lab = 1.2,      # 1.2x fontstørrelse på aksetitlene
     cex.axis = 0.8,       # 0.8x fontstørrelsen på akse-merkelappene
     frame.plot = FALSE    # Ikke ramme rundt grafen
     )

png("min_graf.png", width = 600, height = 300, units = "px")

plot(x = df_bef$år, y = df_bef$befolkning_1_januar, type = "l", xlab = "", ylab = "Folketall", main = "Norges befolkning, 1735-2022", sub = "Kilde: SSB", col = "red", col.lab = "grey50", lwd = "2", cex.main = 1.5, cex.sub = 0.7, cex.lab = 1.2, cex.axis = 0.8, frame.plot = FALSE)

dev.off()

# 5.3 GG-systemet ----------------------------------------

library(ggplot2)

ggplot(data = df_bef, aes(x = år, y = befolkning_1_januar)) +
  geom_line()

ggplot(data = df_bef, aes(x = år, y = befolkning_1_januar)) +
  geom_line() +
  labs(title = "Norges befolkning, 1735-2022")

ggplot(data = df_bef, aes(x = år, y = befolkning_1_januar)) +
  geom_line(color = "darkgreen")

ggplot(df_bef, aes(år, befolkning_1_januar)) +
  geom_line()

# 5.4 Tilpasninger ----------------------------------------

graf <- ggplot(df_bef, aes(år, befolkning_1_januar)) +
  geom_line(color = "darkgreen")

graf

graf_titler <- graf +
  labs(title = "Norges befolkning, 1735-2022",
       subtitle = "Per 1. januar hvert år",
       caption = "Kilde: SSB",
       x = "", y = "Folketall"
       )
graf_titler

graf_fonter <- graf_titler +
  theme(plot.title = element_text(size = 20, 
                                  face = "bold", 
                                  colour = "darkred"),
        plot.subtitle = element_text(size = 12, 
                                     face = "italic"),
        plot.caption = element_text(size = 10, 
                                    face = "bold", 
                                    colour = "grey"),
        axis.title.y = element_text(size = 10, 
                                    face = "bold")
        )
graf_fonter

graf_posisjoner <- graf_fonter +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, 
                                     margin = margin(3, 0, 3, 0, "mm")),
        axis.title.y = element_text(angle = 0, 
                                    vjust = 0.4)
        )
graf_posisjoner

graf_akser <- graf_posisjoner +
  theme(axis.line = element_line(color = "grey"))
graf_akser

graf_aksemerker <- graf_akser +
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  scale_y_continuous(limits = c(0,7000000))
graf_aksemerker

graf_aksefonter <- graf_aksemerker +
  theme(axis.text = element_text(size = 8, 
                                 face = "bold", 
                                 colour = "grey50"
                                 )
        )
graf_aksefonter

graf_mellomrom <- graf_aksefonter +
  scale_y_continuous(expand = c(0,0), limits = c(0,7000000)) +
  scale_x_continuous(expand = c(0,0), breaks = c(1750, 1800, 1850, 1900, 1950, 2000))
graf_mellomrom

graf_fyll <- graf_mellomrom +
  geom_area(colour = "darkgreen", fill = "grey")
graf_fyll

graf_farge <- graf_fyll +
  theme(panel.background = element_rect(fill = "grey95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "grey90")
        )
graf_farge 

graf_marg <- graf_farge +
  theme(plot.margin = margin(0.5,1,0.5,0.5, unit = "cm"))
graf_marg

graf_marg +
  theme_classic()

library(ggthemes)
graf_marg +
  theme_economist()

graf_linje <- graf_marg +
  annotate("line", 
           x = c(1735,2022), 
           y = 4000000,
           color = "red",
           lty = "dashed"
           )
graf_linje

df_bef$befolkning_1_januar[df_bef$år == 1814]

y1814 <- df_bef$befolkning_1_januar[df_bef$år == 1814]

graf_punkt <- graf_linje +
  annotate("point", 
           x = 1814,
           y = y1814,
           colour = "purple"
           )
graf_punkt

påskrift <- paste("1814:", y1814)

graf_tekst <- graf_punkt +
  annotate("text", 
           x = 1814, 
           y = 1500000, 
           label = påskrift
           )
graf_tekst

ny_påskrift <- paste("1814:\n", y1814)

graf_boks <- graf_punkt +
  annotate("label", 
           x = 1814, 
           y = 1500000, 
           label = ny_påskrift, 
           size = 2.5)
graf_boks

# 5.5 Andre typer grafer ----------------------------------------

df_utsnitt <- subset(df_bef, år %in% 1901:1950)

punktgraf <- ggplot(df_utsnitt, aes(år, levendefødte_i_alt)) +
  geom_line() +
  geom_point() +
  labs(title = "Fødsler i Norge, 1900-1950",
       x = "") +
  theme_classic()
punktgraf

søylediagram <- ggplot(df_utsnitt, aes(år, levendefødte_i_alt)) +
  geom_bar(stat = "identity") +
  labs(title = "Fødsler i Norge, 1900-1950",
       x = "") +
  theme_classic()
søylediagram

spredningsplott <- ggplot(df_bef, aes(inngåtte_ekteskap, levendefødte_i_alt)) +
  geom_point() +
  labs(title = "Sammenheng mellom ekteskap og fødsler i Norge, 1735-2022",
       subtitle = "År som observasjoner"
       ) +
  theme_classic()
spredningsplott

ggplot(df_bef, aes(år, levendefødte_per_1000)) +
  geom_line() + 
  labs(title = "Fødsler per 1000 innbyggere i Norge, 1735-2022",
       x = "") +
  theme_classic()

library(patchwork)
punktgraf + søylediagram

punktgraf + søylediagram & 
theme(plot.title = element_text(size = 9),
      plot.subtitle = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_blank())

(punktgraf | søylediagram) / graf_marg &
theme(plot.title = element_text(size = 9),
      plot.subtitle = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_blank())

# 5.6 Lagre som fil ----------------------------------------

ggsave("graf.pdf")

ggsave("graf.pdf", width = 20, height = 15, units = "cm")

ggsave("graf2.png", søylediagram)

# Opprensking
kan_slettes <- list.files(pattern = "csv$|pdf$|png")
file.remove(kan_slettes)
