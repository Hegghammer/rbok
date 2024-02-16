#-----------------------------------
# Kode til kapittel 6 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("timelineS", "ggplot2", "ggeasy", "ggimage", "ggraph", "DiagrammeR", "rsvg", "DiagrammeRsvg", "devtools"))
devtools::install_github("hegghammer/rforalle")

# 6.1 Tidslinjer ----------------------------------------

library(rforalle)
hent_data("kap06_slag.csv")
df_slag <- read.csv("slag.csv")

str(df_slag)
head(df_slag)

df_slag$år <- paste0(df_slag$år, "-07-01")
df_slag$år <- as.Date(df_slag$år)

library(timelineS)
timelineS(df_slag)

timelineS(df_slag,
  main = "Kjente slag i vikingtiden",
  buffer.days = 8000, # Antall dager forlengelse på hver side
  line.width = 5, # Tykkelse på tidslinjen
  line.color = "darkgreen", # Farge på tidslinjen
  scale = "20 years", # Intervaller langs tidslinjen
  scale.cex = 1, # Fontstørrelsen på årstallene langs tidslinjen
  scale.tickwidth = 3, # Tykkelsen på de vertikale taggene
  labels = paste(df_slag[[1]]), # Hvilke data som skal utgjøre teksten
  label.color = "orange", # Farge på linjen mellom hovedlinjen og titlene
  label.cex = 1,
  point.color = "red", # Farge på punktmarkøren
  )

timelineS(df_slag,
  main = "Kjente slag i vikingtiden",
  buffer.days = 8000, # Antall dager forlengelse på hver side
  line.width = 3, # Tykkelse på tidslinjen
  line.color = "darkgreen", # Farge på tidslinjen
  scale = "20 years", # Intervaller langs tidslinjen
  scale.cex = .7, # Fontstørrelsen på årstallene langs tidslinjen
  scale.tickwidth = 1, # Tykkelsen på de vertikale taggene 
  labels = paste(df_slag[[1]]), # Hvilke data som skal utgjøre teksten
  label.color = "orange", # Farge på linjen mellom hovedlinjen og titlene
  label.cex = .5,
  point.color = "red", # Farge på punktmarkøren
  )

png("tidslinje.png", width = 1000, height = 600, units = "px")
timelineS(df_slag,
  main = "Kjente slag i vikingtiden",
  buffer.days = 8000, # Antall dager forlengelse på hver side
  line.width = 5, # Tykkelse på tidslinjen
  line.color = "darkgreen", # Farge på tidslinjen
  scale = "20 years", # Intervaller langs tidslinjen
  scale.cex = 1, # Fontstørrelsen på årstallene langs tidslinjen
  scale.tickwidth = 3, # Tykkelsen på de vertikale taggene
  labels = paste(df_slag[[1]]), # Hvilke data som skal utgjøre teksten
  label.color = "orange", # Farge på linjen mellom hovedlinjen og titlene
  label.cex = 1,
  point.color = "red", # Farge på punktmarkøren
  )
dev.off()

df_slag <- read.csv("slag.csv")

start <- 800
slutt <- 1100

makshøyde <- 3

library(ggplot2)
linje <- ggplot() +
  geom_segment(aes(x = start,
                   xend = slutt,
                   y = 0,
                   yend = 0
                   ),
               linewidth = 1,
               color = "purple",
               arrow = arrow(ends = "both", length = unit(0.3, "cm"))
               ) +
  scale_y_continuous(limits = c(-makshøyde, makshøyde))
linje

merker <- seq(start + 20, slutt -20, 20)

tallinje <- linje +
  annotate(geom = "text",
           x = merker,
           y = -.2,
           label = merker,
           size = 5) +
  geom_segment(aes(x = merker,
                   xend = merker,
                   y = 0,
                   yend = -.08
                   ),
               linewidth = 1,
               color = "purple"
               )
tallinje

høyder <- c(1, -1, 2, -2, 1, -1)

stolper <- tallinje +
  geom_segment(aes(y = 0,
                   yend = høyder,
                   x = df_slag$år,
                   xend = df_slag$år
                   ),
               linewidth = .3) +
  geom_point(aes(x = df_slag$år, y = 0), size = 2)
stolper

beskrivelser <- paste0(df_slag$navn, "\n", df_slag$år)

tekst <- stolper +
  annotate(geom = "label",
           x = df_slag$år,
           y = høyder,
           label = beskrivelser,
           size = 4,
           label.padding = unit(0.3, "cm"),
           fill = "wheat")
tekst

library(ggeasy)
tekst +
  theme_void() +
  labs(title = "\nKjente slag i vikingtiden") +
  easy_center_title() +
  easy_plot_title_size(30) +
  theme(plot.background = element_rect(fill = "lightblue"))

library(ggimage)
bilder <- stolper +
  geom_image(aes(x = df_slag$år, 
                 y = høyder, 
                 image = df_slag$bilde
                 ), 
             size = .2)
bilder

opphav <- paste("Kunstnere: \n", 
                paste(df_slag$kunstner, collapse = "\n")
                )

bilder +
  theme_void() +
  labs(title = "\nKjente slag i vikingtiden") +
  easy_center_title() +
  easy_plot_title_size(30) +
  theme(plot.background = element_rect(fill = "mistyrose")) +
  annotate(geom = "text",
           x = 840,
           y = -1.7,
           label = opphav,
           size = 5)

# 6.2 Gantt-diagrammer ----------------------------------------

hent_data("kap06_konger.csv")
df_konger <- read.csv("konger.csv")
df_konger

library(ggplot2)
ggplot(data = df_konger) +
  geom_segment(aes(x = født, xend = død, y = navn, yend = navn))

df_konger$navn <- factor(df_konger$navn, levels = df_konger$navn)

ggplot(df_konger) +
  geom_segment(aes(x = født, xend = død, y = navn, yend = navn))

ggplot(df_konger) +
  geom_segment(aes(x = født, xend = død, y = navn, yend = navn)) +
  scale_y_discrete(limits = rev)

gantt_farger <- ggplot(df_konger, aes(x = født, xend = død, y = navn, yend = navn)) +
  geom_segment(linewidth = 10, color = "purple") +
  scale_y_discrete(limits = rev)
gantt_farger

gantt_ferdig <- gantt_farger +
  labs(title = "Når levde de første norske kongene?", x = "", y = "") +
  theme(axis.line.x = element_line(linewidth = 1),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.ticks.x = element_line(linewidth = .5),
        axis.ticks.length.x = unit(.5, "cm"),
        panel.background = element_rect(fill = "lavenderblush2"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.x = element_line(color = "grey80"),
        panel.grid.major.y = element_blank()
        )
gantt_ferdig

df_konger$midten <- df_konger$født + (df_konger$død - df_konger$født) / 2

df_konger

ggplot(df_konger, aes(x = født, xend = død, y = navn, yend = navn)) +
  geom_segment(linewidth = 10, color = "darkgoldenrod1") +
  annotate(geom = "text", 
           x = df_konger$midten, 
           y = df_konger$navn, 
           label = df_konger$navn, 
           color = "black",
           fontface = "bold",
           size = 2) +
  scale_y_discrete(limits=rev) +
  labs(title = "Når levde de første norske kongene?", x = "", y = "") +
  theme(axis.line.x = element_line(linewidth = 1),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.ticks.x = element_line(linewidth = .5),
        axis.ticks.length.x = unit(.5, "cm"),
        panel.background = element_rect(fill = "grey95"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.x = element_line(color = "grey80"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill = "wheat"),
        ) 

# 6.3 Dendrogrammer ----------------------------------------

hent_data("kap06_slekt.csv")
df_rel <- read.csv("slekt.csv")
df_rel

library(ggraph)
ggraph(df_rel, layout = "auto") + 
  geom_edge_diagonal()

ggraph(df_rel, layout = "auto") + 
  geom_edge_diagonal() + 
  geom_node_label(aes(label = name), size = 2)

ggraph(df_rel, layout = "dendrogram") + 
  geom_edge_diagonal() + 
  geom_node_label(aes(label = name), size = 2) +
  scale_x_continuous(limits = c(-0.2,6.2)) +
  scale_y_continuous(limits = c(-0.2,3.2)) +
  labs(title = "Hårfagreætta")

farger <- c("gold2", "gold2", "gold2", "gold2", "grey", "gold2", "grey", "grey", "grey", "grey", "white")

ggraph(df_rel, layout = "dendrogram") + 
  geom_edge_diagonal(width = 0.5, color = "grey") + 
  geom_node_label(aes(label = name), 
                  size = 2, 
                  fill = farger) +
  scale_x_continuous(limits = c(-0.2,6.2)) +
  scale_y_continuous(limits = c(-0.2,3.2)) +
  labs(title = "Hårfagreætta")

ggraph(df_rel, layout = "partition") + 
  geom_edge_diagonal()

ggraph(df_rel, layout = "dendrogram") + 
  geom_edge_diagonal() +
  coord_flip() 

ggraph(df_rel, layout = "dendrogram") + 
  geom_edge_bend()

ggraph(df_rel, layout = "dendrogram") + 
  geom_edge_link()

ggraph(df_rel, layout = "dendrogram") + 
  geom_edge_elbow()

# 6.4 Prosessdiagrammer ----------------------------------------

rad1 <- data.frame(from = "Kristendom", 
                   to = "Normer\nmot\nslaveri")
rad2 <- data.frame(from = "Kristendom", 
                   to = "Solidaritet\nmed andre\nkristne")
rad3 <- data.frame(from = "Kristendom", 
                   to = "Insentiver\nfor\nhandel")

rad4 <- data.frame(from = "Normer\nmot\nslaveri", 
                   to = "Færre tokt")
rad5 <- data.frame(from = "Solidaritet\nmed andre\nkristne", 
                   to = "Færre tokt")
rad6 <- data.frame(from = "Insentiver\nfor\nhandel", 
                   to = "Færre tokt")

df_prosess <- rbind(rad1, rad2, rad3, rad4, rad5, rad6)
df_prosess

ggraph(df_prosess, layout = "auto") + 
  geom_edge_link(width = .5, color = "darkred") +
  geom_node_text(aes(label = name), size = 3) +
  scale_x_continuous(limits = c(-.5, 2.5)) +
  scale_y_continuous(limits = c(0.75, 3.25))

ggraph(df_prosess, layout = "auto") + 
  geom_edge_link(width = .5, 
                 color = "darkred",
                 arrow = arrow(length = unit(3, "mm")), 
                 start_cap = square(1.7, "cm"),
                 end_cap = square(1.7, "cm")
                ) +
  geom_node_text(aes(label = name), size = 3) +
  scale_x_continuous(limits = c(-.5, 2.5)) +
  scale_y_continuous(limits = c(0.75, 3.25))

ggraph(df_prosess, layout = "auto") + 
  geom_edge_link(width = .5, 
                 color = "darkred",
                 arrow = arrow(length = unit(3, "mm")), 
                 start_cap = square(1.7, "cm"),
                 end_cap = square(1.7, "cm")
                ) +
  geom_node_text(aes(label = name), size = 3) +
  scale_x_continuous(limits = c(-.5, 2.5)) +
  scale_y_continuous(limits = c(0.75, 3.25)) +
  geom_node_point(shape = 0, size = 22)

ggraph(df_prosess, layout = "auto") + 
  geom_edge_diagonal(width = .5, 
                 color = "darkred",
                 arrow = arrow(length = unit(3, "mm")), 
                 start_cap = square(1.7, "cm"),
                 end_cap = square(1.7, "cm")
                ) +
  geom_node_text(aes(label = name), size = 3) +
  scale_x_continuous(limits = c(-.5, 2.5)) +
  scale_y_continuous(limits = c(0.75, 3.25)) +
  geom_node_point(shape = 0, size = 22)

ggraph(df_prosess, layout = "auto") + 
  geom_edge_diagonal(width = .5, 
                 color = "darkred",
                 arrow = arrow(length = unit(3, "mm")), 
                 start_cap = square(1.7, "cm"),
                 end_cap = square(1.7, "cm")
                ) +
  geom_node_text(aes(label = name), size = 3) +
  geom_node_point(shape = 0, size = 22) +
  coord_flip() +
  scale_y_reverse(limits = c(3.25, 0.75)) +
  scale_x_continuous(limits = c(-.5, 2.5))

library(DiagrammeR)
grViz("
  digraph {
    graph[rankdir = LR]
    node [shape = box]
    A -> B -> C
  }
")

grViz(
  "digraph {
     graph [rankdir = LR]

     A [label = 'Firkant',
        shape = box,
        style = filled,
        fillcolor = green]

     B [label = 'Stor\nsirkel',
        shape = circle,
        style = filled,
        fillcolor = lightblue,
        width = 2]

     C [label = 'Rektangel',
        shape = rectangle,
        style = filled,
        fillcolor = red]

     D [label = 'Mappe',
        shape = folder,
        style = filled,
        fillcolor = linen]

     A -> B -> C
     A -> C
     A -> D
  }"
)

grViz(
  "digraph {
     graph [rankdir = LR]

     edge [color = purple penwidth = 0.5]
     A -> B -> C
     edge [color = orange penwidth = 2]
     A -> C
     edge [color = blue penwidth = 1]
     A -> D
  }"
)

library(rsvg)
library(DiagrammeRsvg)
diagram <- grViz("
  digraph {
    graph[rankdir = LR]
    node [shape = box]
    A -> B -> C
  }
")
svg <- export_svg(diagram)
rsvg_png(charToRaw(svg), "diagram.png")

# 6.5 Nettverksdiagrammer ----------------------------------------

library(rforalle)
hent_data("kap06_nettverk.csv")
df_nettverk <- read.csv("nettverk.csv")
head(df_nettverk)

ggraph(df_nettverk, layout = 'stress') +
  geom_edge_link(aes(color = type)) +
  geom_node_point() +
  geom_node_label(aes(label = name), size = 2) +
  scale_edge_color_manual(values = c('green3', 'red', 'blue')) +
  labs(title = 'Tore Hunds verden') +
  scale_x_continuous(limits = c(-2,2)) +
  scale_y_continuous(limits = c(-2,2.5))

# Opprensking
kan_slettes <- list.files(pattern = 'csv$|png$')
file.remove(kan_slettes)
