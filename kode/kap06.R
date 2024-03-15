#-----------------------------------
# Kode til kapittel 6 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("timelineS", "ggplot2", "ggeasy", "ggimage", "forcats", "ggraph", "DiagrammeR", "rsvg", "DiagrammeRsvg", "devtools"))
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

# 6.2 Gantt-diagrammer med `ggplot2`----------------------------------------

hent_data("kap06_konger.csv")
df_konger <- read.csv("konger.csv")

## str(df_konger)
## head(df_konger)

library(ggplot2)
ggplot(data = df_konger) +
  geom_segment(aes(x = navn, xend = navn, y = født, yend = død))

ggplot(data = df_konger) +
  geom_segment(aes(x = navn, xend = navn, y = født, yend = død)) +
  coord_flip()

library(forcats)
df_konger$navn <- fct_inorder(df_konger$navn)

ggplot(data = df_konger) +
  geom_segment(aes(x = navn, xend = navn, y = født, yend = død)) +
  coord_flip()

ggplot(data = df_konger) +
  geom_segment(aes(x = navn, xend = navn, y = født, yend = død)) +
  coord_flip() +
  scale_x_discrete(limits = rev)

gantt_farger <- ggplot(data = df_konger) +
  geom_segment(aes(x = navn, xend = navn, y = født, yend = død),
               linewidth = 10, 
               color = "purple") +
  coord_flip() +
  scale_x_discrete(limits = rev)
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

ggplot(data = df_konger) +
  geom_segment(aes(x = navn, xend = navn, y = født, yend = død),
               linewidth = 10,
               color = "darkgoldenrod1") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  annotate(geom = "text", 
           x = df_konger$navn,
           y = df_konger$midten,
           label = df_konger$navn, 
           color = "black",
           fontface = "bold",
           size = 2) +
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

# 6.3 Dendrogrammer med `ggraph`----------------------------------------

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

# 6.4 Prosessdiagrammer med Mermaid ----------------------------------------

# Skrives i https://mermaid.live
graph LR
  A --> B

# Skrives i https://mermaid.live
graph LR
  A --> B
  A --> C
  B --> D
  C --> D

# Skrives i https://mermaid.live
graph LR
  A --> B
  A --> C
  B --> D
  C --> D
  A[DiagrammeR]
  B[Mermaid]
  C[GraphViz]
  D[Diagram]

# Skrives i https://mermaid.live
graph LR
A[Mermaid]
A-->B
         B[Diagram]

# Skrives i https://mermaid.live
graph LR
  A --> B
  A --> C
  B --> D
  C --> D

  A(DiagrammeR)
  B[Mermaid]
  C[(GraphViz)]
  D{Diagram}

  style B fill:yellow,stroke-dasharray: 5 5

# Skrives i https://mermaid.live
graph LR
  A --- B
  A -.- C
  B <--> D
  C --Tekst--> D

  A(DiagrammeR)
  B[Mermaid]
  C[(GraphViz)]
  D{Diagram}

  style B fill:yellow,stroke-dasharray: 5 5
  linkStyle 0 stroke:red
  linkStyle 2 stroke:green

# TIlbake i R:
DiagrammeR("
  graph LR
    A --> B
    A --> C
    A --> D
    B --> E
    C --> E
    D --> E
    A(Kristendom)
    B(Normer mot slaveri)
    C(Solidaritet med andre kristne)
    D(Insentiver for handel)
    E(Færre tokt)
")

# 6.5 Nettverksdiagrammer med ggraph ----------------------------------------

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
