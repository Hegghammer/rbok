#-----------------------------------
# Kode til kapittel 11 i "R for alle"
# Thomas Hegghammer, mars 2024
#-----------------------------------

# [Pakker brukt i dette kapittelet]
install.packages(c("lorem","readr", "readtext", "tokenizers", "dplyr", "ggplot2","quanteda", "quanteda.textplots", "RColorBrewer","quanteda.textstats", "ggcorrplot", "lexRankr", "stm","stminsights", "stringr", "spacyr", "googleLanguageR","devtools"))
devtools::install_github("hegghammer/rforalle")devtools::install_github("quanteda/quanteda.sentiment")

# 11.1 Teksthåndtering ----------------------------------------

library(lorem)
ipsum(paragraphs = 3) |> 
  as.character() |> 
  write("lorem.txt")

lorem1 <- readLines("lorem.txt")
length(lorem1)

library(readr)
lorem2 <- read_file("lorem.txt")
length(lorem2)

library(readtext)
lorem3 <- readtext("lorem.txt")
str(lorem3)

sekvens <- c("A", "C", "B")

sort(sekvens)

library(forcats)
faktorer <- fct_inorder(sekvens)

sort(faktorer)

selvvalgt <- fct_relevel(faktorer, c("C", "A", "B"))
sort(selvvalgt)

Sys.getlocale()

# Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

# 11.2 Data: Norske partiprogrammer 1945-2021 ----------------------------------------

library(rforalle)
hent_data("kap11_program.csv")
df <- read.csv("program.csv")

names(df)

substr(df$tekst[1], 1, 500)

head(df$tittel)

nrow(df)

length(unique(df$aar))

unique(df$PartiEintaltekst_forkorting)

min(df$aar)
max(df$aar)

library(dplyr)
library(ggplot2)
library(tokenizers)

df$antall_ord <- count_words(df$tekst)

df_snitt_år <- df |> 
  group_by(aar) |> 
  summarize(snitt_ord = mean(antall_ord))
  
ggplot(df_snitt_år, aes(aar, snitt_ord)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1945, 1985, 2021)) +
  labs(title = "Gjennomsnittlig lengde på norske partiprogrammer",
       subtitle = "Ap, H, V, Sp og KrF, 1945-2021",
       caption = "Data: NSD/Sikt",
       x = "", 
       y = "Antall ord"
       ) +
  theme_minimal()

sum(df$antall_ord)

table(df$målform)

df |>
  select(c(aar, PartiEintaltekst_forkorting, målform)) |>
  filter(målform == "nn")

# 11.3 Søk ----------------------------------------

grepl("Tyskland", df$tekst[1])

grepl("Tyskland", df$tekst)

grep("Tyskland", df$tekst)

indekser_tyskland <- grep("Tyskland", df$tekst)
df$tittel[indekser_tyskland]

df$tittel[grep("Tyskland", df$tekst)]

library(quanteda)
tekst <- "Gamle valgprogrammer er spennende fordi de sier noe om samfunnsutviklingen."
tokenisert_tekst <- tokens(tekst)
kwic(tokenisert_tekst, pattern = "spennende", window = 3)

programmer <- tokens(df$tekst)
treff <- kwic(programmer, pattern = "tyskland", window = 4)
head(treff)

kwic(programmer, pattern = "tysk*", window = 4)

kwic(programmer, pattern = c("tyskland", "frankrike"), window = 4)

streng <- "Venstre er ett av 11 partier. Det ble grunnlagt i 1884."

library(stringr)
str_extract_all(streng, "\\d")

str_extract_all(streng, "\\d{1,4}")

str_extract_all(streng, "\\d{4}")

str_extract_all(streng, "g.*")

str_extract_all(streng, "g.*(?= i)")

str_extract_all(streng, "(?<=Venstre ).*")

str_extract_all(streng, "(?<=Venstre )\\w*")

str_extract_all(streng, "\\w*(?=\\.)")

library(spacyr)
spacy_install()

spacy_download_langmodel(lang_models = "nb_core_news_md")

spacy_initialize(model = "nb_core_news_md")

ut <- spacy_parse(df$tekst[1])

enheter <- entity_extract(ut, type = "all")
head(enheter, 20)

enheter |> filter(entity_type == "GPE")

df_steder <-  spacy_parse(df$tekst[1:50]) |>
  entity_extract(type = "all") |>
  filter(entity_type == "GPE") |>
  count(entity) |>
  filter(n > 3) |>
  arrange(-n)

df_steder <- read.csv("data/steder.csv")

df_uten_norge <- df_steder |>
  filter(!entity %in% c("Norge", "Norges", "Noreg", "Noregs", "-Norge"))

ggplot(df_uten_norge, aes(reorder(entity, n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Stedsnavn i norske partiprogrammer",
       subtitle = "Ap, H, V, Sp og KrF, 1945-1981",
       caption = "Data: NSD/Sikt", 
       x = "", y = "Forekomster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 6))

# 11.4 Oversettelse ----------------------------------------

library(googleLanguageR)

gl_translate("Tekstanalyse er spennende.")

library(tokenizers)
df <- read.csv("program.csv")
setninger <- unlist(tokenize_sentences(df$tekst[1]))

setninger[1:3]

til_oversettelse <- setninger[1:3]
gl_translate(til_oversettelse)

gl_translate(til_oversettelse, target = "zh")

# 11.5 Oppsett for tekstanalyse ----------------------------------------

streng <- "Har du lest KrF-programmet fra 1945?"

token <- tokenize_words(streng)
class(token)
View(token)

token_renset <- tokenize_words(streng,
                               stopwords = c("du", "fra"),
                               strip_numeric = TRUE
                               )
View(token_renset)

library(quanteda)
corp_prog <- corpus(df, text_field = "tekst")

docnames(corp_prog) <- paste(df$PartiEintaltekst_forkorting, df$aar)

hent_data("kap11_stop_nrk.txt")
nrkstop <- readLines("stop_nrk.txt")

partiord <- c("stortingsvalgprogram", "arbeiderparti", "arbeiderpartiet", "arbeiderpartiets", "arbeidarparti", "arbeidarpartiet", "arbeidarpartiets", "ap", "aps", "høyre", "høire", "høgre", "høyres", "høires", "høgres", "venstre", "venstres","senterpartiet", "senterpartiets", "sp", "sps", "kristelig", "folkeparti", "folkepartiets", "krf", "kr.f", "krfs")

rusk <- c("", "ü")

stoppord <- c(nrkstop, partiord, rusk)

tok_prog <- corp_prog |> 
              tokens(
                     remove_punct = TRUE, 
                     remove_symbols = TRUE, 
                     remove_numbers = TRUE,
                     remove_separators = TRUE,
                     split_hyphens = TRUE
                     ) |> 
              tokens_replace("\\.", replacement = "", valuetype = "regex") |> 
              tokens_tolower() |>
              tokens_remove(stoppord)

dfm_prog <- dfm(tok_prog)

# 11.6 Frekvens ----------------------------------------
topfeatures(dfm_prog, 20)

library(quanteda.textplots)
library(RColorBrewer)
palett <- brewer.pal(4, "Set1")
textplot_wordcloud(dfm_prog, max_words = 100, color = palett)

library(quanteda.textstats)

df_parti <- textstat_frequency(
  dfm_prog, n = 20, groups = dfm_prog$PartiEintaltekst_forkorting
  )

ggplot(df_parti, aes(nrow(df_parti):1, frequency)) +
     geom_point() +
     scale_x_continuous(breaks = nrow(df_parti):1,
                        labels = df_parti$feature) +
     scale_y_continuous(breaks = c(700, 1300)) +
     labs(x = "", y = "Frekvens") +
     coord_flip() +
     facet_wrap(~ group, scales = "free", ncol = 5) +
     theme_minimal() +
     theme(axis.text.x = element_text(size = 6))

dfm_prog |>   
  dfm_group(groups = PartiEintaltekst_forkorting) |> 
  textstat_keyness(target = "KrF") |> 
  textplot_keyness(show_reference = FALSE, color = "gold")

dfm_andel <- dfm_prog |> 
    dfm_group(groups = aar) |> 
    dfm_weight(scheme = "prop")

library(tidyr)
av_interesse <- c("landbruket", "industrien")
    
df_begrep <- convert(dfm_andel[, av_interesse], to = "data.frame") |> 
  pivot_longer(cols = all_of(av_interesse), names_to = "begrep", values_to = "frekvens") |>
  rename(år = doc_id) |> 
  mutate(år = as.integer(år))

ggplot(df_begrep, aes(år, frekvens)) +
  geom_line(aes(color = begrep)) +
  scale_color_manual(values = c("deepskyblue", "palegreen3")) +
  scale_x_continuous(breaks = c(1945, 1985, 2021)) +
  labs(title = "Begrepsbruk over tid i partiprogrammene til\nAp, H, V, Sp og KrF, 1945-2021",
       subtitle = "100 dokumenter, ca. 2 millioner ord",
       caption = "Kilde: NSD/Sikt",
       y = "Relativ frekvens",
       x = "", color = "") +
  theme_minimal()

ind <- c("industri", "industrien", "industriell", "fabrikker", "verft")
land <- c("landbruk", "landbruket", "bonde", "bønder", "matproduksjon")
dict <- dictionary(list(industri = ind, landbruk = land))

dfm_ordbok <- dfm_andel |> 
  dfm_lookup(dictionary = dict)

df_ordbok <- convert(dfm_ordbok, to = "data.frame") |> 
  pivot_longer(cols = c(industri, landbruk), names_to = "tema", values_to = "frekvens") |>
  rename(år = doc_id) |> 
  mutate(år = as.integer(år))

ggplot(df_ordbok, aes(år, frekvens)) +
  geom_line(aes(color = tema)) +
  scale_color_manual(values = c("blue", "green4"), labels = c("Industri-\nrelaterte ord", "Landbruks-\nrelaterte ord")) +
  scale_x_continuous(breaks = c(1945, 1985, 2021)) +
  labs(title = "Tematisk begrepsbruk over tid i partiprogrammene til\nAp, H, V, Sp og KrF, 1945-2021",
       subtitle = "100 dokumenter, ca. 2 millioner ord",
       caption = "Kilde: NSD/Sikt",
       y = "Relativ frekvens",
       x = "", color = "") +
  theme_minimal()

# 11.7 Språkkvalitet ----------------------------------------

dfm_aar <- dfm_prog |> 
    dfm_group(groups = aar)
    
df_ord <- textstat_lexdiv(dfm_aar, groups = aar) |> 
  mutate(år = as.integer(document))

ggplot(df_ord, aes(år, TTR)) +
  geom_line() +
  scale_x_continuous(breaks = c(1945, 1985, 2021)) +
  labs(title = "Ordforrådsmangfold (TTR) i norske partiprogrammer\n(Ap, H, V, Sp, KrF), 1945-2021",
       caption = "Data: NSD/Sikt",
       x = "") +
  theme_minimal()

df_lesbar <- textstat_readability(corp_prog, measure = "LIW") |> 
  mutate(år = df$aar) |> 
  group_by(år) |> 
  summarize(LIW = mean(LIW))

ggplot(df_lesbar, aes(år, LIW)) +
  geom_line() +
  scale_x_continuous(breaks = c(1945, 1985, 2021)) +
  labs(title = "LIX-score i norske partiprogrammer\n(Ap, H, V, Sp, KrF), 1945-2021",
       caption = "Data: NSD/Sikt",
       x = "") +
  theme_minimal()

dfm_snitt <- dfm_prog |> 
   dfm_subset(aar > 2016)

likhet <- as.matrix(textstat_simil(dfm_snitt, 
                                   method = "cosine", 
                                   margin = "document"
                                   )
                    )

library(ggcorrplot)
ggcorrplot(likhet, type = "upper")

# 11.8 Sentiment ----------------------------------------

hent_data("kap11_positiv.txt")
hent_data("kap11_negativ.txt")

pos <- readLines("positiv.txt")
neg <- readLines("negativ.txt")

dict <- dictionary(list(
  positive = pos,
  negative = neg
))

library(quanteda.sentiment)
valence(dict) <- c(positive = 1, negative = -1)

dfm_parti <- dfm_prog |> 
  dfm_group(groups = PartiEintaltekst_forkorting)

df_sent_parti <- textstat_valence(dfm_parti, 
                                  dictionary = dict, 
                                  normalization = "all") |> 
  mutate(parti = doc_id)

ggplot(df_sent_parti, aes(reorder(parti, sentiment), sentiment)) +
  geom_point(size = 3) +
  labs(title = "Sentiment i norske partiprogrammer 1945-2021",
       subtitle = "Polaritet (-1,1) målt med med Quanteda og NorSentLex",
       caption = "Data: NSD/Sikt",
       x = "", y = "Gjennomsnittsverdi per ord") + 
  scale_y_continuous(limits = c(-.1, .2)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  theme(legend.position = "none") +
  theme_minimal()

dfm_år <- dfm_prog |> 
  dfm_group(groups = aar)
  
df_sent_år <- textstat_valence(dfm_år, dictionary = dict, normalization = "all") |> 
  mutate(år = as.integer(doc_id)) 

ggplot(df_sent_år, aes(år, sentiment)) +
  geom_line() +
  labs(title = "Sentiment i norske partipgrogrammer\n(Ap, H, V, Sp, KrF), 1945-2021",
       subtitle = "Polaritet (-1,1) målt med med Quanteda og NorSentLex",
       caption = "Data: NSD/Sikt",
       x = "", y = "Gjennomsnittsverdi per ord") +
  scale_x_continuous(breaks = c(1945, 1985, 2021)) +
  scale_y_continuous(limits = c(.03, .13)) +
  theme_minimal()

# 11.9 Innhold ----------------------------------------

tekst <- df$tekst[1]
setninger <- tokenize_sentences(tekst)[[1]]
df_setn <- data.frame(setninger = setninger)
df_setn$doknavn <- 1

library(lexRankr)
df_topp3 <- lexRank(df_setn, 
                   text = df_setn$setninger, 
                   docId = df_setn$doknavn, 
                   n = 3, 
                   continuous = TRUE)

df_topp3$sentence

dfm_stem <- tok_prog |> 
  tokens_wordstem(language = "nor") |> 
  dfm() |> 
  dfm_trim(min_docfreq = 0.1, 
           max_docfreq = 0.65, 
           docfreq_type = "prop")

stm_prog <- convert(dfm_stem, to = "stm")

# [NB: Følgende kode kan ta 5-30 minutter]
library(stm)
K <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)
kresult <- searchK(
                   documents = stm_prog$documents,
                   vocab = stm_prog$vocab,
                   K,
                   prevalence = ~ s(aar),
                   data = stm_prog$meta,
                   #cores = 8 # For evt. parallellprosessering
                   )
plot(kresult)

library(stm)

# [NB: Følgende kommando kan ta flere minutter.]
modell_stm <- stm(
                  documents = stm_prog$documents,
                  vocab = stm_prog$vocab,
                  K = 20,
                  prevalence = ~ s(aar),
                  data = stm_prog$meta,
                  )

plot(modell_stm, type = "summary")

labelTopics(modell_stm, c(1:5))

findThoughts(modell_stm, texts = df$tittel, topics = c(2,4,12))

indekser <- c(2, 4, 6, 7, 8, 11, 12, 13, 14, 15, 16, 19)

overskrifter <- c("Miljøvern", "Kristenmoral", "Distriktsutbygging", "Kristenomsorg", "Næringspolitikk", "Familiepolitikk", "Landbrukspolitikk", "Pengepolitikk", "Bærekraft", "Arbeiderrettigheter", "Innovasjon",
"EU/utenriks")

nøkkelord <- labelTopics(modell_stm)
frex <- nøkkelord$frex[, 1:4]
frex <- split(frex, seq_len(nrow(frex)))
frex <- frex[indekser]

effekter <- estimateEffect(~ s(aar), modell_stm, meta = stm_prog$meta)

library(stminsights)
df_effekter <- get_effects(effekter, "aar", type = "continuous")

df_emner <- df_effekter |> 
  filter(topic %in% indekser)
df_emner$overskrifter <- rep(overskrifter, each = 100)
df_emner$overskrifter <- factor(df_emner$overskrifter, levels = overskrifter)
df_emner$frex <- rep(frex, each = 100)

ggplot(df_emner, aes(value, proportion)) +
 geom_line() +
 labs(x = "", y = "Andel") +
 theme_minimal() +
 theme(axis.text = element_text(size = 6)) +
 facet_wrap(~ overskrifter)

# Opprensking
kan_slettes <- list.files(pattern = "txt$|csv$")
file.remove(kan_slettes)
