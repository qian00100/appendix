{\rtf1\ansi\ansicpg936\cocoartf2822
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 library(dplyr)\
library(ggplot2)\
\
# Relative H\'e4ufigkeit pro Wort innerhalb jedes Corpus berechnen\
Combined_Frequency <- Combined_Frequency %>%\
  group_by(Corpus) %>%\
  mutate(RelFreq = Count / sum(Count))\
\
# Top 20 W\'f6rter \'fcber beide Corpora (h\'f6chste RelFreq)\
top20 <- Combined_Frequency %>%\
  group_by(Noun) %>%\
  summarise(MaxFreq = max(RelFreq)) %>%\
  arrange(desc(MaxFreq)) %>%\
  slice(1:20)\
\
# Filter: nur die Top 20 behalten\
Combined_Frequency_top20 <- Combined_Frequency %>%\
  filter(Noun %in% top20$Noun)\
\
# Diagramm\
ggplot(Combined_Frequency_top20, aes(x = RelFreq,\
                                     y = reorder(Noun, RelFreq),\
                                     fill = Corpus)) +\
  geom_col(position = "dodge") +\
  labs(title = "Top 20 Nouns per Corpus (Relative Frequency)",\
       x = "Relative Frequency",\
       y = "Noun") +\
  theme_minimal() +\
  scale_fill_manual(values = c("BAWE" = "tomato", "ICE-HK" = "turquoise"))}