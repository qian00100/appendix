{\rtf1\ansi\ansicpg936\cocoartf2822
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 BAWE_data   <- subset(BAWE_ICE.HK_clean, Corpus == "BAWE")\
ICE_HK_data <- subset(BAWE_ICE.HK_clean, Corpus == "ICE-HK")\
\
BAWE_freq <- table(BAWE_data$Noun)\
ICE_HK_freq <- table(ICE_HK_data$Noun)\
\
BAWE_freq_df   <- as.data.frame(BAWE_freq)\
ICE_HK_freq_df <- as.data.frame(ICE_HK_freq)\
\
BAWE_freq_df$Percent   <- BAWE_freq_df$Freq / sum(BAWE_freq_df$Freq) * 100\
ICE_HK_freq_df$Percent <- ICE_HK_freq_df$Freq / sum(ICE_HK_freq_df$Freq) * 100\
\
colnames(BAWE_freq_df)   <- c("Noun", "BAWE_Freq", "BAWE_Percent")\
colnames(ICE_HK_freq_df) <- c("Noun", "ICEHK_Freq", "ICEHK_Percent")\
\
comparison_df <- merge(BAWE_freq_df, ICE_HK_freq_df, by = "Noun", all = TRUE)\
\
comparison_df[is.na(comparison_df)] <- 0\
\
head(comparison_df, 20)\
\
BAWE_data_clean   <- subset(BAWE_data, grepl("^[A-Za-z]", Noun))\
ICE_HK_data_clean <- subset(ICE_HK_data, grepl("^[A-Za-z]", Noun))\
\
BAWE_freq <- table(BAWE_data_clean$Noun)\
ICE_HK_freq <- table(ICE_HK_data_clean$Noun)\
\
library(ggplot2)\
\
BAWE_total_nouns <- sum(BAWE_ICE_HK_clean$BAWE_Freq, na.rm = TRUE)\
ICEHK_total_nouns <- sum(BAWE_ICE_HK_clean$ICEHK_Freq, na.rm = TRUE)\
\
BAWE_total_words <- sum(BAWE_ICE_HK_clean$BAWE_Freq, na.rm = TRUE)\
ICEHK_total_words <- sum(BAWE_ICE_HK_clean$ICEHK_Freq, na.rm = TRUE)\
\
df <- data.frame(\
  Corpus = c("BAWE", "ICE-HK"),\
  NounPercent = c(BAWE_total_nouns / BAWE_total_words * 100,\
                  ICEHK_total_nouns / ICEHK_total_words * 100)\
)\
\
ggplot(df, aes(x = Corpus, y = NounPercent, fill = Corpus)) +\
  geom_bar(stat = "identity") +\
  labs(x = "Corpus", y = "Noun Percentage (%)", title = "Overall Noun Percentage") +\
  scale_fill_manual(values = c("steelblue", "tomato")) +\
  theme_minimal()\
\
library(ggplot2)\
\
ggplot(BAWE_ICE_HK_clean, aes(x = BAWE_Percent, y = ICEHK_Percent)) +\
  geom_point(alpha = 0.6, color = "steelblue") +\
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +\
  labs(x = "BAWE Percentage", y = "ICE-HK Percentage",\
       title = "Scatter Plot of Noun Percentages") +\
  theme_minimal()\

\
qqplot(BAWE_ICE_HK_clean$BAWE_Percent,\
       BAWE_ICE_HK_clean$ICEHK_Percent,\
       xlab = "BAWE Quantiles", ylab = "ICE-HK Quantiles",\
       main = "QQ Plot of BAWE vs ICE-HK",\
       col = "darkgreen", pch = 19)\
abline(0, 1, col = "red", lty = 2) }