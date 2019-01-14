library(BetaBit)
library(tidyverse)

frequon()
help(frequon)
(lyo <- BetaBit::lyo)
(pcs <- BetaBit::pcs)
(pistoale <- BetaBit::pistoale)
(roses <- BetaBit::roses)

frequon(subject = "Re: Interested?", content = roses)

roses_split <- strsplit(x = tolower(roses), split = "")
(roses_split <- data.frame(table(roses_split)))
roses_split$roses_split <- as.character(roses_split$roses_split)
roses_split[31,1] <- "k"
roses_split[31,2] <- 0
roses_split[32,1] <- "u"
roses_split[32,2] <- 0
freq <- roses_split$Freq[7:32]
names(freq) <- roses_split$roses_split[7:32]

frequon(subject = "Re: Frequencies", content = freq)

elf <- data.frame(BetaBit::EnglishLetterFrequency)
roses_split <- roses_split %>% 
  filter(roses_split %in% letters) %>% 
  mutate(letter_pct = Freq / sum(Freq) * 100) %>% 
  rename(letter = roses_split)

elf <- elf %>% mutate(letter = rownames(elf)) %>% 
  arrange(letter)

join <- left_join(elf, roses_split, by = "letter") %>% 
  rename(eng_letter_pct = BetaBit..EnglishLetterFrequency,
         cipher_letter_pct = letter_pct) 
join %>% 
  ggplot() + 
  geom_point(aes(x = letter, y = eng_letter_pct, shape = eng_letter_pct == 0), 
                        colour = "blue", size = 4, alpha = 1/3) +
  geom_point(aes(x = letter, y = cipher_letter_pct, shape = cipher_letter_pct == 0),
             colour = "red", size = 4, alpha = 1/3)

join %>% mutate(diff_pct = cipher_letter_pct - eng_letter_pct) %>% 
  ggplot(aes(x = letter, y = diff_pct, fill = diff_pct > 0)) + geom_col()


library(ggpubr)
eng_order <- join %>% ggplot() + 
  geom_point(aes(x = fct_reorder(.f = letter, .x = eng_letter_pct),
                 y = eng_letter_pct), colour = "blue", size = 4) +
  coord_flip()
cipher_order <- join %>% ggplot() + 
  geom_point(aes(x = fct_reorder(.f = letter, .x = cipher_letter_pct),
                 y = cipher_letter_pct), colour = "red", size = 4) +
  coord_flip()
ggarrange(eng_order, cipher_order, nrow = 1, ncol = 2)

(roses_translation <- chartr(old = "peszlydctwoqfxharnjmgvibuk", 
                             new = "etaoinshrdlcumwfgypbvkjxqz",
                             x = tolower(roses)))
frequon(subject = "Re: Transcription", content = roses_translation)

top100commonWords

library(tm)
corp_r_t <- Corpus(VectorSource(roses_translation))
corp_r_t <- tm_map(corp_r_t, removePunctuation)
corp_r_t <- tm_map(corp_r_t, stripWhitespace)
dtm <- TermDocumentMatrix(corp_r_t)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq = v)
d

chartr(old = "ahknowldtbuceisrmpyvfjgzqx",
       new = "hrknowdltbufeasimycvpxgzqj",
       roses_translation)
# translation from first iteration works

# join translations

old <- unlist(strsplit("ahknowldtbuceisrmpyvfjgzqx", ""))
new <- unlist(strsplit("hrknowdltbufeasimycvpxgzqj", ""))

df <- cbind.data.frame(r_t = old, new = new)

original <- unlist(strsplit("peszlydctwoqfxharnjmgvibuk", ""))
translation <- unlist(strsplit("etaoinshrdlcumwfgypbvkjxqz", ""))          

tmp <- cbind.data.frame(org = original, r_t = translation)

df <- left_join(df, tmp, by = "r_t")

# answer
df <- arrange(df, org)
old <- as.character(df$org)
old <- paste(old, sep = "", collapse = "")
new <- paste(as.character(df$new), sep = "", collapse = "")

frequon(subject = "Re: Key",
        content = c(old = old,
                    new = new))
chartr(new, old, "guns and roses")

BetaBit::lyo

frequon(subject = "Re: Next text",
        content = BetaBit::lyo)

wiki <- BetaBit::wikiquotes

lyo_split <- unlist(strsplit(x = lyo, split = " "))
lyo_split <- data.frame(table(nchar(lyo_split)))
lengths <- lyo_split$Freq
names(lengths) <- lyo_split$Var1

wiki_split <- lapply(wiki, function(x) unlist(strsplit(x = x, split = " ")))
wiki_split <- lapply(wiki_split, function(x) data.frame(table(nchar(x))))
wiki_lengths <- lapply(wiki_split, function(x) setNames(nm = x$Var1, object = x$Freq))

frequon(subject = "Re: Lengths in the text", content = lengths, attachment = wiki_lengths)

barplot(lengths, main = "lenghts")

lapply(wiki_lengths, function(x) barplot(x))

frequon(subject = "Re: Language in and message", content = "Romanian")

frequon(subject = "Re: Password", content = pistoale)

