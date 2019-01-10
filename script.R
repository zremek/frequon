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

install.packages("ggpubr")
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
