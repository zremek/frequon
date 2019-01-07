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
  mutate(letter_pct = Freq / sum(Freq) * 100)
