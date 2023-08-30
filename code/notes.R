

####
# Sample removal:
# cutoff = removes seq less than threshold
# drop_samples = by vector of sample names, e.g.c("H00B05")
# drop_samples_match = by partial sample names, e.g.c("H0")
# drop_seqs = by vector of sequence ID
#
# calculate relative abundance AFTER sample changes.


######

library(symportalfunctions)
library(tidyverse)
library(ggridges)


plot_data <- extract_seqs_long(folder="/Users/rof011/symbiodinium/20220919T102058_esampayo", type="relative", cutoff=1000, clade="C") |>
  mutate(numeric.ID = as.numeric((as.factor(seq.ID)))) |>
  mutate(sample.ID = as.factor(sample.ID)) |>
#  filter(abundance>0.01) |>
  complete(seq.ID, sample.ID) |>
  filter(sample.ID %in% levels(sample.ID)[1:10]) |>
  mutate(numeric.ID = as.numeric((as.factor(seq.ID)))) |>
  replace_na(list(abundance = 0))


ggplot() + theme_ridges() + ylab("") +
  geom_ridgeline(data=plot_data, aes(x = numeric.ID, y = sample.ID, height = abundance*2, fill=sample.ID), alpha=0.4, show.legend=FALSE)



tmp <- plot_data |> filter(sample.ID == "H00B06_ES22OT")


# kullback-leibler divergence
# https://towardsdatascience.com/understanding-kl-divergence-f3ddc8dff254

H00B07 <- plot_data |> filter(sample.ID=="H00B07")
H00B05 <- plot_data |> filter(sample.ID=="H00B05")
H00B06_ES22OT <- plot_data |> filter(sample.ID=="H00B05")

library(philentropy)

x <- rbind(as.numeric(H00B07$abundance),as.numeric(H00B06_ES22OT$abundance))
x <- rbind(as.numeric(H00B06_ES22OT$abundance),as.numeric(H00B07$abundance))

#calculate KL divergence
KL(x, unit='log')


as.numeric(H00B06_ES22OT$abundance) - as.numeric(H00B07$abundance)
