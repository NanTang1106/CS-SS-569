library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggstance)

iver <- read.delim2('iverRevised.txt', sep=',')
iver <- data.frame(iver)
iver$povertyReduction <- as.numeric(levels(iver$povertyReduction)[iver$povertyReduction])
iver$effectiveParties <- as.numeric(levels(iver$effectiveParties)[iver$effectiveParties])

col_brewer <- brewer.pal(n=3, 'Set1')
red <- col_brewer[1]
blue <- col_brewer[2]
green <- col_brewer[3]

p1 <- ggplot(data=iver, mapping=aes(x=effectiveParties, y=povertyReduction, col=partySystem, shape = partySystem)) +
  geom_point(size=2) +
  geom_smooth(aes(group=1), method=MASS::rlm, method.args=list(method='MM'), color='black', size = 0.5) +
  geom_text_repel(aes(label=country, col=partySystem), show.legend = F, size=3) +
  scale_color_manual(values = c("Majoritarian" = blue, "Proportional" = green, 'Unanimity' = red)) +
  scale_shape_manual(values = c('Majoritarian' = 17, 'Proportional'=15, 'Unanimity'=16)) +
  scale_x_continuous(trans='log', breaks=2:7) +
  scale_y_continuous(breaks=seq(0, 80, 20), limits=c(0, 100)) +
  coord_cartesian(ylim=c(0, 80)) +
  geom_rug(color='black')

p1 <- p1 + theme(
  panel.background = element_rect(fill=NA),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  legend.position = c(0.15, 0.8),
  legend.title=element_blank(),
  legend.background = element_blank(),
  legend.key = element_rect(fill=NA, color=NA)
)


color_brew <- brewer.pal(9, 'Set1')
blue <- color_brew[2]
orange <- color_brew[5]

presVote <- read.delim2('presVoteEV.txt', sep=',')
presVote$nonwhite <- as.factor(presVote$nonwhite)
presVote$rlibcon <- as.numeric(presVote$rlibcon)
presVote$pe <- as.numeric(levels(presVote$pe)[presVote$pe])
presVote$lower <- as.numeric(levels(presVote$lower)[presVote$lower])
presVote$upper <- as.numeric(levels(presVote$upper)[presVote$upper])

p2 <- ggplot(data = presVote, mapping=aes(y = pe, x=rlibcon, col=nonwhite)) +
  facet_wrap(~vote92) +
  geom_line(aes(color=nonwhite, group=nonwhite)) +
  scale_color_manual(values=c(blue, orange), labels=c('White', 'Non-white')) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=nonwhite), alpha=0.5, show.legend = F, linetype=0) +
  scale_fill_manual(values=c(blue, NA)) + 
  geom_line(aes(y = upper, linetype=nonwhite, group=nonwhite), show.legend = F) + 
  geom_line(aes(y = lower, linetype=nonwhite), show.legend = F) +
  scale_linetype_manual(values=c(0 ,2)) +
  scale_x_continuous(breaks=1:7) +
  scale_y_continuous(breaks=seq(0, 1, 0.2))

p2 <- p2 + goldenScatterCAtheme + theme(legend.position = 'bottom', legend.title = element_blank())

  

cyyoungFD <- read.delim("cyyoungFD.txt", sep=',')

cyyoungFD <- cyyoungFD %>% pivot_longer(cols=pe_m1:upper_m2, names_to = 'name', values_to = 'value') %>%
  separate(name, into=c("stat", "model"), sep="_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(model)

cyyoungFD <- cyyoungFD %>% 
  mutate(signif = case_when(
    lower > 0 & upper > 0 ~ TRUE,
    lower < 0 & upper < 0 ~ TRUE,
    TRUE ~ FALSE))

cyyoungFD %>%
  ggplot(mapping = aes(x=pe, y=fct_reorder(covariate, pe, .desc = T), xmax=upper, xmin=lower)) +
  geom_pointrangeh(aes(col=model, fill=signif, shape=signif), position = position_dodge2v(height=0.7)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(sec.axis=dup_axis(~.)) +
  scale_color_manual(values=c(blue, orange)) +
  scale_shape_manual(values=c(21, 19)) +
  scale_fill_manual(values=c("white", NA)) +
  labs(y=element_blank()) +
  guides(shape='none', fill='none') +
  theme_bw() +
  theme(legend.position = c(0.13, 0.13), 
        legend.title = element_blank(), 
        axis.ticks.x = element_blank())



measles <- read.delim2("measles.txt", sep=',', stringsAsFactors = F)
measles$count <- as.numeric(measles$count)

levels <- rev(c('0', '0-1', '1-10', '10-100', '100-500', '500-1000', '>1000'))

measles <- measles %>%
  mutate(
    year = as.factor(year),
    state = as.factor(state),
    countCat = factor(countCat, levels=levels)
  )

measles %>%
  ggplot(mapping = aes(x=year, y=state, fill=countCat)) +
  geom_tile(color='white', size=0.25) +
  scale_fill_brewer(palette = 'YlGnBu', direction=-1, na.value='gray90') +
  scale_x_discrete(expand=c(0,0), breaks=seq(1930, 2000, 10)) +
  theme_gray(base_size=8) +
  theme(
    legend.text = element_text(face='bold'),
    axis.ticks = element_line(size=0.4),
    plot.background = element_blank(),
    panel.border=element_blank(),
    legend.key.width = grid::unit(0.2,'cm')
  )


  

