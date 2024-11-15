# Read data
results.table <- read.csv(file="results_table.csv")
match.results <- read.csv(file="match_results.csv")

### AVERAGE WINRATES PLOT

# Function that guesses energy type based on prevalent pokemon
infer.energy.type <- function(deckname) {
  if (length(grep("Pikachu", deckname)) > 0) {
    return ("Lightning")
  } else if (length(grep("Dragonite", deckname)) > 0) {
    return ("Multicolor")
  } else if (length(grep("Moltres", deckname)) > 0) {
    return ("Fire")
  } else if (length(grep("Articuno", deckname)) > 0) {
    return ("Water")
  } else if (length(grep("Mewtwo", deckname)) > 0) {
    return ("Psychic")
  } else if (length(grep("Starmie", deckname)) > 0) {
    return ("Water")
  } else if (length(grep("Alakazam", deckname)) > 0) {
    return ("Psychic")
  } else if (length(grep("Weezing", deckname)) > 0) {
    return ("Dark")
  } else if (length(grep("Ninetales", deckname)) > 0) {
    return ("Fire")
  } else if (length(grep("Marowak", deckname)) > 0) {
    return ("Fighting")
  } else if (length(grep("Venusaur", deckname)) > 0) {
    return ("Grass")
  } else if (length(grep("Exeggutor ex Victreebel", deckname)) > 0) {
    return ("Grass")
  } else if (length(grep("Arbok", deckname)) > 0) {
    return ("Dark")
  } else {
    return ("Unknown")
  }
}

# Filtering and calcuations
match.results %>%
  group_by(PlayerDeck)  %>%
  filter(PlayerDeck != OpponentDeck) %>%
  summarise(WR=sum(Result=="WIN")/(sum(Result=="WIN") + sum(Result =="LOSS")), 
            SD = sd(Result=="WIN"),
            N = n()) -> 
  summary

summary$se.low <- summary$WR - summary$SD / sqrt(summary$N)
summary$se.high <- summary$WR + summary$SD / sqrt(summary$N)
summary$energy.type <- as.factor(sapply(summary$PlayerDeck, infer.energy.type))

# Main plot
summary%>% filter(N > 100) %>%
  ggplot(aes(WR, reorder(PlayerDeck, WR, \(x) x), color=energy.type)) +
  geom_segment(aes(x=se.low, xend=se.high, yend=reorder(PlayerDeck, WR, \(x) x))) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values=c("#003e3c","#e08908","#f83c28","#9fcc2d","#ffd400","#e2ca1d","#c67bb6","#55bcf1")) +
  ylab("Deck") +
  xlab("Winrate") +
  geom_vline(xintercept=0.5, linetype="dashed") + 
  ggtitle("Overall non-mirror winrates", "More than 100 matches played in the dataset") -> overall.winrates.plot

# Add pokemon to y-axis
spacing = 0.2
scale = 0.7

magic <- cowplot::axis_canvas(overall.winrates.plot, axis = 'y') 

main <-   c("pikachu", "pikachu", "pikachu", "pikachu", "arcanine", "articuno", 
            "articuno", "articuno", "mewtwo", "starmie", "charizard", "wigglytuff", 
            "ninetales","arbok", "marowak", "marowak", "exeggutor", "marowak", 
            "muk", "arbok", "alakazam", "dragonite", "venusaur", "dragonite")
second <- c("raichu", "zapdos","zebstrika",  "electrode", "moltres",  "starmie", 
            "articuno", "greninja", "gardevoir", "greninja", "moltres", "weezing", 
            "rapidash", "weezing", "sandslash", "primeape","venusaur","dugtrio",
            "weezing", "pidgeot", "weezing", "weezing", "lilligant", "frosmoth")

for (i in 1:length(main)) {
  if (main[[i]] != second[[i]]) {
    magic <- magic + cowplot::draw_image(paste0("images/",main[[i]],".png"), y = 0.5 + 2 * ((length(main) - i) / 2), x = - spacing, scale=scale) +
      cowplot::draw_image(paste0("images/",second[[i]],".png"), y = 0.5 + 2 * ((length(main) - i) / 2), x = spacing, scale=scale)
  } else {
    magic <- magic + cowplot::draw_image(paste0("images/",main[[i]],".png"), y = 0.5 + 2 * ((length(main) - i) / 2), scale=scale)
  }
  
}

# Plot and save
cowplot::ggdraw(cowplot::insert_yaxis_grob(overall.winrates.plot, magic, position="left"))
ggsave("non-mirror-winrates.png")

### HEATMAP PLOT

# Filtering and calculations
match.results %>%
  group_by(PlayerDeck, OpponentDeck)  %>%
  summarise(WR=sum(Result=="WIN")/(sum(Result=="WIN") + sum(Result =="LOSS")), 
            SD = sd(Result=="WIN"),
            N = n()) -> 
  summary.per.archetype

summary.per.archetype$se.low <- summary.per.archetype$WR - summary.per.archetype$SD / sqrt(summary.per.archetype$N)
summary.per.archetype$se.high <- summary.per.archetype$WR + summary.per.archetype$SD / sqrt(summary.per.archetype$N)
summary.per.archetype$energy.type <- as.factor(sapply(summary.per.archetype$PlayerDeck, infer.energy.type))

factors.selected <- summary %>%
  group_by(PlayerDeck) %>%
  filter(sum(N) > 100)
factors.selected.y <- factors.selected$PlayerDeck

factors.selected <- summary %>%
  group_by(PlayerDeck) %>%
  filter(sum(N) > 500)
factors.selected.x <- factors.selected$PlayerDeck

summary.per.archetype %>% 
  filter(N >= 5) %>%
  filter(WR > 0.001) %>%
  filter(PlayerDeck %in% factors.selected.y) %>%
  filter(OpponentDeck %in% factors.selected.x) -> summary.heatmap

ggplot(summary.heatmap, aes(y=PlayerDeck, x=fct_rev(OpponentDeck))) +
  geom_tile(aes(fill=WR)) +
  scale_fill_gradient2(midpoint=0.5, labels=scales::percent) + 
  geom_text(aes(label=scales::percent(WR,1)), nudge_y=0.1) +
  geom_text(aes(label=scales::percent(se.low,0.1)),
            nudge_y=-0.3,
            nudge_x=-0.25,
            size=2) +
  geom_text(aes(label=scales::percent(se.high,0.1)),
            nudge_y=-0.3,
            nudge_x=0.25,
            size=2) +
  scale_x_discrete(position="top") +
  ylab("Deck archetype") +
  xlab("vs deck") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=60, hjust=-0.1, vjust=-0.05), legend.position = "right") +
  ggtitle("Per matchup winrates","Analysis of 29 326 tournament matches.") -> heatmap.plot

# Add pokemon to y-axis
spacing = 0.2
scale = 1
magic <- cowplot::axis_canvas(heatmap.plot, axis = 'y') 

main <-   c("mewtwo", "pikachu", "pikachu", "pikachu", "charizard", "articuno", 
            "pikachu", "ninetales", "arbok", "marowak", "exeggutor", "articuno", 
            "starmie","dragonite", "marowak", "alakazam", "wigglytuff", "muk", 
            "articuno", "venusaur", "arbok", "arcanine", "marowak", "dragonite")
second <- c("gardevoir", "zapdos","electrode",  "zebstrika", "moltres",  "starmie", 
            "raichu", "rapidash", "weezing", "sandslash", "venusaur", "articuno", 
            "greninja", "weezing", "primeape", "weezing","weezing","weezing",
            "greninja", "lilligant", "pidgeot", "moltres", "dugtrio", "frosmoth")

for (i in 1:length(main)) {
  if (main[[i]] != second[[i]]) {
    magic <- magic + cowplot::draw_image(paste0("images/",main[[i]],".png"), y = 0.5 + 2 * ((length(main) - i) / 2), x = - spacing, scale=scale) +
      cowplot::draw_image(paste0("images/",second[[i]],".png"), y = 0.5 + 2 * ((length(main) - i) / 2), x = spacing, scale=scale)
  } else {
    magic <- magic + cowplot::draw_image(paste0("images/",main[[i]],".png"), y = 0.5 + 2 * ((length(main) - i) / 2), scale=scale)
  }
  
}

# Plot and save
cowplot::ggdraw(cowplot::insert_yaxis_grob(heatmap.plot, magic, position="left"))
ggsave("heatmap_winrates.png")

### Meta position plot

# Filtering and calculus
per.pokemon.summary <- left_join(match.results, results.table[, c("Player", "tournament", "Pokemon1", "Pokemon2")], by=join_by(tournament==tournament,Opponent == Player))
per.pokemon.summary %>%
  group_by(PlayerDeck, Pokemon1) %>%
  summarise(WR=sum(Result=="WIN")/(sum(Result=="WIN") + sum(Result =="LOSS")), 
            SD = sd(Result=="WIN"),
            N = n()) %>%
  filter(N > 20) %>%
  filter(Pokemon1 %in% c("Mewtwo ex", "Pikachu ex")) %>%
  pivot_wider(names_from = Pokemon1, values_from = c(WR, SD, N)) %>%
  remove_missing() -> per.pokemon.summary.xy
per.pokemon.summary.xy$energy <- sapply(per.pokemon.summary.xy$PlayerDeck, infer.energy.type)

# Plotting and save
ggplot(per.pokemon.summary.xy, aes(`WR_Mewtwo ex`, `WR_Pikachu ex`, color=energy)) +
  geom_point()+
  ggrepel::geom_text_repel(aes(label=PlayerDeck)) +
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab("Winrate versus Mewtwo ex") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Winrate versus Pikachu ex") +
  scale_color_manual(values=c("#003e3c","#e08908","#f83c28","#9fcc2d","#ffd400","#e2ca1d","#c67bb6","#55bcf1")) +
  ggtitle("Winrate vs top meta decks", "Decks with at least 20 matches registered for each matchup")
ggsave("winratevstopdecks.png")

