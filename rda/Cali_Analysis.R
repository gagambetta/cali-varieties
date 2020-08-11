## Load necessary libraries
library(readxl)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)

###Set significant digits ###

options(digits = 3)

## Read in Excel data

cali_var <- read_excel("data/cali_var.xlsx", sheet = "cali_data")

### Create new variabless ###
## Change tons and acres to 1000s of ##

cali_var <- cali_var %>% mutate(acres = acres/1000, tons = tons/1000)

## Create percetage variables ##

cali_var <- cali_var %>% group_by(year) %>% mutate(acres_per_tot = (acres/sum(acres))*100)

cali_var <- cali_var %>% group_by(year, color) %>% mutate(acres_per_color = (acres/sum(acres))*100)

## Create change in acres ##

cali_var <- cali_var %>% group_by(var) %>% arrange(year) %>% mutate(delta_acres = (acres - lag(acres))*1000)

## yield (tons per acre) ##

cali_var <- cali_var %>% mutate(yield = tons/acres)

write.csv(cali_var, "California Variety Database.csv")


## Produce correlation matrix ##

corr <- cali_var[, sapply(cali_var, is.numeric)] %>% filter(yield < 40)
cor(corr, use = "complete.obs") ## Builds correlation matrix with NAs

or
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_p <- rcorr(as.matrix(corr))
cor_results <- flattenCorrMatrix(cor_p$r, cor_p$P)


## Exploritory graphs for varieties ##
# Brix over time #
cali_var %>% filter(color != "NA") %>% ggplot(aes(factor(year), brix)) +
  geom_boxplot() +
  facet_grid(color ~ .)
  
  
  
  ggplot(aes(year, var, fill = brix)) +
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Greens")) +
  xlab("Year") +
  ylab("Variety")




cali_var %>% ggplot((aes(delta_acres))) +
  geom_density()

cali_var %>% filter(brix>0) %>% ggplot((aes(brix, value))) +
  geom_point()


## Create Red/White totals DF

totals <- cali_var %>% 
  filter(var == "Total White Wine" | var == "Total Red Wine") 

## check accounted for percentages
## total_check <- totals %>% group_by((year)) %>%
##  summarise(total_per = sum(per_total))

  ## Corrects some 100% totals (preent from small percentage, less than 0.5%, of missing varieties)

totals <- totals %>% group_by(year) %>% mutate(per_total_new = (acres/sum(acres))*100)

## Define global Red/White colors

berry_color = c("#3f6da1", "#f8d63c")

## berry_color = c("#2e4e77", "#f3c831")



## Total surface area by color graph
total_area_plot <-
  totals %>% ggplot(aes(year, acres_thousand)) +
  xlab("Year") +
  ylab("Surface area (1000 acres)") +
  scale_fill_manual(values = berry_color) +
geom_bar(aes(fill = color), stat = "identity") 


## Red White percent makup evolution
red_white_plot <- 
  totals %>% ggplot(aes(year, per_total_new)) +
  xlab("Year") +
  ylab("Surface area (% of total)") +
  scale_fill_manual(values = berry_color) +
geom_bar(aes(fill = color), stat = "identity")


## Total variety number evolution
         
var_numbers <- cali_var %>% filter(!is.na(color)) %>%
                            group_by(color, year) %>%
                            summarise(n = length(var))

var_num_plot <-
  var_numbers %>% ggplot(aes(year, n)) +
  xlab("Year") +
  ylab("Total number of varieties planted") +
  ylim(0, 100) +
  scale_fill_manual(values = berry_color) +
geom_bar(aes(fill = color), stat = "identity")

overview_figure <- grid.arrange(total_area_plot,
                       red_white_plot,
                       var_num_plot,
                       ncol = 1)

ggsave("overview_figure.svg", plot = overview_figure,
       width = 190, height = 280, units = "mm",
       device = svg, dpi=600)


### Tile graphs (All Data)

reds <- cali_var %>% 
  filter(color == "Red") %>% filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>% 
  mutate(var = reorder(var, acres_thousand))

reds_tile <- reds %>% ggplot(aes(year, var, fill = acres_thousand)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt")+
  xlab("Year") +
  ylab("Variety")

whites <- cali_var %>% 
  filter(color == "White") %>% filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>% 
  mutate(var = reorder(var, acres_thousand))

whites_tile <- whites %>% ggplot(aes(year, var, fill = acres_thousand)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  xlab("Year") +
  ylab("Variety")

comb_tile <- grid.arrange(reds_tile,
                                whites_tile,
                                ncol = 1)

ggsave("comb_tile.svg", plot = comb_tile,
       width = 190, height = 350, units = "mm",
       device = svg, dpi=600)

### Tile graphs (filtering by surface area)

reds_filtered <- cali_var %>% 
  filter(color == "Red") %>% filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>% filter(acres >= 1000) %>% 
  mutate(var = reorder(var, acres_thousand))

reds_filt_tile <- reds_filtered %>% ggplot(aes(year, var, fill = acres_thousand)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Purples"), trans = "sqrt")+
  xlab("Year") +
  ylab("Variety")

whites_filtered <- cali_var %>% 
  filter(color == "White") %>% filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>% filter(acres >= 1000) %>% 
  mutate(var = reorder(var, acres_thousand))

whites_filt_tile <- whites_filtered %>% ggplot(aes(year, var, fill = acres_thousand)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Greens"), trans = "sqrt") +
  xlab("Year") +
  ylab("Variety")

comb_filt_tile <- grid.arrange(reds_filt_tile,
                          whites_filt_tile,
                          ncol = 1)

ggsave("comb_filt_tile.svg", plot = comb_filt_tile,
       width = 190, height = 350, units = "mm",
       device = svg, dpi=600)



## Rankings of top varietals

var_order <- cali_var %>% 
  filter(var != "Other Red Wine") %>%
  filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(acres_per_tot)) %>%
  group_by(year) %>% top_n(n = 5, wt = acres_per_tot)

bar_order <- c("Chardonnay",
               "Cabernet Sauvignon",
               "Merlot",
               "Mission",
               "Alicante Bouschet",
               "Grenache",
               "Barbera",
               "Chenin Blanc",
               "Carignane",
               "French Colombard",
               "Pinot Noir",
               "Zinfandel")

top_5 <- var_order %>%
  ggplot(aes(year, acres_per_tot, fill=factor(var, levels = bar_order))) +
  xlab("Year") +
  ylab("Surface area (% of total)") +
  ylim(0,100) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="bottom") +
  geom_bar(stat = "identity")

## Explore cut-off values
cali_var %>% ggplot(aes(acres)) +
  geom_density() +
  scale_x_continuous(trans = 'log2')

cali_var %>% ggplot(aes(sample = scale(acres))) +
  geom_qq() +
  geom_abline()

cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  filter(acres >= 1000) %>%
  group_by(year) %>% summarize(total_per = sum(per_total)) %>%
  ggplot((aes(year, total_per))) +
  geom_line() +
  ylim(0,100)

## Percent make-up of n_varieties

var_1_tot <- cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(per_total)) %>%
  group_by(year) %>% top_n(n = 1, wt = per_total) %>% summarise(total_per_1 = sum(per_total))

var_3_tot <- cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(per_total)) %>%
  group_by(year) %>% top_n(n = 3, wt = per_total) %>% summarise(total_per_3 = sum(per_total))

var_5_tot <- cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(per_total)) %>%
  group_by(year) %>% top_n(n = 5, wt = per_total) %>% summarise(total_per_5 = sum(per_total))

var_10_tot <- cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(per_total)) %>%
  group_by(year) %>% top_n(n = 10, wt = per_total) %>% summarise(total_per_10 = sum(per_total))

var_15_tot <- cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(per_total)) %>%
  group_by(year) %>% top_n(n = 15, wt = per_total) %>% summarise(total_per_15 = sum(per_total))

var_20_tot <- cali_var %>% 
  filter(var != "Total Red Wine") %>% filter(var != "Other Red Wine") %>%
  filter(var != "Total White Wine") %>% filter(var != "Other White Wine") %>%
  arrange(desc(year), desc(per_total)) %>%
  group_by(year) %>% top_n(n = 20, wt = per_total) %>% summarise(total_per_20 = sum(per_total))

var_n_tot <- full_join(var_1_tot,
                       full_join(var_3_tot,
                       full_join(var_5_tot, 
                       full_join(var_10_tot ,
                                 full_join(var_15_tot, var_20_tot, by = "year"),
                                 by = "year"),
                                 by = "year"),
                                 by = "year"),
                                 by = "year")

var_n <- var_n_tot %>% ggplot()+
  geom_line(aes(year, total_per_20), color = "orange", size = 1.5) +
  geom_line(aes(year, total_per_15), color = "yellow", size = 1.5) +
  geom_line(aes(year, total_per_10), color = "purple", size = 1.5) +
  geom_line(aes(year, total_per_5), color = "green", size = 1.5) +
  geom_line(aes(year, total_per_3), color = "blue", size = 1.5) +
  geom_line(aes(year, total_per_1), color = "red", size = 1.5) +
  xlab("Year") +
  ylab("Surface area (% of total)") +
  ylim(0,100)

diversity <- grid.arrange(var_n,
                          top_5,
                               ncol = 1)

ggsave("diversity.svg", plot = diversity,
       width = 190, height = 280, units = "mm",
       device = svg, dpi=600)
