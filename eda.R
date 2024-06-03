################################################################################
# 
# Example project exploratory data analysis and investigation
# Created by: Natalie Smith
# 
################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(gt)
library(webshot2)
library(palmerpenguins)
library(MetBrewer)
library(clock)
# install.packages('modeldata')


# Investigate data -------------------------------------------------------------

head(penguins)
head(penguins_raw)

colnames(penguins_raw)

summary(penguins)

# Tables -----------------------------------------------------------------------

penguins |> 
  group_by(species) |> 
  summarise(penguin_count = n()) |> 
  arrange(across(penguin_count, desc)) |> 
  gt(rowname_col = "species") |> 
  tab_header(
    title = "Count of Penguins by Species",
    subtitle = "Collected from 3 islands in the Palmer Archipelago, Antarctica"
  ) |> 
  cols_label(
    penguin_count = html("Count") 
  ) |>   
  tab_stubhead(label = "Species") |> 
  cols_align(
    align = "left",
    columns = species
  ) |> 

gtsave('penguins.png')

# Visualisations ---------------------------------------------------------------

## Scatterplot -----------------------------------------------------------------

colorblind.friendly(c('Java'))

ggplot(penguins, aes(bill_depth_mm, bill_length_mm, color = species, fill = species)) +
  geom_point(alpha = 0.25) +
  geom_smooth() +
  scale_fill_manual(values = met.brewer("Java", 3, override.order = FALSE, direction = -1)) + #Archambault Kandinsky Isfahan1 Isfahan2 Hiroshige Java Homer1 Navajo Klimt Pissaro Lakota Nizami Renoir Hokusai1 
  scale_color_manual(values = met.brewer("Java", 3, override.order = FALSE, direction = -1)) +
  theme_minimal(base_size = 14
                , base_line_size = 14 / 22
                , base_rect_size = 14 / 22) +
  guides(color = guide_legend(reverse = TRUE)
         , fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid = element_line(colour = 'grey90')
        # , panel.border = element_blank()
        , legend.position = 'top'
        , axis.ticks = element_blank()
        , plot.title = element_text(colour = 'black', hjust = 0)
        , plot.title.position = 'plot'
        , plot.subtitle = element_text(colour = 'grey45')
        , axis.title = element_text(colour = 'grey20')
        , axis.text = element_text(colour = 'grey20')
        , axis.line = element_line(colour = 'grey30')
        ) +
  labs(title = str_wrap("Penguins with a larger bill depth generally have larger bill length", 70)
       , subtitle = str_wrap("This is consistent across species", 70)
       , x = 'Bill Depth (mm)'
       , y = 'Bill Length (mm)'
       , color = 'Species'
       , fill = 'Species')

## Bar Chart -------------------------------------------------------------------

ggplot(penguins, aes(x = island)) +
  geom_bar(fill = '#62205f') +
  theme_minimal(base_size = 14
                , base_line_size = 14 / 22
                , base_rect_size = 14 / 22) +
  theme(panel.grid = element_blank()
        , panel.border = element_blank()
        , legend.position = 'top'
        , axis.ticks = element_blank()
        , plot.title = element_text(colour = 'black', hjust = 0)
        , plot.title.position = 'plot'
        , plot.subtitle = element_text(colour = 'grey45')
        , axis.title = element_text(colour = 'grey20')
        , axis.text = element_text(colour = 'grey20')
        , axis.line = element_line(colour = 'grey30')
  ) +
  labs(title = str_wrap("Fewer observations were taken at Torgersen Island", 70)
       , x = 'Island'
       , y = 'Count of Pengins')

## Line Graph ------------------------------------------------------------------

colorblind.friendly("Hiroshige")

penguins |> 
  count(year, island) |> 
ggplot(aes(x = year, y = n, fill = island, color = island)) + 
  geom_point() + 
  geom_line(size = 1) +
  scale_fill_manual(values = met.brewer("Hiroshige", 3, override.order = FALSE, direction = -1)) + #Archambault Kandinsky Isfahan1 Isfahan2 Hiroshige Java Homer1 Navajo Klimt Pissaro Lakota Nizami Renoir Hokusai1 
  scale_color_manual(values = met.brewer("Hiroshige", 3, override.order = FALSE, direction = -1)) +
  geom_vline(xintercept = 2008, linetype = 'dashed', color = 'grey45') +
  annotate("text", x = 2008.4, y = 55, label = "Change in observations", , color = 'grey45') +
  scale_x_continuous(breaks=2007:2009) +
  theme_minimal(base_size = 14
                , base_line_size = 14 / 22
                , base_rect_size = 14 / 22) +
  theme(panel.grid = element_blank()
        , panel.border = element_blank()
        , axis.ticks = element_blank()
        , plot.title = element_text(colour = 'black', hjust = 0)
        , plot.title.position = 'plot'
        , plot.subtitle = element_text(colour = 'grey45')
        , axis.title = element_text(colour = 'grey20')
        , axis.text = element_text(colour = 'grey20')
        , axis.line = element_line(colour = 'grey30')
  ) +
  labs(title = str_wrap("Observations appear to be shifted from Dream Island to Biscoe Island in 2008", 70)
       , x = 'Year'
       , y = 'Count of Pengins'
       , color = 'Island'
       , fill = 'Island')

## Fancier Bar Chart -----------------------------------------------------------

species_by_year <- penguins |> 
  count(species, year) |> 
  group_by(year) |> 
  mutate(var_pct = n / sum(n)
         , species = factor(species)
         , year = factor(year, c(2009, 2008, 2007)))

species_levels <-  species_by_year |> 
  filter(year == 2007) |> 
  arrange(var_pct) |> 
  pull(species)
  
species_by_year |> 
  mutate(species = factor(species, species_levels)) |> 
  ungroup() |> 
  ggplot(aes(year, var_pct, fill = species)) +
  geom_col(position = 'stack', color = NA) + 
  scale_fill_manual(values = rev(met.brewer("Isfahan1", 3, override.order = TRUE, direction = -1))) + #Archambault Kandinsky Isfahan1 Isfahan2 Hiroshige Java Homer1 Navajo Klimt Pissaro Lakota Nizami Renoir Hokusai1 
  scale_color_manual(values = rev(met.brewer("Isfahan1", 3, override.order = TRUE, direction = -1))) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(), n.breaks = 6) +
  theme_minimal(base_size = 14
                , base_line_size = 14 / 22
                , base_rect_size = 14 / 22) +
  theme(panel.grid = element_blank()
        , panel.border = element_blank()
        , axis.ticks = element_blank()
        , plot.title = element_text(colour = 'black', hjust = 0)
        , legend.position = 'top'
        , plot.title.position = 'plot'
        , plot.subtitle = element_text(colour = 'grey45')
        , axis.title = element_text(colour = 'grey20')
        , axis.text = element_text(colour = 'grey20')
  ) +
  labs(title = str_wrap("Adelie penguins are the most observed in each year of this study", 70)
       , x = 'Year'
       , y = 'Proportion of Pengins'
       , color = 'Species'
       , fill = 'Species')
