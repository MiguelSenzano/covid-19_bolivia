# ----
# Datos de covid-19, Bolivia
#
# adaptado por Luis Miguel Senzano Castro
# Biologo, Departamento de Zoologia,
# Universidade Estadual Paulista - UNESP, SP, Brazil
# e-mail: luismiguelsenzanocastro@gmail.com
# ultima modificacion: 22/05/2020
# ----


# Working directory

setwd("")


# required packages
library(httr)
library(tidyverse)
library(readxl)
library(nCov2019)
require(shadowtext)
require(rnaturalearth)
require(RColorBrewer)
require(ggpubr)
library(gganimate)
library(janitor)
library(gifski)

# reportes diarios----

# reportes actualizados diariamente de: European Centre for Disease Prevention and Control (https://www.ecdc.europa.eu/en)

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

# Obtener el dataset

httr::GET(url, authenticate(":", ":", type="ntlm"), write_disk(df_covid <- file.path(fileext = "covid.xlsx"),overwrite=TRUE))

covid <- read_excel(df_covid)
names(covid) <- gsub(" ", "_", names(covid))

covid.bo <- dplyr:: filter(covid, Countries_and_territories == "Bolivia")
covid.bo$DateRep = as.Date(covid.bo$DateRep, "%Y.%m.%d")

cases_death = data.frame(
  group = c(rep("cases", nrow(covid.bo)), rep("deaths", nrow(covid.bo))),
  date = rep(covid.bo$DateRep, 2),
  x = rep(covid.bo$Day, 2),
  y = c(covid.bo$Cases, - covid.bo$Deaths),
  stringsAsFactors = F
)

# plot

covid.date <- ggplot(cases_death, aes(x = reorder(x, date), y = y, fill = group)) + 
  geom_bar(stat="identity", position="identity") +
  scale_fill_manual(values = c("cases" = "mediumseagreen", "deaths" = "indianred1")) +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15)) +
  labs(x="Dias/Marzo", y="Numero de casos confirmados", title = "Casos diarios reportados") +
  theme_bw()

covid.date


### Evolucion acumulada----

y <- load_nCov2019(lang = 'en', source='github')
d = y['global'] %>% 
  as_tibble %>%
  rename(confirm=cum_confirm) %>%
  filter(confirm > 0 & country == "Bolivia") %>%
  mutate(days_since_0 = as.numeric(time - min(time))) %>%
  ungroup

breaks=c(0, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000)

covid.evol <- ggplot(d, aes(days_since_0, confirm, color = country)) +
  geom_smooth(color='grey50', linetype='dashed') +
  geom_line(size = 1) +
  geom_point(pch = 21, size = 3) +
  scale_y_log10(expand = c(0,0.1), 
                breaks = breaks, labels = breaks) +
  scale_x_continuous(expand = c(0,1)) +
  theme_minimal(base_size = 14) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,20,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  geom_shadowtext(aes(label = paste0(" ",time)), hjust=0, vjust = 0, 
                  data = . %>% top_n(1, days_since_0), 
                  bg.color = "white") +
  labs(x = "Numero de dias desde el primer caso", y = "Numero de casos", 
       title = "Casos COVID-19 confirmados",
       subtitle = time(y))

covid.evol

# Por departamentos----

# Casos reportados (fuente: https://www.boliviasegura.gob.bo/datos-oficiales/)

LP = 363
OR = 138
PT = 40
TA = 14
SC = 3241
CH = 19
PA = 12
BE = 833
CB = 259

cases <- c(LP,OR,PT,TA,SC,CH,PA,BE,CB)
name <- c("La Paz","Oruro","Potosí","Tarija","Santa Cruz","Chuquisaca","Pando","El Beni","Cochabamba")
cov <- data.frame(name,cases)

# map

bol <- rnaturalearth::ne_states(country = 'Bolivia', returnclass = "sf")

bol.cov <- left_join(bol, cov)
bol.cov$cases

# Plot bolivia

map <- ggplot(bol.cov, aes(fill = cases)) +
  geom_sf(data = bol) +
  labs(x = "", y = "", fill = "Casos") +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "OrRd"))+
  theme_bw() +
  theme(title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.background = element_rect(fill = "white",
                                         size = 0.5, 
                                         linetype = "solid", 
                                         colour = "black"),
        axis.title = element_text(size = 15, face = "plain"),
        legend.position = c(.85, .8))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

map <- map + geom_text(data = bol.cov, aes(x = longitude, y = latitude, label = cases), cex = 5, col = "black")
map

## juntos

g_covid <- ggarrange(map, labels="Casos confirmados por departamento",
                     ggarrange(covid.date, covid.evol, ncol = 1,labels = c("", "")),
                     nrow = 1, align = "h")

annotate_figure(g_covid)


# Evolution temporal interactiva----

cov_dpto <- read.delim("cov_depto.txt", sep = "\t", header = TRUE)
cov_formatted <- cov_dpto %>%
  group_by(day) %>%
  mutate(rank = rank(-value)) %>%
  group_by(dpto_name) %>% 
  ungroup()

# plot

staticplot = ggplot(cov_formatted, aes(rank, group = dpto_name, 
                                       fill = as.factor(dpto_name), color = as.factor(dpto_name))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(dpto_name, " ")), vjust = 0.2, hjust = 1, size = 6) +
  geom_text(aes(y=value, label = paste(value, " "), hjust=0), size = 6.5) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=27, hjust=0.5, face="bold", colour="grey40", vjust=-1),
        plot.subtitle=element_text(size=20, hjust=0.5, face="italic", color="grey40"),
        plot.caption =element_text(size=15, hjust=0.5, face="italic", color="grey40"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

# interactive

anim = staticplot + transition_states(day, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Cov-19 por Fecha : {closest_state}',  
       subtitle  =  "Casos por departamentos",
       caption  = "casos confirmados | compilados de: Bolivia Segura (www.boliviasegura.gob.bo/datos-oficiales/)")

# export (GIF)

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("CovDpto.gif"))

##--- fin
