# ----
# Datos de covid-19, Bolivia
#
# adaptado por Luis Miguel Senzano Castro
# Biologo, Departamento de Zoologia,
# Universidade Estadual Paulista - UNESP, SP, Brazil
# e-mail: luismiguelsenzanocastro@gmail.com
# ultima modificacion: 22/03/2020
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

# reportes diarios----

# reportes actualizados diariamente de: European Centre for Disease Prevention and Control (https://adv-r.hadley.nz/subsetting.html)

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

# Obtener el dataset

httr::GET(url, authenticate(":", ":", type="ntlm"), write_disk(df_covid <- file.path(fileext = "covid.xlsx")))

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

breaks=c(0, 10, 20, 30, 40, 50, 100, 200, 300, 400)

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

# Casos reportados

LP = 1
OR = 8
PT = 0
TA = 0
SC = 13
CH = 0
PA = 0
BE = 0
CB = 2

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
