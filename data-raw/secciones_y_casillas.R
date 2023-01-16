library(tidyverse)
library(readxl)

cas <- readxl::read_xlsx(path = "data-raw/AYUNTAMIENTOS_xlsx/2021_SEE_AYUN_GTO_CAS.xlsx") %>% janitor::clean_names()

mun <- readxl::read_xlsx(path = "data-raw/AYUNTAMIENTOS_xlsx/2021_SEE_AYUN_GTO_MUN.xlsx") %>% janitor::clean_names()

muncan <- readxl::read_xlsx(path = "data-raw/AYUNTAMIENTOS_xlsx/2021_SEE_AYUN_GTO_MUNCAND.xlsx") %>% janitor::clean_names()

munp <- readxl::read_xlsx(path = "data-raw/AYUNTAMIENTOS_xlsx/2021_SEE_AYUN_GTO_MUNPP.xlsx") %>% janitor::clean_names()

sec <- readxl::read_xlsx(path = "data-raw/AYUNTAMIENTOS_xlsx/2021_SEE_AYUN_GTO_SEC.xlsx") %>% janitor::clean_names()

aux <- cas %>% select(municipio, seccion, casilla)

aux %>% group_by(municipio) %>% distinct(seccion) %>% count(seccion) %>%  summarise(secciones = sum(n)) %>%
  left_join(aux %>% group_by(municipio) %>% count(casilla) %>% summarise(casillas = sum(n)), by = "municipio") %>%
  summarise(across(!municipio, .fns = ~sum(.x)))
