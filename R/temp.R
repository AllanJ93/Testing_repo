library(tidyverse)
library(readr)
library(here)

data <- as_tibble(read.csv("ITER_07CSV20.csv", fileEncoding = "UTF-8"))

data %>% filter(!grepl("Total", x = NOM_LOC), !grepl("Total", x = NOM_MUN)) %>% count(NOM_MUN, NOM_LOC, wt = POBTOT, sort = T) %>% 
  top_n(20) %>% write_excel_csv("Chiapas20.csv")





top20 <- data %>% slice(5:tot) %>% arrange(desc(POBTOT)) %>%  filter(NOM_LOC != "Total del Municipio") %>% slice_head(n = 20)

write_excel_csv(top20,"Chiapas20MasPoblados.csv")

