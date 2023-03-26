# Preámbulo ---------------------------------------------------------------

library(janitor)
library(readxl)
library(scales)
library(tidyr)
library(dplyr)

# Cargar bases de datos ---------------------------------------------------

ensambles <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 1) %>%
  janitor::clean_names()

cpus <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 2) %>%
  janitor::clean_names()

motherboards <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 3) %>%
  janitor::clean_names()

almacenamiento <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 4) %>%
  janitor::clean_names()

ram <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 5) %>%
  janitor::clean_names()

fuente_de_poder <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 6) %>%
  janitor::clean_names()

disipador <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 7) %>%
  janitor::clean_names()

gpu <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 8) %>%
  janitor::clean_names()

pasta <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 9) %>%
  janitor::clean_names()

gabinetes <- readxl::read_xlsx(path = "data-raw/Presupuesto PC.xlsx", sheet = 10) %>%
  janitor::clean_names()

# Procesamiento -----------------------------------------------------------

precios <- ensambles %>%
  left_join(cpus %>% select(id, precio_cpu = precio), by = c("id_cpu" = "id")) %>% select(!id_cpu) %>%
  left_join(motherboards %>% select(id, precio_motherboards = precio), by = c("id_motherboard" = "id")) %>% select(!id_motherboard) %>%
  left_join(almacenamiento %>% select(id, precio_almacenamiento = precio), by = c("id_almacenamiento" = "id")) %>% select(!id_almacenamiento) %>%
  left_join(ram %>% select(id, precio_ram = precio), by = c("id_ram" = "id")) %>% select(!id_ram) %>%
  left_join(fuente_de_poder %>% select(id, precio_fuente_de_poder = precio), by = c("id_fuente_de_poder" = "id")) %>% select(!id_fuente_de_poder) %>%
  left_join(disipador %>% select(id, precio_disipador = precio), by = c("id_disipador" = "id")) %>% select(!id_disipador) %>%
  left_join(gpu %>% select(id, precio_gpu = precio), by = c("id_gpu" = "id")) %>% select(!id_gpu) %>%
  left_join(pasta %>% select(id, precio_pasta = precio), by = c("id_pasta_termica" = "id")) %>% select(!id_pasta_termica) %>%
  left_join(gabinetes %>% select(id, precio_gabinete = precio), by = c("id_gabinete" = "id")) %>% select(!id_gabinete)

calcular_costo_ensamble <- function(){
  precios %>% pivot_longer(cols = !ensamble, names_to = "componente",
                                                          names_prefix = "precio_",   values_to = "precio") %>%
    group_by(ensamble) %>%
    summarise(total = sum(precio, na.rm = T)) %>% arrange(desc(total))
}

desplegar_calculo <- function(){
  ensambles %>%
    left_join(cpus %>% mutate(cpu = paste(fabricante, modelo)) %>% select(id, cpu), by = c("id_cpu" = "id")) %>% select(!id_cpu) %>%
    left_join(motherboards %>% mutate(motherboard = paste(fabricante, modelo)) %>% select(id, motherboard), by = c("id_motherboard" = "id")) %>% select(!id_motherboard) %>%
    left_join(almacenamiento %>% mutate(almacenamiento = paste(fabricante, modelo)) %>% select(id, almacenamiento), by = c("id_almacenamiento" = "id")) %>% select(!id_almacenamiento) %>%
    left_join(ram %>% mutate(ram = paste(fabricante, modelo)) %>% select(id, ram), by = c("id_ram" = "id")) %>% select(!id_ram) %>%
    left_join(fuente_de_poder %>% mutate(fuente_de_poder = paste(fabricante, modelo)) %>% select(id, fuente_de_poder), by = c("id_fuente_de_poder" = "id")) %>% select(!id_fuente_de_poder) %>%
    left_join(disipador %>% mutate(disipador = paste(fabricante, modelo)) %>% select(id, disipador), by = c("id_disipador" = "id")) %>% select(!id_disipador) %>%
    left_join(gpu %>% mutate(gpu = paste(fabricante, modelo)) %>% select(id, gpu), by = c("id_gpu" = "id")) %>% select(!id_gpu) %>%
    left_join(pasta %>% mutate(pasta = paste(fabricante, modelo)) %>% select(id, pasta), by = c("id_pasta_termica" = "id")) %>% select(!id_pasta_termica) %>%
    left_join(gabinetes %>% mutate(gabinete = paste(fabricante, modelo)) %>% select(id, gabinete), by = c("id_gabinete" = "id")) %>% select(!id_gabinete) %>%
    left_join(calcular_costo_ensamble(), by = "ensamble") %>%
    arrange(desc(total)) %>%
    mutate(total = paste("$", scales::comma(total), sep = " ")) %>%
    View()
}

# Resultado ---------------------------------------------------------------

desplegar_calculo()


