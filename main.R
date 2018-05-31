library(tidyverse)
library(magrittr)
library(readODS)
library(ggplot2) # Please install development version from Github: devtools::install_github("tidyverse/ggplot2")
library(sf)      # Please install development version from Github: devtools::install_github("r-spatial/sf")
library(lwgeom)

# Create output dirfolder 
dir.create("output", showWarnings = FALSE)

# Download municipality polygons from IBGE
mt_mu_url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/UFs/MT/MT.zip"
tmpfile = tempfile(fileext = ".zip")
download.file(url = mt_mu_url, destfile = tmpfile, quiet = TRUE)
unzip(zipfile = tmpfile, exdir = "./input/shp", overwrite = TRUE)
unlink(tmpfile)

# Read Municipality polygons 
mt_mu <- sf::read_sf("./input/shp/MT/51MUE250GC_SIR.shp")
mt_mu %>% dplyr::select(NM_MUNICIP) %>% 
  plot()

# List available crop calendars
mt_crop_calendars <- readODS::ods_sheets("./input/planting_dates_mt_brazil.ods")
mt_crop_calendars

# Read municipality concordance table
mu_concordance <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "concordance_ibge", col_names = TRUE) %>% 
  tibble::as_tibble()

# Create soybean spatiotemporal caledar 
mt_soybean_caledar <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "soy", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Soybean", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  st_set_precision(1000) %>% 
  lwgeom::st_make_valid() %>% 
  dplyr::group_by(start, end) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

mt_soybean_caledar %>% 
  plot()

# Create maize first crop spatiotemporal caledar 
mt_corn_1_caledar <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "corn_1st_crop", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Soybean", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  st_set_precision(1000) %>% 
  lwgeom::st_make_valid() %>% 
  dplyr::group_by(start, end) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

mt_corn_1_caledar %>% 
  plot()


# Create cotton spatiotemporal caledar 
mt_cotton_caledar <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "cotton", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Soybean", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mu_concordance, by = c("NM_MUNICIP" = "name_1")) %>% 
  dplyr::mutate(NM_MUNICIP = stringr::str_to_upper(ibge)) %>%
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  st_set_precision(1000) %>% 
  lwgeom::st_make_valid() %>% 
  dplyr::group_by(start, end) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

mt_cotton_caledar %>% 
  plot()

# Create beans first crop spatiotemporal caledar 
mt_beans_1_caledar <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "beans_1st_crop", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Soybean", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mu_concordance, by = c("NM_MUNICIP" = "name_1")) %>% 
  dplyr::mutate(NM_MUNICIP = stringr::str_to_upper(ibge)) %>%
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  st_set_precision(1000) %>% 
  lwgeom::st_make_valid() %>% 
  dplyr::group_by(start, end) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

mt_beans_1_caledar %>% 
  plot()

# Create beans second crop spatiotemporal caledar 
mt_beans_2_caledar <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "beans_2nd_crop", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Soybean", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mu_concordance, by = c("NM_MUNICIP" = "name_1")) %>% 
  dplyr::mutate(NM_MUNICIP = stringr::str_to_upper(ibge)) %>%
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  st_set_precision(1000) %>% 
  lwgeom::st_make_valid() %>% 
  dplyr::group_by(start, end) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

mt_beans_2_caledar %>% 
  plot()

