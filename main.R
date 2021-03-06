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
mt_spatiotemporal_soybean <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "soy", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Soybean", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf()  %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Create cotton spatiotemporal caledar 
mt_spatiotemporal_cotton <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "cotton", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Cotton", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mu_concordance, by = c("NM_MUNICIP" = "name_1")) %>% 
  dplyr::mutate(NM_MUNICIP = stringr::str_to_upper(ibge)) %>%
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Create maize first crop spatiotemporal caledar 
mt_spatiotemporal_maize_1st <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "corn_1st_crop", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Maize 1st crop", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Create maize second crop spatiotemporal caledar 
mt_spatiotemporal_maize_2nd <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "corn_2nd_crop", col_names = TRUE, skip = 1, range = c("A1:A142")) %>% 
  tibble::tibble(Nome_Municipio = .) %>%
  dplyr::bind_cols(readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "corn_2nd_crop", col_names = TRUE, skip = 1, range = c("D1:E142"))) %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), label = "Maize 2nd crop", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Create maize second pasture spatiotemporal caledar 
mt_spatiotemporal_maize_pasture <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "Corn_2nd_pasture", col_names = TRUE, range = c("A1:A141")) %>% 
  tibble::tibble(Nome_Municipio = .) %>%
  dplyr::bind_cols(readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "Corn_2nd_pasture", col_names = TRUE, range = c("D1:E141"))) %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), label = "Maize 2nd pasture", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Create beans first crop spatiotemporal caledar 
mt_spatiotemporal_beans_1st <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "beans_1st_crop", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Beans 1st crop", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mu_concordance, by = c("NM_MUNICIP" = "name_1")) %>% 
  dplyr::mutate(NM_MUNICIP = stringr::str_to_upper(ibge)) %>%
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Create beans second crop spatiotemporal caledar 
mt_spatiotemporal_beans_2nd <- readODS::read_ods("./input/planting_dates_mt_brazil.ods", sheet = "beans_2nd_crop", col_names = TRUE, skip = 1, range = "A1:D141") %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(Cod_Munic != "Total Result") %>% 
  dplyr::transmute(NM_MUNICIP = stringr::str_to_upper(Nome_Municipio), start = `Min - start_date`, end = `Max - end_date`) %>% 
  dplyr::mutate(label = "Beans 2nd crop", start = lubridate::mdy(start), end = lubridate::mdy(end)) %>% 
  dplyr::right_join(mu_concordance, by = c("NM_MUNICIP" = "name_1")) %>% 
  dplyr::mutate(NM_MUNICIP = stringr::str_to_upper(ibge)) %>%
  dplyr::right_join(mt_mu, by = c("NM_MUNICIP" = "NM_MUNICIP")) %>% 
  sf::st_as_sf() %>% 
  dplyr::select(label, start, end) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid()  

# Join calendars to one spatiotemporal object and group by label, starting month, and ending month
mt_spatiotemporal_calendar <- rbind(mt_spatiotemporal_soybean, 
      mt_spatiotemporal_cotton,
      mt_spatiotemporal_maize_1st,
      mt_spatiotemporal_maize_2nd,
      mt_spatiotemporal_maize_pasture,
      mt_spatiotemporal_beans_1st,
      mt_spatiotemporal_beans_2nd) %>% 
  sf::st_set_precision(10000) %>% 
  lwgeom::st_make_valid() %>% 
  dplyr::mutate(month_start = as.integer(lubridate::month(start)), 
                month_end   = as.integer(lubridate::month(end))) %>% 
  dplyr::group_by(label, month_start, month_end) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

# Save results to file 
sf::write_sf(mt_spatiotemporal_calendar, dsn = "./output/mt_spatiotemporal_calendar.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# Plot starting months by crop type 
gp <- mt_spatiotemporal_calendar %>% 
  dplyr::mutate(Range = stringr::str_c(month.abb[month_start], "-", month.abb[month_end])) %>% 
  ggplot2::ggplot(aes(group = label, fill = Range)) +
  ggplot2::facet_wrap(~label, nrow = 2) +
  ggplot2::geom_sf(size = 0.1) +
  ggplot2::theme(legend.position = "bottom")

gp

ggsave(filename = "./output/mt_spatiotemporal_calendar.png", plot = gp, device = "png", 
       width = 15, height = 9, dpi = 300)

