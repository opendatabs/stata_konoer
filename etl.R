library(data.table)
library(zoo)
library(lubridate)
library(tidyverse)
library(httr)

get_dataset <- function(url, output_file = "100120.csv") {
  # Create directory if it does not exist
  data_path <- file.path('data_orig')
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }

  # Define the path for the downloaded file
  csv_path <- file.path(data_path, output_file)

  # Download the file using httr::GET
  response <- GET(url, write_disk(csv_path, overwrite = TRUE))

  # Check if the request was successful
  if (http_type(response) != "text/csv" && status_code(response) != 200) {
    stop("Failed to download the file. Check the URL or proxy settings.")
  }

  # Read the CSV file
  data <- tryCatch(
    read.csv(csv_path, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8"),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  # If the dataframe only has one column or less, the data is not ";" separated
  if (is.null(data) || ncol(data) <= 1) {
    stop("The data wasn't imported properly. Very likely the correct separator couldn't be found.\nPlease check the dataset manually and adjust the code.")
  }

  return(data)
}


pathBussen <- "data_orig/ordnungsbussen/Ordnungsbussen_OGD_all.csv"
pathWildeDeponien <- "data_orig/wildedeponien/wildedeponien_all.csv"
urlStrassenverkehr <- "https://data.bs.ch/explore/dataset/100120/download?format=csv&timezone=Europe%2FZurich"
pathSprayereien <- "data_orig/sprayereien/sprayereien.csv"
pathRequisition <- "data_orig/requisitionen/Requisitionen.csv"
pathAllmendbewilligungen <- "data_orig/allmend/sbt_allmend_centroids.csv"

# ----------------- Wilde Deponien -----------------
data_deponien <- fread(pathWildeDeponien, header = TRUE)
data_deponien_new <- data_deponien %>%
  select(-id) %>%
  mutate(abfallkategorie = ifelse(abfallkategorie=="", "Unbekannt", abfallkategorie))%>%
  # mutate(geometry = gsub(".*\\(", "", geometry),
  #        geometry = gsub("\\)", "", geometry)) %>%
  # separate_wider_delim(col=geometry, delim = " ", names = c("longitude","latitude")) %>%
  rename(incident_type_primary = abfallkategorie) %>%
  mutate(parent_incident_type = "Wilde Deponien",
         incident_date = lubridate::date(data_deponien$bearbeitungszeit_meldung),
         year = lubridate::year(incident_date),
         month = lubridate::month(incident_date),
         day_of_week_nr = lubridate::wday(incident_date),
         day_of_week = lubridate::wday(incident_date, label = TRUE, abbr = FALSE),
         hour_of_day = lubridate::hour(bearbeitungszeit_meldung))  %>%
  rownames_to_column(var = "id") %>%
  mutate(x = pmap(.l = list(lon,lat),.f = function(lon,lat,...){
    eRTG3D::transformCRS.3d(data.frame(x = lon, y = lat, z = 260), fromCRS=2056, toCRS=4326)
  }))%>% 
  unnest(x) %>%
  rename(longitude = x, latitude =y)%>%
  select(id,incident_date,year, month,day_of_week_nr,day_of_week,hour_of_day,longitude,latitude,parent_incident_type,incident_type_primary)

# data_deponien_new  %>% View()
#   select(id,incident_date,year, month,day_of_week_nr,day_of_week,hour_of_day,lon,lat ,parent_incident_type,incident_type_primary)
# transformationLonLat <- eRTG3D::transformCRS.3d(as.data.frame(data_deponien_new %>% select(x=lon, y=lat) %>% 
#                                                mutate(z = rep(260, times =dim(data_deponien_new)[1]))), fromCRS=2056, toCRS=4326)%>%
#   rownames_to_column(var = "id")
# data_deponien_new <- data_deponien_new %>% left_join(transformationLonLat %>% select(id, longitude=x , latitude=y), by= join_by(id==id))
# 
write.csv(data_deponien_new,file = "data/data_wildeDeponien.csv", fileEncoding = "UTF-8", row.names = FALSE)

# ----------------- Ordnungsbussen -----------------
#Ordnungsbussen select = c(1,5,6,7,12,19,20)
data_bussen <-fread(pathBussen, header = TRUE)
data_bussen_new <- data_bussen %>%
  rename(incident_type_primary = BuZi,
         incident_type_primary_text = "BuZi Text", 
         vehicle_typ = "KAT BEZEICHNUNG",
         year = "Übertretungsjahr", 
         month = "Übertretungsmonat", 
         latitude = "GPS Breite", 
         longitude = "GPS Länge", 
         day_of_week_nr = "ÜbertretungswochentagNummer", 
         day_of_week = Übertretungswochentag, 
         id = Laufnummer) %>%
  mutate(parent_incident_type = "Ordnungsbussen",
         incident_date =lubridate::ymd(data_bussen$Übertretungsdatum),
         hour_of_day = NA) %>% 
  select(id,incident_date,year,month,day_of_week_nr, day_of_week,hour_of_day,longitude,latitude,parent_incident_type, incident_type_primary,vehicle_typ) %>% 
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude))
print(head(data_bussen_new))
write.csv(data_bussen_new,file = "data/data_Ordnungsbussen.csv", fileEncoding = "UTF-8", row.names = FALSE)

# ----------------- Strassenverkehr -----------------
data_strassenverkehr <- get_dataset(urlStrassenverkehr)
nm <- names(data_strassenverkehr)
# Map both schemas to the same working names used downstream
if ("id_unfall" %in% nm) {
  map <- list(
    id = "id_unfall",
    incident_type_primary = "typ",
    x_temp = "schwere",
    year = "jahr",
    month = "monat",
    hour_of_day = "stunde"
  )
  wday_col <- "wochentag"
} else {
  map <- list(
    id = "gml_id",
    incident_type_primary = "vu_typ",
    x_temp = "vu_schwerekategorie",
    year = "vu_jahr",
    month = "vu_monat",
    hour_of_day = "vu_stunde"
  )
  wday_col <- "vu_wochentag"
}

data_strassenverkehr_new <- data_strassenverkehr %>%
  dplyr::rename(!!!map) %>%
  # lat/lon from geo_point_2d remain the same
  tidyr::separate_wider_delim(col = "geo_point_2d", delim = ",",
                              names = c("latitude","longitude")) %>%
  tidyr::separate_wider_delim(col = all_of(wday_col), delim = " ",
                              names = c("day_of_week_nr","day_of_week")) %>%
  dplyr::mutate(
    parent_incident_type = "Verkehrsunfälle",
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude),
    incident_date = lubridate::ymd(paste(year, month, "1", sep = "-"))
  ) %>%
  rowwise() %>%
  mutate(x = {
    tmp <- stringr::str_split(x_temp, pattern = " ", n = 2)[[1]]
    paste(tmp[1], tmp[2], sep = ",")
  }) %>%
  ungroup() %>%
  tidyr::separate_wider_delim(col = "x", delim = ",",
                              names = c("incident_type_secondary_nr","incident_type_secondary")) %>%
  dplyr::select(
    id, incident_date, year, month,
    day_of_week_nr, day_of_week, hour_of_day,
    longitude, latitude,
    parent_incident_type, incident_type_primary,
    incident_type_secondary_nr, incident_type_secondary
  )

write.csv(data_strassenverkehr_new, file = "data/data_strassenverkehr.csv",
          fileEncoding = "UTF-8", row.names = FALSE)

write.csv(
  data_strassenverkehr_new %>%
    dplyr::select(parent_incident_type, incident_type_primary,
                  incident_type_secondary_nr, incident_type_secondary) %>%
    dplyr::distinct(),
  file = "data/data_strassenverkehrziffern.csv",
  fileEncoding = "UTF-8", row.names = FALSE
)

# ----------------- Requisitionen -----------------
data_requis <- fread(pathRequisition, header = TRUE, encoding = "Latin-1")

data_requis_new <- data_requis %>%
  rename(
    id = EINSAETZE_KEY,
    incident_type_primary   = EreignistypKlasse,
    incident_type_secondary = Ereignistyp
  ) %>%
  mutate(
    parent_incident_type = "Requisitionen",

    # Einsatzdatum_key is YYYYMMDD (numeric or character)
    incident_date = as.Date(as.character(Einsatzdatum_key), format = "%Y%m%d"),
    year  = lubridate::year(incident_date),
    month = lubridate::month(incident_date),

    day_of_week = lubridate::wday(incident_date, label = TRUE, abbr = FALSE),
    day_of_week_nr = lubridate::wday(incident_date),

    # Parse times like "HM" or "HMS"
    hour_of_day = lubridate::hour(lubridate::parse_date_time(Einsatzzeit, orders = c("HMS", "HM")))
  ) %>%
  filter(!is.na(OriginalKoordinateX) & !is.na(OriginalKoordinateY)) %>%
  mutate(
    x = purrr::pmap(
      .l = list(OriginalKoordinateX, OriginalKoordinateY),
      .f = function(x, y, ...) {
        eRTG3D::transformCRS.3d(data.frame(x = x, y = y, z = 260), fromCRS = 2056, toCRS = 4326)
      }
    )
  ) %>%
  tidyr::unnest(x) %>%
  rename(longitude = x, latitude = y)

data_requis_export <- data_requis_new %>% filter(OriginalKoordinateX != 2000000)

write.csv(
  data_requis_export %>%
    select(id, incident_date, year, month, day_of_week_nr, day_of_week, hour_of_day,
           longitude, latitude, parent_incident_type, incident_type_primary, incident_type_secondary),
  file = "data/data_requisitionen.csv",
  fileEncoding = "UTF-8",
  row.names = FALSE
)


# ----------------- Allmend -----------------
data_allmend <- fread(pathAllmendbewilligungen, header = TRUE)
head(data_allmend)
data_allmend_export <- data_allmend %>%
  select(latitude = centroid_y, longitude = centroid_x,BegehrenID, incident_type_secondary= Bezeichnung, incident_type_primary = "Entscheid-Bezeichnung",  incident_date = Datum_von, incident_date_to = Datum_bis) %>%
  mutate(
    parent_incident_type =  "Allmend",
    year = lubridate::year(incident_date),
    year_to = lubridate::year(incident_date_to),
    month = lubridate::month(incident_date),
    month_to = lubridate::month(incident_date_to),
    day_of_week_nr = lubridate::wday(incident_date),
    day_of_week = lubridate::wday(incident_date, label = TRUE, abbr = FALSE), 
    day_of_week_nr_to = lubridate::wday(incident_date_to),
    day_of_week_to = lubridate::wday(incident_date_to, label = TRUE, abbr = FALSE), 
    hour_of_day = NA,
    incident_type_primary = ifelse(incident_type_primary %in% c("", "<unbekannt>"), "unbekannt", incident_type_primary),
    ) %>%
  select(incident_date,year,month,BegehrenID,day_of_week_nr, day_of_week,hour_of_day,incident_date_to, year_to, month_to,day_of_week_nr_to,day_of_week_to,longitude,latitude,parent_incident_type, incident_type_primary,incident_type_secondary) %>% 
  arrange(incident_type_primary)
data_allmend_export$id <- seq(nrow(data_allmend_export))

write.csv(data_allmend_export,
          file = "data/data_allmend.csv", fileEncoding = "UTF-8", row.names = FALSE)

# ----------------- Sprayereien -----------------
spray <- read.csv(pathSprayereien)

spray$date <- spray$erfassungszeit

# Konvertieren der incident_date-Spalte in ein POSIXct-Objekt
spray$date_char <- ymd_hms(spray$date, tz = "UTC")

# Umwandeln in ein Date-Objekt (nur Datum)
spray$incident_date <- as.Date(spray$date_char)

# Formatieren des Datums im Format dd.mm.yyyy
#spray$incident_date <- format(spray$date, format = "%d.%m.%Y")

# Erzeugen der neuen Spalten
spray$year <- year(spray$incident_date)
spray$month <- month(spray$incident_date)
spray$day_of_week_nr <- wday(spray$incident_date)
spray$day_of_week <- weekdays(spray$incident_date)  # Voller Name des Wochentags
spray$hour_of_day <- hour(spray$date_char)


# Funktion zur Extraktion von Longitude und Latitude
extract_coordinates <- function(point_string) {
  # Verwende reguläre Ausdrücke, um die Koordinaten zu extrahieren
  matches <- regmatches(point_string, regexec("POINT \\(([^ ]+) ([^ )]+)\\)", point_string))
  coords <- unlist(matches)[-1]
  if (length(coords) == 2) {
    return(as.numeric(coords))
  } else {
    return(c(NA, NA))
  }
}

# Wende die Funktion auf die 'geometry'-Spalte an und erstelle neue Spalten
coordinates <- t(sapply(spray$geometry, extract_coordinates))
colnames(coordinates) <- c("longitude", "latitude")
spray <- cbind(spray, coordinates)

library(tidyr)
library(dplyr)
spray <- spray %>%
  rename(incident_type_primary = spray_typ) %>%
  mutate(incident_type_primary = recode(incident_type_primary,
                                "sprayout" = "Spray-Out",
                                "sprayex" = "Spray-Ex"))%>%
  mutate(parent_incident_type = "Sprayereien")

spray <- dplyr::select(spray, id, incident_date, year, month, day_of_week_nr, day_of_week, hour_of_day, longitude, latitude, parent_incident_type, incident_type_primary)

# Pfad zur CSV-Datei
output_file <- "data/data_sprayereien.csv"

# Exportieren des DataFrames als CSV-Datei
write.csv(spray, file = output_file, row.names = FALSE)

# ----------------- Metadaten -----------------
# Last but not least, get the Metadata
urlMetadata <- "https://data.bs.ch/explore/dataset/100057/download/?format=csv&timezone=Europe/Zurich&use_labels=true"
metadata <- get_dataset(urlMetadata, output_file = "100057.csv")
# TODO: Check if some processing is necessary
write.csv(metadata, file = "data/data_metadata.csv", row.names = FALSE)
