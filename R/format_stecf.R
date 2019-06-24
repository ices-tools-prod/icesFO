#' Format the data from STECF effort and landings
#'
#' Format the data from STECF effort and landings for the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#' 
#' These two dataframes have to be downloaded by hand and put in the data folder. 
#' The proper Annexes have to decided by the user.
#'
#' @param x the name of the dataframe with effort data
#' @param y the name of the dataframe with landings data
#' @param path, the path to the files
#'
#'@note Some considerable errors have been identified in the STECF data. Finland and Estonia effort data are not reliable,
#' and Germany recorded an erroneous haul in 2013. These values have been removed.
#' 
#' @return a data frame of stock status relative to reference points and catch, discards, and landings
#' by stock for the most recent assessment.
#' 
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{format_sag}} for formatting raw data from the ICES Stock Assessment database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' stecf_formatted <- format_stecf("Celtic Seas")
#' }
#'
#' @references
#'
#' STECF dissemination tool https://stecf.jrc.ec.europa.eu/web/stecf/dd/effort/graphs-annex


format_stecf_effort <- function(path){
        df <- read.csv(path)
        if(df$COUNTRY %in% c("SCO|ENG|GBG|GBJ|IOM|NIR")){
        df$country <- gsub("SCO|ENG|GBG|GBJ|IOM|NIR", "GBR", df$country)
        }
        df <- df %>% rename(ISO3C = country)
        df$ISO3C <- df$country
        df <- df%>%
                mutate(COUNTRY = countrycode::countrycode(ISO3C, "iso3c", "country.name"),
                       COUNTRY = ifelse(grepl("United Kingdom", COUNTRY),
                                        "United Kingdom",
                                        COUNTRY)) %>%
                mutate(YEAR = year,
                       EFFORT = as.numeric(nominal_effort)) %>%
                select(YEAR,
                       ANNEX = annex,
                       AREA = regulated.area,
                       COUNTRY,
                       GEAR = regulated.gear,
                       EFFORT)
        gear_dat <- df %>%
                        select(ANNEX, AREA, GEAR)
        gear_dat_clean <- bind_rows(
                gear_dat %>%
                        filter(ANNEX == "BAL") %>%
                        mutate(ECOREGION = "Baltic Sea Ecoregion",
                               gear_class = case_when(
                                       grepl("BEAM", .$GEAR) ~ "Beam trawl",
                                       grepl("3A|DREDGE", .$GEAR) ~ "Dredge",
                                       grepl("3B|3C|3T|GILL|TRAMMEL|LONGLINE", .$GEAR) ~ "Static/Gill net/LL",
                                       grepl("DEM_SEINE|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
                                       grepl("POTS", .$GEAR) ~ "Pots",
                                       grepl("NONE", .$GEAR) ~ "other",
                                       is.na(.$GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        ),
                gear_dat %>%
                        filter(ANNEX == "IIA",
                               AREA %in% c("3A", "3B1", "3B2", "3B3")) %>%
                        mutate(ECOREGION = "Greater North Sea Ecoregion",
                               gear_class = case_when(
                                       grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
                                       grepl("DREDGE", .$GEAR) ~ "Dredge",
                                       grepl("GN1|GT1|LL1|TRAMMEL", .$GEAR) ~ "Static/Gill net/LL",
                                       grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
                                       grepl("POTS", .$GEAR) ~ "Pots",
                                       grepl("NONE", .$GEAR) ~ "other",
                                       is.na(.$GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        ),
                gear_dat %>%
                        filter(ANNEX == "CEL1") %>%
                        mutate(ECOREGION = "Celtic Seas Ecoregion",
                               gear_class = case_when(
                                       grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
                                       grepl("DREDGE", .$GEAR) ~ "Dredge",
                                       grepl("GN1|GT1|LL1|TRAMMEL", .$GEAR) ~ "Static/Gill net/LL",
                                       grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
                                       grepl("POTS", .$GEAR) ~ "Pots",
                                       grepl("NONE", .$GEAR) ~ "other",
                                       is.na(.$GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        ),
                gear_dat %>%
                        filter(ANNEX == "BOB") %>%
                        mutate(ECOREGION = "Bay of Biscay and the Iberian Coast Ecoregion",
                               gear_class = case_when(
                                       grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
                                       grepl("DREDGE", .$GEAR) ~ "Dredge",
                                       grepl("GN1|GT1|LL1|TRAMMEL", .$GEAR) ~ "Static/Gill net/LL",
                                       grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
                                       grepl("POTS", .$GEAR) ~ "Pots",
                                       grepl("NONE", .$GEAR) ~ "other",
                                       is.na(.$GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        ),
                gear_dat %>%
                        filter(ANNEX == "IIB") %>%
                        mutate(ECOREGION = "Bay of Biscay and the Iberian Coast Ecoregion",
                               gear_class = case_when(
                                       grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
                                       grepl("DREDGE", .$GEAR) ~ "Dredge",
                                       grepl("GN1|GT1|LL1|TRAMMEL", .$GEAR) ~ "Static/Gill net/LL",
                                       grepl("TR1|TR2|TR3|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
                                       grepl("POTS", .$GEAR) ~ "Pots",
                                       grepl("NONE", .$GEAR) ~ "other",
                                       is.na(.$GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        )
        )
        
        df <- gear_dat_clean %>%
                left_join(df, by = c("ANNEX", "AREA", "GEAR")) %>%
                mutate(YEAR = as.numeric(YEAR))%>%
                select(YEAR ,
                       ANNEX,
                       ECOREGION,
                       AREA,
                       GEAR = gear_class,
                       COUNTRY,
                       EFFORT) %>%
                filter(!COUNTRY %in% c("Finland", "Estonia"))
        df
}

format_stecf_landings <- function(path){
        df <- read.csv(path)
        if(df$COUNTRY %in% c("SCO","ENG","GBG","GBJ","IOM","NIR")){
                df$country <- gsub("SCO|ENG|GBG|GBJ|IOM|NIR", "GBR", df$country)
        }
        df$ISO3C <- df$country
        df <- df%>%
                mutate(COUNTRY = countrycode::countrycode(ISO3C, "iso3c", "country.name"),
                       COUNTRY = ifelse(grepl("United Kingdom", COUNTRY),
                                        "United Kingdom",
                                        COUNTRY)) %>%
                mutate(YEAR = year,
                       LANDINGS = as.numeric(sum_landings),
                       LANDINGS = ifelse(COUNTRY == "Germany" &
                                                 year == 2013 &
                                                 `vessel.length` == "U8M",
                                         NA, LANDINGS)) %>%
                select(YEAR,
                       ANNEX = ï..annex,
                       AREA = regulated.area,
                       COUNTRY,
                       GEAR = regulated.gear,
                       LANDINGS)
        gear_dat <- df %>%
                select(ANNEX, AREA, GEAR)
        gear_dat_clean <- gear_dat %>%
                        mutate(gear_class = case_when(
                                       grepl("BEAM|BT1|BT2", .$GEAR) ~ "Beam trawl",
                                       grepl("DREDGE|3A", .$GEAR) ~ "Dredge",
                                       grepl("GN1|3B|3C|GT1|LL1|GILL|TRAMMEL|LONGLINE", .$GEAR) ~ "Static/Gill net/LL",
                                       grepl("TR1|TR2|TR3|DEM_SEINE|OTTER|DEM_SEINE", .$GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", .$GEAR) ~ "Pelagic trawl/seine",
                                       grepl("TRAMMEL|3T", .$GEAR) ~ "Trammel nets",
                                       grepl("POTS", .$GEAR) ~ "Pots",
                                       grepl("NONE", .$GEAR) ~ "other",
                                       is.na(.$GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        )
        gear_dat_clean <- unique(gear_dat_clean)
        df <- df %>% left_join(gear_dat_clean) %>%
                group_by(YEAR, gear_class) %>%
                summarize(LANDINGS = sum(LANDINGS, na.rm = TRUE))
        df
}
