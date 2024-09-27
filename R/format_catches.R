# Load custom configurations
source("configurations.R")

#' Format the data from the time-series of ICES historical, official and preliminary catches.
#'
#' @param year the year required
#' @param historical a dataframe output from load_historical_catches() required.
#' @param official a dataframe output from load_official_catches() required.
#' @param preliminary a dataframe output from load_preliminary_catches() optional.
#' @param ecoregion an identifier of the Ecoregion of interest
#' @param species_list a list of species common names and scientific names from ASFIS
#' @param sid the main table from ICES Stock Information DB (SD)
#'
#' @return A data frame.
#'
#' @export
format_catches <- function(year, ecoregion, historical, official, preliminary = NULL, species_list, sid) {
  fish_category <- dplyr::mutate(sid, X3A_CODE = substr(sid$StockKeyLabel, start = 1, stop = 3))
  fish_category <- dplyr::select(fish_category, X3A_CODE, FisheriesGuild)
  fish_category$X3A_CODE <- toupper(fish_category$X3A_CODE)
  fish_category <- unique(fish_category)
  
  catch_dat_1950 <- tidyr::gather(historical, YEAR, VALUE, -Country, -Species, -Division) 
  catch_dat_1950 <- dplyr::mutate(catch_dat_1950,
    YEAR = as.numeric(gsub("X", "", YEAR)),
    VALUE = ifelse(VALUE == "<0.5", as.numeric(0), as.numeric(VALUE)),
    Country = clean_country_name(Country),
    ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
    ECOREGION = sapply(Division, determine_ecoregion)
  )
  catch_dat_1950 <- dplyr::filter(catch_dat_1950, YEAR <= 2005)
  catch_dat_1950 <- dplyr::left_join(catch_dat_1950, species_list, c("Species" = "English_name"))# Merge to add FAO species information
  catch_dat_1950 <- dplyr::left_join(catch_dat_1950, species_list,c("Species" = "Scientific_name", # Merge to add FAO species information
                                                                              "X3A_CODE"))
  catch_dat_1950 <- dplyr::left_join(catch_dat_1950, fish_category, by = "X3A_CODE")
  catch_dat_1950 <- dplyr::select(catch_dat_1950,YEAR,
                                  COUNTRY = Country,
                                  ISO3,
                                  GUILD = FisheriesGuild,
                                  ECOREGION,
                                  SPECIES_NAME = Scientific_name,
                                  SPECIES_CODE = X3A_CODE,
                                  COMMON_NAME = Species,
                                  VALUE)
  
  # add in official
  catch_dat_2010 <- tidyr::gather(official, YEAR, VALUE, -Country, -Species, -Area, -Units)
  catch_dat_2010 <- dplyr::mutate(catch_dat_2010,
    YEAR = as.numeric(gsub("X", "", YEAR)),
    VALUE = as.numeric(VALUE),
    Country = clean_country_name(countrycode::countrycode(Country, "iso2c", "country.name")),
    ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
    Area = tolower(Area),
    ECOREGION = sapply(Area, determine_ecoregion)
  )
  catch_dat_2010 <- dplyr::filter(catch_dat_2010, Country != "")
  catch_dat_2010 <- dplyr::left_join(catch_dat_2010, species_list, c("Species" = "X3A_CODE"))
  catch_dat_2010 <- dplyr::left_join(catch_dat_2010, fish_category, by = c("Species" = "X3A_CODE")) 
  catch_dat_2010 <- catch_dat_2010[!is.na(catch_dat_2010$ECOREGION),]
  catch_dat_2010 <- dplyr::select(catch_dat_2010,YEAR,
                                  COUNTRY = Country,
                                  ISO3,
                                  GUILD = FisheriesGuild,
                                  ECOREGION,
                                  SPECIES_NAME = Scientific_name,
                                  SPECIES_CODE = Species,
                                  COMMON_NAME = English_name,
                                  VALUE)
  catch_dat_2010 <- dplyr::group_by(catch_dat_2010,YEAR, COUNTRY, GUILD, ECOREGION,COMMON_NAME)
  catch_dat_2010 <- dplyr::summarise(catch_dat_2010, VALUE = sum(VALUE))
  
  # now do preliminary catches
  
  if (is.null(preliminary)) {
          df <- dplyr::bind_rows(catch_dat_2010,catch_dat_1950)
  } else {
          catch_dat_prelim <- dplyr::filter(preliminary, Country != "")
          catch_dat_prelim$VALUE <- catch_dat_prelim[,7]
          catch_dat_prelim <- catch_dat_prelim[, -grep("AMS", colnames(catch_dat_prelim))]
          catch_dat_prelim <- catch_dat_prelim[, -grep("BMS", colnames(catch_dat_prelim))]
          catch_dat_prelim$Species.Latin.Name <- catch_dat_prelim[,3]
          # catch_dat_prelim <- catch_dat_prelim[, -grep("Species.Latin.Name", colnames(catch_dat_prelim))]
          # tidyr::gather(ï..Year, -Country, -AphiaID, -Area, -Catch) %>%
          catch_dat_prelim <- dplyr::mutate(catch_dat_prelim,
            YEAR = Year,
            Country = clean_country_name(countrycode::countrycode(Country, "iso2c", "country.name")),
            ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
            Area = tolower(Area),
            ECOREGION = sapply(Area, determine_ecoregion),
            COMMON_NAME = clean_species_name(Species.Latin.Name),
            SPECIES_CODE = get_species_code(Species.Latin.Name)
          )
          
          catch_dat_prelim <- dplyr::filter(catch_dat_prelim, ECOREGION != "OTHER")
          catch_dat_prelim <- dplyr::left_join(catch_dat_prelim, species_list, c("Species.Latin.Name" = "Scientific_name"))
          
          catch_dat_prelim <- dplyr::left_join(catch_dat_prelim, fish_category, by = "X3A_CODE")
          catch_dat_prelim <- dplyr::select(catch_dat_prelim,YEAR,
                                            COUNTRY = Country,
                                            ISO3 = X3A_CODE,
                                            GUILD = FisheriesGuild,
                                            ECOREGION,
                                            SPECIES_NAME = "Species.Latin.Name",
                                            SPECIES_CODE = X3A_CODE,
                                            COMMON_NAME = English_name,
                                            VALUE)
          catch_dat_prelim$VALUE <- as.numeric(catch_dat_prelim$VALUE)
          df <- dplyr::bind_rows(catch_dat_2010,catch_dat_1950, catch_dat_prelim)
  }
  
  
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, GUILD = ifelse(is.na(GUILD),
                                         "undefined",
                                         GUILD))
  
  df$COUNTRY<-gsub("Russian Federation", "\\Russia\\",df$COUNTRY)
  df$COUNTRY<-gsub("Russia", "Russian Federation", df$COUNTRY)
  df <- dplyr::select(df,YEAR,
                     COUNTRY,
                     ISO3,
                     GUILD ,
                     ECOREGION,
                     SPECIES_NAME,
                     SPECIES_CODE,
                     COMMON_NAME,
                     VALUE)
  
  # df$GUILD[which(ices_catch_dat$SPECIES_CODE == "WHB")] <- "pelagic"
  df <- dplyr::filter(df, ECOREGION %in% ecoregion)
  
  return(df)
}




