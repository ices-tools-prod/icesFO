#' Format the data from STECF effort and landings
#'
#' Format the data from STECF effort and landings for the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#' 
#' These two dataframes have to be downloaded by hand and put in the data folder. 
#' The proper Annexes have to decided by the user.
#'
#' @param x the name of the dataframe with effort data
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
#' 
#' @rdname format_stecf
#' @name format_stecf
NULL

#' @rdname format_stecf
#' @export
format_stecf_effort <- function(x){
        df <- x
        df$country <- gsub("SCO|ENG|GBG|GBJ|IOM|NIR", "GBR", df$country)
        df <- dplyr::rename(df,ISO3C = country)
        df <- dplyr::mutate(df, COUNTRY = countrycode::countrycode(ISO3C, "iso3c", "country.name"),
                       # COUNTRY = ifelse(grepl("United Kingdom", COUNTRY),
                       #                  "United Kingdom",
                       #                  COUNTRY),
                       YEAR = year,
                       EFFORT = as.numeric(nominal_effort))
        df <- dplyr::select(df,YEAR ,
                       ANNEX = annex,
                       AREA = "regulated.area",
                       COUNTRY,
                       GEAR = "regulated.gear",
                       EFFORT )
        gear_dat <- dplyr::select(df,ANNEX, AREA, GEAR)
        
        gear_dat<-dplyr::mutate(gear_dat,ECOREGION = case_when(
                grepl("BAL", ANNEX) ~ "Baltic Sea Ecoregion",
                grepl("IIA", ANNEX) & grepl("3A", AREA) ~ "Greater North Sea Ecoregion",
                grepl("IIA", ANNEX) & grepl("3B1", AREA) ~ "Greater North Sea Ecoregion",
                grepl("IIA", ANNEX) & grepl("3B2", AREA) ~ "Greater North Sea Ecoregion",
                grepl("IIA", ANNEX) & grepl("3B3", AREA) ~ "Greater North Sea Ecoregion",
                grepl("CEL1", ANNEX) ~ "Celtic Seas Ecoregion",
                grepl("BOB", ANNEX) ~ "Celtic Seas Ecoregion",
                grepl("IIB", ANNEX) ~ "Celtic Seas Ecoregion",
                TRUE ~ "other"))
                
          gear_dat_clean <- dplyr::mutate(gear_dat, gear_class = case_when(
                                       grepl("BEAM|BT1|BT2", GEAR) ~ "Beam trawl",
                                       grepl("3A|DREDGE", GEAR) ~ "Dredge",
                                       grepl("GN1|GT1|LL1|3B|3C|3T|GILL|TRAMMEL|LONGLINE", GEAR) ~ "Static/Gill net/LL",
                                       grepl("TR1|TR2|TR3|DEM_SEINE|OTTER|DEM_SEINE", GEAR) ~ "Otter trawl/seine",
                                       grepl("PEL_SEINE|PEL_TRAWL", GEAR) ~ "Pelagic trawl/seine",
                                       grepl("POTS", GEAR) ~ "Pots",
                                       grepl("NONE", GEAR) ~ "other",
                                       is.na(GEAR) ~ "other",
                                       TRUE ~ "other"
                               )
                        )
          
        df2 <- dplyr::left_join(df,gear_dat_clean)
        df <- unique(df2)
        df <- dplyr::mutate(df,YEAR = as.numeric(YEAR))
        df <- dplyr::select(df,YEAR ,
                       ANNEX,
                       ECOREGION,
                       AREA,
                       GEAR = gear_class,
                       COUNTRY,
                       EFFORT) 
        
        df <- df[complete.cases(df), ]
        df
}


#' @rdname format_stecf
#' @export
format_stecf_landings <- function(x){
        df <- x
        df$country <- gsub("SCO|ENG|GBG|GBJ|IOM|NIR", "GBR", df$country)
        df$ISO3C <- df$country
        df <- dplyr::mutate(df, COUNTRY = countrycode::countrycode(ISO3C, "iso3c", "country.name"),
                            YEAR = year,
                            LANDINGS = as.numeric(sum_landings))
        
        df <- dplyr::select(df,YEAR,
                       ANNEX = annex,
                       AREA = "regulated.area",
                       COUNTRY,
                       GEAR = "regulated.gear",
                       LANDINGS)
        gear_dat <- dplyr::select(df,ANNEX, AREA, GEAR)
        gear_dat_clean <- dplyr::mutate(gear_dat,gear_class = case_when(
                grepl("BEAM|BT1|BT2", GEAR) ~ "Beam trawl",
                grepl("3A|DREDGE", GEAR) ~ "Dredge",
                grepl("GN1|GT1|LL1|3B|3C|3T|GILL|TRAMMEL|LONGLINE", GEAR) ~ "Static/Gill net/LL",
                grepl("TR1|TR2|TR3|DEM_SEINE|OTTER|DEM_SEINE", GEAR) ~ "Otter trawl/seine",
                grepl("PEL_SEINE|PEL_TRAWL", GEAR) ~ "Pelagic trawl/seine",
                grepl("POTS", GEAR) ~ "Pots",
                grepl("NONE", GEAR) ~ "other",
                is.na(GEAR) ~ "other",
                TRUE ~ "other"
                )
                )
        gear_dat_clean <- unique(gear_dat_clean)
        df <- dplyr::left_join(df,gear_dat_clean)
        df <- dplyr::group_by(df,YEAR, gear_class)
        df <- dplyr::summarise(df,LANDINGS = sum(LANDINGS, na.rm = TRUE))
        df <- df[complete.cases(df), ]
        df
}
