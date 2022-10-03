#' Format the data from STECF effort and landingsfrom 2020 when the stecf format changed
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

library(readxl)
# data_list <- import_list("FDI-catches-by-country.xlsx", setclass = "tbl", rbind = TRUE)
df1<- read_excel("FDI-catches-by-country.xlsx", 
              sheet = 1)
df2<- read_excel("FDI-catches-by-country.xlsx", 
                 sheet = 2)
df3<- read_excel("FDI-catches-by-country.xlsx", 
                 sheet = 3)
df4<- read_excel("FDI-catches-by-country.xlsx", 
                 sheet = 4)
df5<- read_excel("FDI-catches-by-country.xlsx", 
                 sheet = 5)
# df6<- read_excel("FDI-catches-by-country.xlsx", 
#                  sheet = 6)

df <- rbind(df1,df2,df3,df4,df5)

df<- read_excel("FDI-effort-by-country.xlsx", 
                 sheet = 2)


names(df)
unique(df$`supra-region`)
unique(df$`sub-region`)
unique(df$`sub-region`)

bts_df <- df %>% filter(`sub-region`%in% c( "27.3.D.28.1","27.3.B.23", "27.3.D.32" ,"27.3.D.30",
                                            "27.3.D.25","27.3.D.26","27.3.D.28.2","27.3.D.31",
                                            "27.3.D.24","27.3.D.29","27.3.D.27","27.3.C.22" ))

nrs_df <- df %>% filter(`sub-region`%in% c("27.3.A.20", "27.3.A.21", "27.4.A", "27.4.B",
                                           "27.4.C", "27.4.D", "27.4.E"))

cs_df <- df %>% filter(`sub-region`%in% c("27.6.A", "27.6.B.2", "27.7.A", "27.7.B",
                                           "27.7.C.2", "27.7.F", "27.7.G", "27.2.H", "27.7.J.2", "27.7.K.2"))

bi_df <- df %>% filter(`sub-region`%in% c("27.8.A", "27.8.B", "27.8.C", "27.8.D.2",
                                           "27.8.E.2", "27.9.A", "27.9.B.2"))


cs_df <- dplyr::mutate(cs_df, gear_class = case_when(
        grepl("TBB",`gear type`) ~ "Beam trawl",
        grepl("DRB|DRH|HMD", `gear type`) ~ "Dredge",
        grepl("GNS|GND|GTN|GNC|LHP|LLS|FPN|GTR|FYK|LLD|SDN|LTL|LHM|LNB|LA|GEF", `gear type`) ~ "Static/Gill net/LL",
        grepl("OTT|OTB|PTB|SSC|SB", `gear type`) ~ "Otter trawl/seine",
        grepl("PTM|OTM|PS|SV|SPR", `gear type`) ~ "Pelagic trawl/seine",
        grepl("FPO", `gear type`) ~ "Pots",
        grepl("NK", `gear type`) ~ "other",
        is.na(`gear type`) ~ "other",
        TRUE ~ "other"
)
)

check <-unique(cs_df[c("gear type", "gear_class")])
names(nrs_df)

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
