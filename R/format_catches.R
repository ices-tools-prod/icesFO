#' Format the data from the time-series of ICES historical, official and preliminary catches.
#'
#' Format the data from the ICES Stock Assessment Database for the downloaded year and the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#'
#' @param year the year required
#' @param historical a dataframe output from load_historical_catches() required.
#' @param official a dataframe output from load_official_catches() required.
#' @param preliminary a dataframe output from load_preliminary_catches() optional.
#' @param ecoregion an identifier of the Ecoregion of interest
#' @param species_list a list of species common names and scientific names
#'  from ASFIS
#' @param sid the main table from ICES Stock Information DB (SD)
#'
#' @return A data frame..
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#' \code{\link{format_sid}} for formatting raw data from the ICES Stock Information database. 
#'
#' \code{\link{icesFO-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#'   sid <- load_sid(2019)
#'   species_list <- load_asfis_species()
#'   historical <- load_historical_catches()
#'   official <- load_official_catches()
#'   preliminary <- load_preliminary_catches(2018)
#'   catches_format <- 
#'     format_catches(2019, "Celtic Seas", historical, official, preliminary,
#'                    species_list, sid)
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

format_catches <- function(year, ecoregion, historical, official, preliminary = NULL, species_list, sid) {

  fish_category <- dplyr::mutate(sid, X3A_CODE = substr(sid$StockKeyLabel, start = 1, stop = 3))
  fish_category <- dplyr::select(fish_category, X3A_CODE, FisheriesGuild)
  fish_category$X3A_CODE <- toupper(fish_category$X3A_CODE)
  fish_category <- unique(fish_category)

  # fish_category<- fish_category[complete.cases(fish_category),]

  historic_bs <- c("III (not specified)", "III b  Baltic 23",
                   "III b+c (not specified)", "III b-d (not specified)",
                   "III c  Baltic 22", "III d  (not specified)",
                   "III d  Baltic 24", "III d  Baltic 25",
                   "III d  Baltic 26", "III d  Baltic 27",
                   "III d  Baltic 28 (not specified)", "III d  Baltic 28-1",
                   "III d  Baltic 28-2", "III d  Baltic 29",
                   "III d  Baltic 30", "III d  Baltic 31",
                   "III d  Baltic 32")

  historic_ns <- c("III a", "IIIa  and  IV  (not specified)",
                   "IIIa  and  IVa+b  (not specified)", "IV (not specified)",
                   "IV a", "IV a+b (not specified)",
                   "IV b", "IV b+c (not specified)",
                   "IV c", "VII d")

  historic_uk <- paste0(c("^UK", "^Channel", "^Isle of Man"),
                        collapse = "|")
  #these historical catches definition need decision on conflicts
  historic_bob <- c("VIII a", "VIII b", "VIII c", "VIII d2", "VIII e2",
                    "IX a", "IX b2", "VIII d (not specified)", "VIII (not specified)","VIII e (not specified)", "IX (not specified)",
                    "IX b (not specified)")

  historic_cs <- c("VI a", "VI b2", "VII a", "VII b", "VII c2", "VII f", "VII g", "VII h",
                   "VII j2", "VII k2", "VII (not specified)", "VII b+c (not specified)",
                   "VII c (not specified)", "VII d-k (not specified)", "VII f-k (not specified)",
                   "VII g-k (not specified)", "VII j (not specified)")

  historic_is <- c("V a (North-East)", "V a (South-West)", "V a1", "V a (not specified)", "V a2")
  historic_az <- c("X (not specified)", "X a (not specified)")
  historic_gs <- c("XII a3", "XIV (not specified)", 
                   "XIV a", "XIV b (not specified)", "XIV b2" )
  

  if(ecoregion == "Norwegian Sea"){
  historic_nw <- c( "II a1", "II b1", "I  and  IIa (not specified)","II a (not specified)",
                    "II (not specified)", "II a2", "II b (not specified)",
                    "II b2", "XIV", "XIVa" )
  }
  if(ecoregion == "Barents Sea"){
  historic_br <- c( "I (not specified)", "I a","I b",  "I  and  IIa (not specified)","II a (not specified)",
                    "II (not specified)", "II a2", "II b (not specified)",
                    "II b2" )
  }
  if(ecoregion == "Faroes"){
          historic_fo <- c( "V b2","V b (not specified)", "V b1 (not specified)", "V b1B")
          
  }
  if(ecoregion == "Oceanic Northeast Atlantic"){
          historic_nea <- c( "V b1A", "VI b1", "VII c1", "VII j1", "VII k1", "VIII d1", "VIII e1",
                             "IX b1", "X b", "XII a1", "XII b", "XIV b1","X (not specified)", "X a (not specified)", 
                             "XII (not specified)")
  }
  fo_2020 <- historical %>% filter(Division == "ICES Area (not specified)")
  fo_2020 <-fo_2020%>% filter(Country == "Faeroe Islands")
  
 
          
  fo_2020$Division <- "V b1 (not specified)"
  fo_2020$Division[which(fo_2020$Species == "Atlantic mackerel")] <- "ICES Area (not specified)"
  fo_2020$Division[which(fo_2020$Species == "Atlantic horse mackerel")] <- "ICES Area (not specified)"
  fo_2020$Division[which(fo_2020$Species == "Atlantic herring")] <- "ICES Area (not specified)"
  historical <- full_join(historical, fo_2020)

  
  historical[is.na(historical)] <- 0

  catch_dat_1950 <- tidyr::gather(historical, YEAR, VALUE, -Country, -Species, -Division) 
  catch_dat_1950 <- 
    dplyr::mutate(catch_dat_1950, YEAR = as.numeric(gsub("X", "", YEAR)),
                 VALUE = ifelse(VALUE == "<0.5",
                                as.numeric(0),
                                VALUE),
                 VALUE = ifelse(!is.na(VALUE),
                                as.numeric(VALUE),
                                NA),
                 Country = dplyr::case_when(
                         grepl(historic_uk, Country) ~ "United Kingdom",
                         grepl("^Germany", Country) ~ "Germany",
                         Country %in% c("Un. Sov. Soc. Rep.") ~ "Russian Federation",
                         grepl("Faeroe Islands", Country) ~ "Faroe Islands",
                         grepl("Other nei", Country) ~ "OTHER",
                         TRUE ~ Country
                 ),
                 ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                 ECOREGION = dplyr::case_when(
                         Division %in% historic_bs ~ "Baltic Sea",
                         Division %in% historic_ns ~ "Greater North Sea",
                         Division %in% historic_bob ~ "Bay of Biscay and the Iberian Coast",
                         Division %in% historic_cs ~ "Celtic Seas",
                         Division %in% historic_is ~ "Icelandic Waters",
                         Division %in% historic_az ~ "Azores",
                         Division %in% historic_gs ~ "Greenland Sea",
                         if(ecoregion == "Oceanic Northeast Atlantic"){
                                 Division %in% historic_nea ~ "Oceanic Northeast Atlantic"
                         
                                }, 
                         if(ecoregion == "Faroes"){
                                 Division %in% historic_fo ~ "Faroes"
                                 
                         },
                        if(ecoregion == "Norwegian Sea"){
                         Division %in% historic_nw ~ "Norwegian Sea"
                                 },
                         if(ecoregion == "Barents Sea"){
                                 Division %in% historic_br ~ "Barents Sea"
                                 },
                         TRUE ~ "OTHER")) 

  catch_dat_1950 <- dplyr::filter(catch_dat_1950, YEAR <= 2005)
  catch_dat_1950 <- dplyr::left_join(catch_dat_1950, y = species_list, c("Species" = "English_name"))# Merge to add FAO species information
  catch_dat_1950 <- dplyr::left_join(catch_dat_1950, y = species_list,c("Species" = "Scientific_name", # Merge to add FAO species information
                                                                            "X3A_CODE"))
  catch_dat_1950 <- dplyr::left_join(catch_dat_1950, y = fish_category, by = "X3A_CODE")
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
  catch_dat_2010$VALUE <- as.numeric(catch_dat_2010$VALUE)
  catch_dat_2010$VALUE[is.na(catch_dat_2010$VALUE)] <- 0
  catch_dat_2010 <- dplyr::filter(catch_dat_2010, Country != "")
  catch_dat_2010 <- dplyr::mutate(catch_dat_2010,YEAR = as.numeric(gsub("X", "", YEAR)),
                 VALUE = as.numeric(VALUE),
                 Country = countrycode::countrycode(Country,"iso2c", "country.name"),
                 Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                                  "United Kingdom",
                                  Country),
                 ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                 Country = gsub("(United Kingdom) .*", "\\1", Country),
                 Area = tolower(Area))
  catch_dat_2010 <- dplyr::mutate(catch_dat_2010,
                                  ECOREGION = dplyr::case_when(
                         Area %in% c("27.3.bc", "27.3.d", "27.3_nk") ~ "Baltic Sea",
                         Area %in% c("27.3.a", "27.4", "27.7.d") ~ "Greater North Sea",
                         Area %in% c("27.8.a", "27.8.b","27.8.c",
                                       "27.8.d.2", "27.8.e.2", "27.9.a",
                                       "27.9.b.2") ~ "Bay of Biscay and the Iberian Coast",
                         Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                                       "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2") ~ "Celtic Seas",
                         Area %in% c("27.5.a.1", "27.5.a.2","27.5.a_NK","27.5.a_nk","27.12.a.4") ~ "Icelandic Waters",
                        if(ecoregion == "Norwegian Sea"){
                         Area %in% c("27.2.a.1", "27.2.a.2","27.2.a_NK","27.2.a_nk", "27.2.b.1", "27.2.b.2", "27.2.b_NK","27.2.b_nk","27.14.a", "27.14_NK", "27.14_nk") ~ "Norwegian Sea"
                                 },
                        if(ecoregion == "Azores"){
                                Area %in% c("27.10.a.2", "27.10.a_NK", "27.10.a_nk", "27.10_NK", "27.10_nk") ~ "Azores"
                        },
                        if(ecoregion == "Greenland Sea"){
                                Area %in% c("27.12.a.3", "27.14.a", "27.14.b.2", "27.14.b_NK", "27.14.b_nk", "27.14_NK", "27.14_nk") ~ "Greenland Sea"
                        },
                        if(ecoregion == "Faroes"){
                                Area %in% c("27.5.b.2","27.5.b.1.a","27.5.b.1.b", "27.5.b.1_NK", "27.5.b_NK", "27.5.b.1_nk",
                                            "27.5.b_nk") ~ "Faroes"
                        },
                         if(ecoregion =="Barents Sea"){
                                 Area %in% c("27.1.a", "27.1.b","27.2.a.2","27.2.a_NK","27.2.a_nk", "27.2.b.2","27.2.b_NK","27.2.b_nk", "27.1_NK", "27.1_nk") ~ "Barents Sea"
                                 },
                        if(ecoregion == "Oceanic Northeast Atlantic"){
                                Area %in% c("27.5.b.1.a", "27.6.b.1","27.7.c.1", "27.7.j.1",
                                            "27.7.k.1", "27.8.d.1", "27.8.e.1", "27.9.b.1", 
                                            "27.10.a.1", "27.10.b", "27.12_nk","27.12_NK", "27.12.a.1", "27.12.b", 
                                            "27.12.c", "27.14.b.1") ~ "Oceanic Northeast Atlantic" 
                        },
                        TRUE ~ "OTHER"))
  catch_dat_2010 <- dplyr::left_join(catch_dat_2010,species_list, c("Species" = "X3A_CODE"))
  catch_dat_2010 <- dplyr::left_join(catch_dat_2010,fish_category, by = c("Species" = "X3A_CODE")) 
  catch_dat_2010<- catch_dat_2010[!is.na(catch_dat_2010$ECOREGION),]
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
          catch_dat_prelim$VALUE <- catch_dat_prelim[,6]
          catch_dat_prelim <- catch_dat_prelim[, -grep("AMS", colnames(catch_dat_prelim))]
          catch_dat_prelim <- catch_dat_prelim[, -grep("BMS", colnames(catch_dat_prelim))]
          catch_dat_prelim$Species.Latin.Name <- catch_dat_prelim[,3]
          # catch_dat_prelim <- catch_dat_prelim[, -grep("Species.Latin.Name", colnames(catch_dat_prelim))]
          # tidyr::gather(ï..Year, -Country, -AphiaID, -Area, -Catch) %>%
          catch_dat_prelim <- dplyr::mutate(catch_dat_prelim, YEAR = Year,
                                            Country = countrycode::countrycode(Country,"iso2c", "country.name"),
                                            Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                                                             "United Kingdom",
                                                             Country),
                                            ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                                            Country = gsub("(United Kingdom) .*", "\\1", Country),
                                            Area = tolower(Area))
          
          
          #check why areas names are different!!
          catch_dat_prelim <- dplyr::mutate(catch_dat_prelim, ECOREGION = dplyr::case_when(
                  Area %in% c("27_3_bc", "27_3_c_22","27_3_d","27_3_d_24","27_3_d_25","27_3_d_26","27_3_d_30",
                              "27_3_d_27","27_3_d_31","27_3_nk", "27_3_b_23", "27_3_d_28_2","27_3_d_32","27_3_d_29") ~ "Baltic Sea",
                  Area %in% c("27_3_a", "27_4_a","27_4_b", "27_4_c", "27_7_d") ~ "Greater North Sea",
                  
                  Area %in% c("27_8_a", "27_8_b","27_8_c",
                              "27_8_d_2", "27_8_e_2", "27_9_a",
                              "27_9_b_2")~ "Bay of Biscay and the Iberian Coast",
                  Area %in% c("27_6_a", "27_6_b_2","27_7_a", "27_7_b", "27_7_c_2","27_7.e",
                              "27_7_f", "27_7_g", "27_7_h","27_7_j_2", "27_7_k_2")~"Celtic Seas",
                  
                  Area %in% c("5_a_1", "5_a_2","12_a_4")~"Icelandic Waters",
                  Area %in% c("27_10_a_2", "27_10_A_2")~"Azores",
                  Area %in% c("27_1_a", "27_1_b", "27_2_b_2")~ "Barents Sea",
                  Area %in% c("27_2_a_1", "27_2_a_2", "27_2_b_1", "27_2_b_2", "27_14_a", "27_2_a", "27_2_b")~"Norwegian Sea",
                  Area %in% c("27_5_b_1_A", "27_6_b_1","27_7_c_1", "27_7_j_1","27_7_k_1",
                              "27_8_d_1", "27_8_e_1", "27_9_b_1", "27_10_a_1", "27_10_b", 
                              "27_12_a_1", "27_12_b", "27_12_c", "27_14_b_1")~"Oceanic Northeast Atlantic",
                  Area %in% c("27_14_B", "27_14", "27_14_B_2", "27_14_A", "27_14_NK")~"Greenland Sea",
                  Area %in% c(" 27_5_b", "27_5_b_1", "27_5_b_2", "27_5_b_1_b")~"Faroes",
                  TRUE ~ "OTHER"))
          
          catch_dat_prelim <- dplyr::filter(catch_dat_prelim,ECOREGION != "OTHER")
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
          catch_dat_prelim$COMMON_NAME[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "Sandeels(=Sandlances) nei"
          catch_dat_prelim$SPECIES_CODE[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "SAN"
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
