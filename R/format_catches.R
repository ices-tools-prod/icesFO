#' Format the data from the time-series of ICES historical, official and preliminary catches.
#'
#' Format the data from the ICES Stock Assessment Database for the downloaded year and the specific Ecoregion
#' for which you are producing the Fisheries Overviews.
#'
#' @param x a dataframe output from load_historical_catches() required.
#' @param y a dataframe output from load_official_catches() required.
#' @param z a dataframe output from load_preliminary_catches() optional.
#' @param ecoregion an identifier of the Ecoregion of interest
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
#' catches_format <- format_catches(x,y,z, "Celtic Seas")
#' }
#'
#' @references
#'
#' The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
#'
#' @export

#other variables to keep?
format_catches <- function(year, ecoregion, x, y,z = NULL) {
        sid <-load_sid(year)
        fish_category <- dplyr::mutate(sid, X3A_CODE = substr(sid$StockKeyLabel, start = 1, stop = 3))
        fish_category <- dplyr::select(fish_category, X3A_CODE, FisheriesGuild)
        fish_category$X3A_CODE <- toupper(fish_category$X3A_CODE)
        fish_category <- unique(fish_category)
        
        # fish_category<- fish_category[complete.cases(fish_category),]
        
        spURL <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
        tmpFileSp <- tempfile(fileext = ".zip")
        download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
        FAO_file <- grep(".*txt", unzip(tmpFileSp,list = TRUE)$Name, value = TRUE)
        species_list_raw <- read.delim(unz(tmpFileSp, FAO_file),
                                       fill = TRUE,
                                       stringsAsFactors = FALSE,
                                       header = TRUE,
                                       na.strings = "")
        
        species_list_raw <- dplyr::select(species_list_raw, English_name, Scientific_name, X3A_CODE)
        
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
        #these historical catches definition need decission on conflicts
        historic_bob <- c("VIII a", "VIII b", "VIII c", "VIII d2", "VIII e2",
                          "IX a", "IX b2", "VIII d (not specified)", "IX (not specified)")
        
        historic_cs <- c("VI a", "VI b2", "VII a", "VII b", "VII c2", "VII f", "VII g", "VII h",
                         "VII j2", "VII k2", "VII (not specified)", "VII b+c (not specified)",
                         "VII c (not specified)", "VII d-k (not specified)", "VII f-k (not specified)",
                         "VII g-k (not specified)", "VII j (not specified)")
        if(ecoregion == "Norwegian Sea Ecoregion"){
        historic_nw <- c( "II a1", "II b1", "I  and  IIa (not specified)","II a (not specified)",
                          "II (not specified)", "II a2", "II b (not specified)",
                          "II b2", "XIV", "XIVa" )
        }
        if(ecoregion == "Barents Sea Ecoregion"){
        historic_br <- c( "I (not specified)", "I a","I b",  "I  and  IIa (not specified)","II a (not specified)",
                          "II (not specified)", "II a2", "II b (not specified)",
                          "II b2" )
        }
        x[is.na(x)] <- 0
        catch_dat_1950 <- tidyr::gather(x,YEAR, VALUE, -Country, -Species, -Division) 
        catch_dat_1950 <- dplyr::mutate(catch_dat_1950, YEAR = as.numeric(gsub("X", "", YEAR)),
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
                               Division %in% historic_bs ~ "Baltic Sea Ecoregion",
                               Division %in% historic_ns ~ "Greater North Sea Ecoregion",
                               Division %in% historic_bob ~ "Bay of Biscay and the Iberian Coast Ecoregion",
                               Division %in% historic_cs ~ "Celtic Seas Ecoregion",
                               if(ecoregion == "Norwegian Sea Ecoregion"){
                               Division %in% historic_nw ~ "Norwegian Sea Ecoregion"
                                       },
                               if(ecoregion == "Barents Sea Ecoregion"){
                                       Division %in% historic_br ~ "Barents Sea Ecoregion"
                                       },
                               TRUE ~ "OTHER")) 
        catch_dat_1950 <- dplyr::filter(catch_dat_1950, YEAR <= 2005) 
        catch_dat_1950 <- dplyr::left_join(catch_dat_1950, y = species_list_raw, c("Species" = "English_name"))# Merge to add FAO species information
        catch_dat_1950 <- dplyr::left_join(catch_dat_1950, y = species_list_raw,c("Species" = "Scientific_name", # Merge to add FAO species information
                                                                                  "X3A_CODE"))
        catch_dat_1950 <- dplyr::left_join(catch_dat_1950, y = fish_category, by = "X3A_CODE")
        catch_dat_1950 <-dplyr::select(catch_dat_1950,YEAR,
                       COUNTRY = Country,
                       ISO3,
                       GUILD = FisheriesGuild,
                       ECOREGION,
                       SPECIES_NAME = Scientific_name,
                       SPECIES_CODE = X3A_CODE,
                       COMMON_NAME = Species,
                       VALUE)
        
        catch_dat_2010 <- tidyr::gather(y, YEAR, VALUE, -Country, -Species, -Area, -Units)
        catch_dat_2010 <- dplyr::filter(catch_dat_2010, Country != "")
        catch_dat_2010 <- dplyr::mutate(catch_dat_2010,YEAR = as.numeric(gsub("X", "", YEAR)),
                       VALUE = as.numeric(VALUE),
                       Country = countrycode::countrycode(Country,"iso2c", "country.name"),
                       Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                                        "United Kingdom",
                                        Country),
                       ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                       Country = gsub("(United Kingdom) .*", "\\1", Country),
                       Area = tolower(Area),
                       ECOREGION = dplyr::case_when(
                               Area %in% c("27.3.bc", "27.3.d", "27.3_nk") ~ "Baltic Sea Ecoregion",
                               Area %in% c("27.3.a", "27.4", "27.7.d") ~ "Greater North Sea Ecoregion",
                               
                               Area %in% c("27.8.a", "27.8.b","27.8.c",
                                             "27.8.d.2", "27.8.e.2", "27.9.a",
                                             "27.9.b.2") ~ "Bay of Biscay and the Iberian Coast Ecoregion",
                               Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                                             "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2") ~ "Celtic Seas Ecoregion",
                               
                               Area %in% c("27.5.a.1", "27.5.a.2","27.12.a.4") ~ "Icelandic Waters Ecoregion",
                               if(ecoregion == "Norwegian Sea Ecoregion"){
                               Area %in% c("27.2.a.1", "27.2.a.2", "27.2.b.1", "27.2.b.2", "27.14.a") ~ "Norwegian Sea Ecoregion"
                                       },
                               if(ecoregion =="Barents Sea Ecoregion"){
                               Area %in% c("27.1.a", "27.1.b","27.2.a.2","27.2.a_NK", "27.2.b.2","27.2.b_NK", "27.1_NK") ~ "Barents Sea Ecoregion"
                                       }))
        catch_dat_2010 <- dplyr::left_join(catch_dat_2010,species_list_raw, c("Species" = "X3A_CODE"))
        catch_dat_2010 <- dplyr::left_join(catch_dat_2010,fish_category, by = c("Species" = "X3A_CODE")) 
        catch_dat_2010 <- dplyr::select(catch_dat_2010,YEAR,
                       COUNTRY = Country,
                       ISO3,
                       GUILD = FisheriesGuild,
                       ECOREGION,
                       SPECIES_NAME = Scientific_name,
                       SPECIES_CODE = Species,
                       COMMON_NAME = English_name,
                       VALUE)
        
        # catch_dat_prelim <- z %>%
        #         # tidyr::gather(ï..Year, -Country, -AphiaID, -Area, -Catch) %>%
        #         filter(Country != "") %>%
        #         mutate(YEAR = Year,
        #                VALUE = AMS.Catch.TLW.,
        #                Country = countrycode::countrycode(Country,"iso2c", "country.name"),
        #                Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
        #                                 "United Kingdom",
        #                                 Country),
        #                ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
        #                Country = gsub("(United Kingdom) .*", "\\1", Country),
        #                Area = tolower(Area))
        
        
        #check why areas names are different!!
        # catch_dat_prelim <- catch_dat_prelim%>%
        #         mutate(ECOREGION = dplyr::case_when(
        #                 .$Area %in% c("27_3_bc", "27_3_c_22","27_3_d","27_3_d_24","27_3_d_25","27_3_d_26","27_3_d_30",
        #                               "27_3_d_27","27_3_d_31","27_3_nk", "27_3_b_23", "27_3_d_28_2","27_3_d_32","27_3_d_29") ~ "Baltic Sea Ecoregion",
        #                 .$Area %in% c("27_3_a", "27_4_a","27_4_b", "27_4_c", "27_7_d") ~ "Greater North Sea Ecoregion",
        #                 
        #                 .$Area %in% c("27_8_a", "27_8_b","27_8_c",
        #                               "27_8_d_2", "27_8_e_2", "27_9_a",
        #                               "27_9_b_2")~ "Bay of Biscay and the Iberian Coast Ecoregion",
        #                 .$Area %in% c("27_6_a", "27_6_b_2","27_7_a", "27_7_b", "27_7_c_2","27_7.e",
        #                               "27_7_f", "27_7_g", "27_7_h","27_7_j_2", "27_7_k_2")~"Celtic Seas Ecoregion",
        #                 
        #                 .$Area %in% c("5_a_1", "5_a_2","12_a_4")~"Icelandic Waters Ecoregion",
        #                 
        #                 .$Area %in% c("2_a_1", "2_a_2", "2_b_1", "2_b_2", "14.a")~"Norwegian Sea Ecoregion",
        #                 TRUE ~ "OTHER"))%>%
        #         filter(ECOREGION != "OTHER") %>%
        #         left_join(species_list_raw, c("Species.Latin.Name" = "Scientific_name"))
        # 
        # catch_dat_prelim <- catch_dat_prelim%>%
        #         left_join(fish_category) %>%
        #         select(YEAR,
        #                COUNTRY = Country,
        #                ISO3,
        #                GUILD = FisheriesGuild,
        #                ECOREGION,
        #                SPECIES_NAME = Species.Latin.Name,
        #                SPECIES_CODE = X3A_CODE,
        #                COMMON_NAME = English_name,
        #                VALUE)
        # catch_dat_prelim$COMMON_NAME[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "Sandeels(=Sandlances) nei"
        # catch_dat_prelim$SPECIES_CODE[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "SAN"
        # 
        df <- dplyr::bind_rows(catch_dat_2010,catch_dat_1950)
        df <- dplyr::mutate(df, GUILD = ifelse(is.na(GUILD),
                                               "undefined",
                                               GUILD))
                
                #remember to turn this on again if I want preliminary catches
                # bind_rows(catch_dat_prelim)%>%
                
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
