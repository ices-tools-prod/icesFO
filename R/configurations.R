# Custom configurations for format_catches

# Historic division mappings
historic_division_mappings <- list(
  "Baltic Sea" = c("III (not specified)", "III b  Baltic 23", "III b+c (not specified)", "III b-d (not specified)",
                   "III c  Baltic 22", "III d  (not specified)", "III d  Baltic 24", "III d  Baltic 25",
                   "III d  Baltic 26", "III d  Baltic 27", "III d  Baltic 28 (not specified)", "III d  Baltic 28-1",
                   "III d  Baltic 28-2", "III d  Baltic 29", "III d  Baltic 30", "III d  Baltic 31", "III d  Baltic 32"),
  "Greater North Sea" = c("III a", "IIIa  and  IV  (not specified)", "IIIa  and  IVa+b  (not specified)", "IV (not specified)",
                          "IV a", "IV a+b (not specified)", "IV b", "IV b+c (not specified)", "IV c", "VII d"),
  "Bay of Biscay and the Iberian Coast" = c("VIII a", "VIII b", "VIII c", "VIII d2", "VIII e2", "IX a", "IX b2", "VIII d (not specified)",
                                            "VIII (not specified)", "VIII e (not specified)", "IX (not specified)", "IX b (not specified)"),
  "Celtic Seas" = c("VI a", "VI b2", "VII a", "VII b", "VII c2", "VII f", "VII g", "VII h", "VII j2", "VII k2",
                    "VII (not specified)", "VII b+c (not specified)", "VII c (not specified)", "VII d-k (not specified)",
                    "VII f-k (not specified)", "VII g-k (not specified)", "VII j (not specified)"),
  "Icelandic Waters" = c("V a (North-East)", "V a (South-West)", "V a1", "V a (not specified)", "V a2"),
  "Azores" = c("X (not specified)", "X a (not specified)"),
  "Greenland Sea" = c("XII a3", "XIV (not specified)", "XIV a", "XIV b (not specified)", "XIV b2"),
  "Norwegian Sea" = c("II a1", "II b1", "I  and  IIa (not specified)", "II a (not specified)", "II (not specified)",
                      "II a2", "II b (not specified)", "II b2", "XIV", "XIVa"),
  "Barents Sea" = c("I (not specified)", "I a", "I b", "I  and  IIa (not specified)", "II a (not specified)",
                    "II (not specified)", "II a2", "II b (not specified)", "II b2"),
  "Faroes" = c("V b2", "V b (not specified)", "V b1 (not specified)", "V b1B"),
  "Oceanic Northeast Atlantic" = c("V b1A", "VI b1", "VII c1", "VII j1", "VII k1", "VIII d1", "VIII e1", "IX b1", "X b", "XII a1",
                                   "XII b", "XIV b1", "X (not specified)", "X a (not specified)", "XII (not specified)")
)

# Modern area mappings
modern_area_mappings <- list(
  "Baltic Sea" = c("27.3.bc", "27.3.d", "27.3_nk"),
  "Greater North Sea" = c("27.3.a", "27.4", "27.7.d"),
  "Bay of Biscay and the Iberian Coast" = c("27.8.a", "27.8.b", "27.8.c", "27.8.d.2", "27.8.e.2", "27.9.a", "27.9.b.2"),
  "Celtic Seas" = c("27.6.a", "27.6.b.2", "27.7.a", "27.7.b", "27.7.c.2", "27.7.f", "27.7.g", "27.7.h", "27.7.j.2", "27.7.k.2"),
  "Icelandic Waters" = c("27.5.a.1", "27.5.a.2", "27.5.a_NK", "27.5.a_nk", "27.12.a.4"),
  "Norwegian Sea" = c("27.2.a.1", "27.2.a.2", "27.2.a_NK", "27.2.a_nk", "27.2.b.1", "27.2.b.2", "27.2.b_NK", "27.2.b_nk", "27.14.a", "27.14_NK", "27.14_nk"),
  "Azores" = c("27.10.a.2", "27.10.a_NK", "27.10.a_nk", "27.10_NK", "27.10_nk"),
  "Greenland Sea" = c("27.12.a.3", "27.14.a", "27.14.b.2", "27.14.b_NK", "27.14.b_nk", "27.14_NK", "27.14_nk"),
  "Faroes" = c("27.5.b.2", "27.5.b.1.a", "27.5.b.1.b", "27.5.b.1_NK", "27.5.b_NK", "27.5.b.1_nk", "27.5.b_nk"),
  "Barents Sea" = c("27.1.a", "27.1.b", "27.2.a.2", "27.2.a_NK", "27.2.a_nk", "27.2.b.2", "27.2.b_NK", "27.2.b_nk", "27.1_NK", "27.1_nk"),
  "Oceanic Northeast Atlantic" = c("27.5.b.1.a", "27.6.b.1", "27.7.c.1", "27.7.j.1", "27.7.k.1", "27.8.d.1", "27.8.e.1", "27.9.b.1", "27.10.a.1", "27.10.b", "27.12_nk", "27.12_NK", "27.12.a.1", "27.12.b", "27.12.c", "27.14.b.1")
)

# Helper function to determine ecoregion based on division or area
determine_ecoregion <- function(area_or_division) {
  # Check if it's a modern area code
  for (ecoregion in names(modern_area_mappings)) {
    if (area_or_division %in% modern_area_mappings[[ecoregion]]) {
      return(ecoregion)
    }
  }
  
  # Check if it's a historic division code
  for (ecoregion in names(historic_division_mappings)) {
    if (area_or_division %in% historic_division_mappings[[ecoregion]]) {
      return(ecoregion)
    }
  }
  
  return("OTHER")
}

# Helper function to clean country names
clean_country_name <- function(country) {
  case_when(
    grepl("Guernsey|Isle of Man|Jersey", country) ~ "United Kingdom",
    grepl("^Germany", country) ~ "Germany",
    country == "Un. Sov. Soc. Rep." ~ "Russian Federation",
    country == "Faeroe Islands" ~ "Faroe Islands",
    TRUE ~ country
  )
}

# Helper function to clean species names
clean_species_name <- function(species_name) {
  case_when(
    species_name == "Ammodytes" ~ "Sandeels(=Sandlances) nei",
    TRUE ~ species_name
  )
}

# Helper function to get species code
get_species_code <- function(species_name) {
  case_when(
    species_name == "Ammodytes" ~ "SAN",
    TRUE ~ NA_character_
  )
}