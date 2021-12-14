
# get the area_27 level names for an ecoregion
get_area_27 <- function(ecoregion) {

  #ecoregion <- match.arg(ecoregion, ecoregions)
  switch(ecoregion,
    `Baltic Sea` = 
      c("3.a.20", "3.a.21", # extra ones for labeling
        "3.d.27", "3.d.25", "3.d.24",
        "3.b.23", "3.c.22", "3.d.31",
        "3.d.30", "3.d.32", "3.d.29",
        "3.d.28.1", "3.d.28.2", "3.d.26"),
    `Greater North Sea` = 
      c("3.a.20", "3.a.21","3.b.23",
        "4.a", "4.b", "4.c",
        "7.d", "7.e"),
    `Celtic Seas` = 
      c("4.a", "2.a.2", "5.b", # extra ones for labeling
        "6.a", "6.b.2","7.a", "7.b", "7.c.2", "7.e",
        "7.f", "7.g", "7.h","7.j.2", "7.k.2"),
    `Bay of Biscay and the Iberian Coast` =
      c("8.a", "8.b","8.c",
        "8.d.2", "8.e.2", "9.a",
        "9.b.2"),
    `Icelandic Waters` = 
      c("5.a.1", "5.a.2","12.a.4"),  
    `Norwegian Sea` = 
      c("2.a.1", "2.a.2", "2.b.1", "2.b.2", "14.a"),  
    `Barents Sea` = 
    c("1.a", "1.b","2.a.2", "2.b.2"),
    `Greenland Sea` = 
            c("12.a.3", "14.b.2","14.a", "5.a.2", "2.b.2"),
    `Faroes` = 
            c("6.a", "5.b.1.b", "2.a.2", "4.a", "5.b.2"),
    `Oceanic Northeast atlantic` = 
            c("14.b.1", "12.a.1", "12.b", "6.b.1", "7.c.1", "12.c", "7.k.1", "7.j.1", "10.b",
              "8.e.1", "8.d.1", "10.a.1", "9.b.1", "5.b.1.a", "12.a.2"),
    `Azores` = 
            c("10.a.2")
  )
}
