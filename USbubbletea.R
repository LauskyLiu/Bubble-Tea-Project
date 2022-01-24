
library(pacman)
library(stringr)
library(dplyr)
library(htmltab)
library(tidycensus)
library(sugarbag)

pacman::p_load(leaflet, glue, dplyr, sf, tmap, tmaptools, oldtmaptools, tidycensus, ggmap, htmltools, htmlwidgets)
pacman::p_load_gh(c("walkerke/tigris", "bhaskarvk/leaflet.extras"))
p_load_gh("OmaymaS/yelpr")


topcities <- citytable <- htmltab("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population#Locations_of_50_most_populous_cities", 
                                  which = 5)

str(topcities)
head(topcities,3)

tester <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                        location = 'Bronx',
                                        term = "bubble tea",
                                        offset = 0,
                                        limit = 50))

housbubbletea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 0,
                                                limit= 50))

housbubbletea2 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 50,
                                                limit= 50))

housbubbletea3 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 100,
                                                limit= 50))

housbubbletea4 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 150,
                                                limit= 50))

housbubbletea5 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 200,
                                                limit= 50))

housbubbletea6 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 250,
                                                limit= 50))

housbubbletea7 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 300,
                                                limit= 50))

housbubbletea8 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                                location = "Houston",
                                                term = "bubble tea",
                                                offset = 350,
                                                limit= 50))

housbubbleteatotal <- bind_rows(housbubbletea1,housbubbletea2,housbubbletea3,housbubbletea4,housbubbletea5,housbubbletea6,housbubbletea7,housbubbletea8)

housclean <-housbubbleteatotal[!str_detect(housbubbleteatotal$businesses.name,
                                         "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]


labubbletea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 0,
                                              limit= 50))

labubbletea2 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 50,
                                              limit= 50))

labubbletea3 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 100,
                                              limit= 50))

labubbletea4 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 150,
                                              limit= 50))

labubbletea5 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 200,
                                              limit= 50))

labubbletea6 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 250,
                                              limit= 50))

labubbletea7 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 300,
                                              limit= 50))

labubbletea8 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 350,
                                              limit= 50))

labubbletea9 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 400,
                                              limit= 50))

labubbletea10 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 450,
                                              limit= 50))

labubbletea11 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 500,
                                              limit= 50))

labubbletea12 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 550,
                                              limit= 50))

labubbletea13 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 600,
                                              limit= 50))

labubbletea14 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 650,
                                              limit= 50))

labubbletea15 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Los Angeles",
                                              term = "bubble tea",
                                              offset = 700,
                                              limit= 50))

labubbleteatotal <- bind_rows(labubbletea1,labubbletea2,labubbletea3,labubbletea4,labubbletea5,
                              labubbletea6,labubbletea7,labubbletea8,labubbletea9,labubbletea10,
                              labubbletea11,labubbletea12,labubbletea13,labubbletea14,labubbletea15)

laclean <-labubbleteatotal[!str_detect(labubbleteatotal$businesses.name,
                                       "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

chibubbletea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                              location = "Chicago",
                                              term = "bubble tea",
                                              offset = 0,
                                              limit= 50))

chibubbletea2 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Chicago",
                                               term = "bubble tea",
                                               offset = 100,
                                               limit= 50))

chibubbletea3 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Chicago",
                                               term = "bubble tea",
                                               offset = 150,
                                               limit= 50))

chibubbletea4 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Chicago",
                                               term = "bubble tea",
                                               offset = 200,
                                               limit= 50))

chibubbletea5 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Chicago",
                                               term = "bubble tea",
                                               offset = 250,
                                               limit= 50))

chibubbletea6 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Chicago",
                                               term = "bubble tea",
                                               offset = 300,
                                               limit= 50))

chibubbletea7 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Chicago",
                                               term = "bubble tea",
                                               offset = 350,
                                               limit= 50))

chibubbleteatotal <- bind_rows(chibubbletea1,chibubbletea2,chibubbletea3,chibubbletea4,chibubbletea5,chibubbletea6,chibubbletea7)

chiclean <-chibubbleteatotal[!str_detect(chibubbleteatotal$businesses.name,
                                       "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

bosbubbletea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Boston",
                                               term = "bubble tea",
                                               offset = 0,
                                               limit= 50))

bosbubbletea2 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Boston",
                                               term = "bubble tea",
                                               offset = 50,
                                               limit= 50))

bosbubbletea3 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Boston",
                                               term = "bubble tea",
                                               offset = 100,
                                               limit= 50))

bosbubbletea4 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Boston",
                                               term = "bubble tea",
                                               offset = 150,
                                               limit= 50))

bosbubbletea5 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Boston",
                                               term = "bubble tea",
                                               offset = 200,
                                               limit= 50))

bosbubbletea6 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Boston",
                                               term = "bubble tea",
                                               offset = 250,
                                               limit= 50))

bosbubbleteatotal <- bind_rows(bosbubbletea1,bosbubbletea2,bosbubbletea3,bosbubbletea4,bosbubbletea5,bosbubbletea6)

bosclean <-bosbubbleteatotal[!str_detect(bosbubbleteatotal$businesses.name,
                                         "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

hunttea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                               location = "Huntsville",
                                               term = "bubble tea",
                                               offset = 0,
                                               limit= 50))

huntclean <-hunttea1[!str_detect(hunttea1$businesses.name,
                                         "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

alastea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                          location = "Anchorage",
                                          term = "bubble tea",
                                          offset = 0,
                                          limit= 50))

alasclean <-alastea1[!str_detect(alastea1$businesses.name,
                                 "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

phoetea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                          location = "Phoenix",
                                          term = "bubble tea",
                                          offset = 0,
                                          limit= 50))

phoetea2 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                          location = "Phoenix",
                                          term = "bubble tea",
                                          offset = 50,
                                          limit= 50))

phoetea3 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                          location = "Phoenix",
                                          term = "bubble tea",
                                          offset = 100,
                                          limit= 50))

phoebubbleteatotal <- bind_rows(phoetea1, phoetea2, phoetea3)

phoeclean <-phoebubbleteatotal[!str_detect(phoebubbleteatotal$businesses.name,
                                 "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

littletea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                          location = "Little Rock",
                                          term = "bubble tea",
                                          offset = 0,
                                          limit= 50))

littleclean <-littletea1[!str_detect(littletea1$businesses.name,
                                           "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]

dentea1 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                             location = "Denver",
                                             term = "bubble tea",
                                             offset = 0,
                                             limit= 50))

dentea2 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                         location = "Denver",
                                         term = "bubble tea",
                                         offset = 50,
                                         limit= 50))

dentea3 <- as.data.frame(business_search("D4b5z_zDrc6CmR5f8wfDlTwxnUI5qXvJeUqy2vaGFizu7PQj9d5Z4sEFD5mnOtAJuTsLjBbDWx2CukdOEhuvQMxOh7FlnWwVmBUuYAUHi1KbzWjrLoLSrT_6RM-RYXYx",
                                         location = "Denver",
                                         term = "bubble tea",
                                         offset = 100,
                                         limit= 50))

denbubbleteatotal <- bind_rows(dentea1, dentea2, dentea3)

denclean <-denbubbleteatotal[!str_detect(denbubbleteatotal$businesses.name,
                                     "Noodle|Pastry|Saigon|Deli|Sandiwches|Teriyaki|Vegeterian|Pho|Ramen|BBQ|Dog"),]



