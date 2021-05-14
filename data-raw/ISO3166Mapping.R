
#
# devtools::install_github("rOpenGov/regions")

glimpse(regions::google_nuts_matchtable %>% filter(country_code=="GB"))


here::i_am("data-raw/IS03166Mapping.R")

# data from
# http://www.statoids.com/ugb.html
tmp = readxl::read_excel(here::here("data-raw/ISO3166-2.xlsx"),sheet = "unofficial")
tmp2 = tmp %>% mutate(ISO = paste0("GB-",ISO)) %>% select(name = Name, HASC, ISO, NUTS)
tmp3 = readxl::read_excel(here::here("data-raw/ISO3166-2.xlsx"),sheet = "official")
# Official versus unofficial 
tmp3 %>% anti_join(tmp2, by=c("3166-2 code"="ISO")) %>% View()

# Investigations using NUTS 
# tmp4 = tmp3 %>% dpc$codes$findCodesByName(nameVar = `Subdivision name`)
# 
# nuts3 = readr::read_csv("https://opendata.arcgis.com/datasets/3e1d40ce19494869b43a6997e7a539a2_0.csv")
# nuts2 = readr::read_csv("https://opendata.arcgis.com/datasets/ded3b436114440e5a1561c1e53400803_0.csv")
# nuts1 = readr::read_csv("https://opendata.arcgis.com/datasets/46e7d1871e914b5aa7b57a38eed26caf_0.csv")
# 
# nuts1 = nuts1 %>% mutate(code = NUTS118CD, name = NUTS118NM, codeType="NUTS118") %>% select(name,code,codeType)
# nuts2 = nuts2 %>% mutate(code = NUTS218CD, name = NUTS218NM, codeType="NUTS218") %>% select(name,code,codeType)
# nuts3 = nuts3 %>% mutate(code = NUTS318CD, name = NUTS318NM, codeType="NUTS318") %>% select(name,code,codeType)
# 
# nuts=bind_rows(nuts1,nuts2,nuts3)
# tmp3 %>% mutate(name = stringr::str_to_lower(`Subdivision name`)) %>% inner_join(nuts %>% mutate(name = stringr::str_to_lower(name),by=c("name")))
# tmp5 = regions::google_nuts_matchtable %>% filter(country_code=="GB")

# The google mobility shapefile
tmp6 = dpc$datasets$getGoogleMobility()

# Sub region 1 google mobility
googleMap = rgdal::readOGR("https://github.com/datasciencecampus/google-mobility-reports-data/blob/master/geography/Google_Places_UK_Boundaries_BGC_200417.gpkg?raw=true")
googleMap = googleMap %>% sf::st_as_sf() %>% rename(GSS_ENT_CD = GSS_ENTITY_CD, GSS_ENT_NM=GSS_ENTITY_NM)
# dpc$geog$saveShapefile(mapId = "GOOGLE_MOB",shape = sf::st_as_sf(googleMap) %>% rename(GSS_ENT_CD = GSS_ENTITY_CD, GSS_ENT_NM=GSS_ENTITY_NM),overwrite = TRUE)


gm = googleMap %>% mutate(lc = GPL_NM %>% stringr::str_to_lower())
tmp7 = tmp6 %>% mutate(lc = ifelse(is.na(sub_region_2), sub_region_1, sub_region_2)) %>% mutate(lc = lc %>% stringr::str_to_lower()) %>% mutate(lc = ifelse(lc=="bristol city","city of bristol",lc))

tmp7 %>% anti_join(gm, by="lc") %>% select(country_region,sub_region_1,sub_region_2,metro_area,iso_3166_2_code) %>% distinct()
gm %>% anti_join(tmp7, by="lc")

tmp8 = gm %>% inner_join(tmp7 %>% select(country_region,sub_region_1,sub_region_2,metro_area,iso_3166_2_code,lc) %>% distinct(), by="lc")

# tmp8 %>% as_tibble() %>% select(-geometry) %>% View()
# tmp8 %>% as_tibble() %>% select(-geometry) %>% select(GSS_CD,GSS_NM,GSS_ENT_CD,GSS_ENT_NM,iso_3166_2_code) %>% distinct() %>% View()


# Sub region  2 
googleMap2 = rgdal::readOGR("https://github.com/datasciencecampus/google-mobility-reports-data/blob/master/geography/google_mobility_lad_boundary_200903.gpkg?raw=true")
googleMap2 = googleMap2 %>% sf::st_as_sf()
googRegions = tmp6 %>% select(country_region,sub_region_1,sub_region_2,metro_area,iso_3166_2_code) %>% distinct()

#googRegions %>% anti_join(googleMap2, by=c("sub_region_1","sub_region_2")) %>% View()
#googleMap2 %>% anti_join(googRegions, by=c("sub_region_1","sub_region_2"))

tmp9 = googRegions %>% left_join(googleMap2 %>% as_tibble() %>% select(-geometry), by=c("sub_region_1","sub_region_2"))
tmp10 = tmp9 %>% select(-c(bng_e,bng_n,long,lat,st_areasha,st_lengths,flag_2018,country_region_code))
tmp11 = tmp10 %>% rename(GSS_CD=lad19cd,GSS_NM=lad19nm)

tmp12 = tmp11 %>% as_tibble() %>% select(iso_3166_2_code,GSS_CD,GSS_NM) %>% filter(!is.na(iso_3166_2_code) & !is.na(GSS_CD)) %>% distinct() %>% arrange(iso_3166_2_code)
tmp13 = tmp8 %>% as_tibble() %>% select(iso_3166_2_code,GSS_CD,GSS_NM) %>% filter(!is.na(iso_3166_2_code) & !is.na(GSS_CD)) %>% distinct() %>% arrange(iso_3166_2_code)
tmp14 = tmp12 %>% bind_rows(tmp13 %>% anti_join(tmp12,by="iso_3166_2_code"))

missing = tmp3 %>% anti_join(tmp14, by=c("3166-2 code"="iso_3166_2_code")) %>% dpc$codes$findCodesByName(nameVar = "Subdivision name", codeTypes = "UA") %>% select(iso_3166_2_code=`3166-2 code`, GSS_CD = code, GSS_NM = name.original)  %>% filter(!is.na(GSS_CD)) 
tmp15 = bind_rows(tmp14,missing)
tmp16 = tmp15 %>% dpc$codes$findNamesByCode(codeVar = "GSS_CD")

tmp17 = tmp16 %>% left_join(dpc$codes$getTransitiveClosure() %>% filter(toCodeType == "NHSER"), by=c("GSS_CD"="fromCode"))

lad19 = tmp15 %>% 
  mutate(GSS_CD = ifelse(GSS_NM=="Dorset Ceremonial County","E06000059",GSS_CD)) %>% 
  bind_rows(tibble(
    iso_3166_2_code=c("GB-SHN","GB-ELS"),
    GSS_CD=c("E08000013","S12000013"),
    GSS_NM=c("St. Helens","Eilean Siar"))) %>% 
  inner_join(dpc$geog$getMap("LAD19"), by=c("GSS_CD"="code"))
ctyua19 = tmp15 %>% inner_join(dpc$geog$getMap("CTYUA19"), by=c("GSS_CD"="code")) %>% anti_join(lad19, by="GSS_CD")
comp = bind_rows(lad19,ctyua19)
comp = comp %>% select(-GSS_NM) %>% rename(code = GSS_CD) %>% sf::st_as_sf()

tmp3 %>% anti_join(comp, by=c("3166-2 code"="iso_3166_2_code"))

dpc$geog$saveShapefile(shape = comp, mapId = "ISO3166_MANUAL",overwrite = TRUE)
dpc$geog$preview(comp)
cogUKMap = comp
usethis::use_data(cogUKMap,overwrite = TRUE)

