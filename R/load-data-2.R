library(bahelpers)
library(myhelpr)

load_all_data <- function(
  attribute_file = "../../../data/building_level/attributes_20181019_tidy.csv",
  qh_file = "../../../data/building_level/data_all_2018.RData",
  building_list_file = "../../../data/building_level/building_list_20181022.csv"
) {
  cat("Load attributes...\n\n")
  # Already tidied file. Use ba_helpers::read_ba_attributes if you need to tidy original file.
  attributes <- read_csv(attribute_file)
  
  cat("Load quarter hourly data...\n\n")
  # data_2018 <- load_qh_csv(qh_path = "../../data/building_level/qh_2018/",
  #                          outlier_file = "../../data/building_level/outlier_dates_20181120.csv")
  # save(data_2018, file = "../../data/building_level/data_all_2018.RData")
  load(qh_file)
  
  cat("Load building list...\n\n")
  buildings <- read_csv(building_list_file, comment = "#") %>% 
    select(b_uid = UID,
           BID = IDNUMBER,
           Name = NAME,
           NetLettableArea = NETLETTABLEAREA,
           ASSETTYPE_UID,
           CITY,
           STATE)
  
  all_data <- attributes %>% 
    inner_join(buildings) %>% 
    inner_join(data_2018) %>% 
    mutate(Date = floor_date(ts, "day"),
           Wh = kwh/NetLettableArea*1e3,
           `Working hours` = if_else(hour(ts)>=9 & hour(ts)<=17, TRUE, FALSE),
           Season = season(ts),
           Hour = hour(ts),
           Period = (hour(ts)+minute(ts)/60)*4 + 1)
  
  
  # Filter for Australian office buildings
  all_data <- all_data %>% 
    filter(ASSETTYPE_UID %in% c(5, 9)) %>%  # Office buildings
    filter(STATE %in% c("ACT", "NSW", "QLD", "SA", "VIC", "WA")) %>%  # In australia
    # filter(AssetName %in% c("Office Building", "Office Building V2", "Office Building Universal")) %>% 
    #filter(!(b_uid %in% c(34, 175, 177))) %>% #these have weird negative values
    #filter(b_uid != 169) %>%  # This building has weird spikes - huge values
    #filter(BID != "BID1801") %>% # Test building. Weird Facade percentages
    filter(Wh > 0, Wh < 50) # Remove obvious outliers
  
  rm(data_2018)
  
  all_data
}



load_qh_csv <- function(qh_path = "../../../data/building_level/qh_2018",
                        outlier_file = "../../../data/building_level/outlier_dates_20181120.csv") {
  
  outlier_dates <- read_csv(outlier_file) %>% 
    mutate(DATE = date(DATE))
  
  data_frame(file = list.files(qh_path, full.names = TRUE)) %>% 
    mutate(data = map(file, read_csv,
                      col_types = cols_only(
                        BUILDINGUID = col_integer(),
                        # NLA = col_integer(),
                        DT = col_datetime(format = ""),
                        # DATE = col_date(format = ""),
                        WEEKDAY = col_character(),
                        # PERIOD = col_integer(),
                        KWH = col_double()  #,
                        # KWH_PREDICTED = col_double(),
                        # TEMPERATURE = col_double(),
                        # MOISTURE = col_double(),
                        # MONTH = col_integer(),
                        # SEASON = col_character()
                      ))) %>% 
    select(data) %>% 
    unnest() %>% 
    filter(!(WEEKDAY %in% c("Saturday", "Sunday"))) %>% 
    mutate(DATE = date(DT)) %>% 
    anti_join(outlier_dates) %>%  # Remove public holidays
    rename_all(tolower) %>% 
    select(ts = dt, b_uid = buildinguid, kwh) %>% 
    na.omit()
}
