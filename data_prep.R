library(icd.data)
library(dplyr)
mort_data <- arrow::read_parquet("mortality_full.parquet")
mort_tmp <- mort_data %>% left_join(icd.data::icd10cm2016 %>% filter(code == three_digit) %>% select(three_digit, short_desc), by = c("Cause" = "three_digit"))
head(icd.data::icd10_pcs_2016)

exception_codes <- data.table::fread("exception_codes.csv") %>% 
  select(Cause = code, cause_description = Cause)
mort_data <- mort_data %>% left_join(exception_codes)
mort_data$cause_description[is.na(mort_data$cause_description)] <-  mort_data %>% 
  filter(is.na(cause_description)) %>% 
  select(-cause_description) %>% 
  mutate(Cause = substr(Cause, 1, 3)) %>% 
  left_join(icd.data::icd10cm2016 %>% filter(code == three_digit) %>% select(three_digit, cause_description = short_desc), by = c("Cause" = "three_digit")) %>% 
  pull(cause_description)
mort_data$cause_description[is.na(mort_data$cause_description)] <- "Other"
mort_data$Sex <- ifelse(mort_data$Sex == 1, "male", ifelse(mort_data$Sex == 2, "female", "unspecified"))
data.table::fwrite(mort_data, "mortality_full.csv")
arrow::write_parquet(mort_data, "mortality_full.parquet")
