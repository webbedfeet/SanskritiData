library(tidyverse)
con <- DBI::dbConnect(RMySQL::MySQL(),
                      host = 'localhost',
                      user = 'root',
                      password = 'slbc249',
                      dbname = 'sanskriti20180306')

std_phone <- function(s) {
  x <- as.data.frame(str_match(s, "\\(*(\\d{3})[-\\.\\) ]*(\\d{3})[-\\. ]*(\\d{4})")[, 2:4])
  y <- x %>% mutate(no = glue::glue("{V1}-{V2}-{V3}")) %>% pull(no)
  return(y)
}


# Sanskriti printed directory -----------------------------------------------------------------

library(tidyverse)
dat <- readLines("data/raw/Sanskriti_directory.md")
d <- strsplit(paste(dat, collapse = "\n"), "\n\n")[[1]] # Bring each record into 1 line, then separate lines

fullnames <- str_match(d, "^([A-Za-z, &\\(\\)\\.\\-\\']+)\\n")[,2]
addresses <- map_chr(d, ~str_match(., "\\n([0-9P]+[A-Za-z0-9#\\., #\\']+)\\n")[,2]) %>%
  str_to_title()

d <- str_replace(d, 'Maryland','MD') # One record, 756
csz_regex <- '\\n([A-Za-z][a-z \\.][A-Za-z, ]+[A-Z]{2}[ 0-9]*)' # some dont have ZIP, one isn't capitalized
city_state_zip <- map_chr(d, ~str_match(., csz_regex)[,2])
states <- str_extract(city_state_zip, '[A-Z]{2}')
zips <- str_extract(city_state_zip, "\\d+$")
zips <- ifelse(nchar(zips) == 4, paste0('0',zips), zips) # fixes NE zips
zips[56] = '21234' # Fixes extra character in zip
cities <- str_match(city_state_zip, "^([A-Za-z, \\.]+)[A-Z]{2}")[,2] %>%
  str_trim() %>%
  str_replace(',$','')

regex_phone <- '\\(?\\d{3}\\)?[ \\.\\-]?\\d{3}[ \\.\\-]?\\d{4}'
bl <- str_match_all(d, regex_phone)
std_phone_format <- function(x){
  y <- str_match(x, '(\\d{3})[ \\-\\.]?(\\d{3})[ \\-\\.]?(\\d{4})')
  return(paste(y[,-1],collapse='-'))
}

phone_numbers <- str_match_all(d, regex_phone) %>%
  map(~apply(as.matrix(.), 1, std_phone_format)) %>%
  map_chr(paste, collapse=', ')

# TODO: Annotate (H), (C) by hand, since relatively small numbers

email_regex <- "[A-Za-z0-9\\.\\_\\%\\+\\-]+@[A-Za-z0-9\\.\\-]+\\.[A-Za-z]{2,}"
emails <- str_match_all(d, email_regex) %>%
  map_chr(paste, collapse = ', ')

life_member_ind <- str_detect(d, "[Ll]ife [Mm]ember")
life_member <- ifelse(life_member_ind, 'Life Member', NA)


normalized_data <- tibble(FullName = fullnames,
                          Address = addresses,
                          City = cities,
                          State = states,
                          ZIP = zips,
                          Email = emails,
                          Phone = phone_numbers,
                          LifeMember = life_member)

openxlsx::write.xlsx(normalized_data, 'Test.xlsx')
saveRDS(normalized_data, file='data/rda/directory_normalized.rds', compress=T)


# Old code ------------------------------------------------------------------------------------
######################################################################

grab_line <- function(x, grep_expr) {
  y <- unlist(str_split(x, "\n"))
  ind <- which(str_detect(y, grep_expr))
  if (length(ind) == 0) {
    return(NA)
  } else {
    return(y[ind])
  }
}
fix_strings <- function(s) {
  s %>%
    str_replace_all("\\(", "\\\\(") %>%
    str_replace_all("\\)", "\\\\)") %>%
    str_replace_all("\\.", "\\\\.")
}

name <- unlist(map(d, ~str_split(., "\n")[[1]][1]))
name <- ifelse(str_detect(name, "[0-9@]"), NA, name)
life_member_ind <- str_detect(d, "[Ll]ife [Mm]ember")
life_member <- str_extract(d, "\\*\\*[lL]ife [Mm]ember\\*\\*")
phone_number <- unlist(map(d, ~grab_line(., "\\d{3}[-\\. ]*\\d{3}[-\\. ]\\d{4}")))
email <- unlist(map(d, ~grab_line(., "[A-Za-z0-9-\\_\\.]+@[A-Za-z0-9\\.]+")))

replace_if_exists <- function(x, pattern, replacement = ''){
  if (is.na(pattern)) {
    return(x)
  }
  if (str_detect(x, pattern)) {
    return(str_replace(x, pattern, replacement))
  } else {
    return(x)
  }
}

dat <- tibble("orig" = d, name, phone_number, email, life_member, life_member_ind)
dat %>%
  mutate(address = orig %>%
    replace_if_exists(fix_strings(name), "") %>%
    replace_if_exists(fix_strings(phone_number), "") %>%
    replace_if_exists(fix_strings(email), "") %>%
    replace_if_exists("\\*\\*[Ll]ife [Mm]ember\\*\\*", "") %>%
    replace_if_exists("^\n", "") %>%
    replace_if_exists("[\n]+$", "") %>%
    replace_if_exists("\n", ", ")) %>%
  mutate(pid = 1:nrow(.)) %>%
  select(pid, name, address, phone_number, email, life_member, life_member_ind) -> directory_printed

directory_printed %>%
  mutate(split_names = map(name, ~unlist(str_split(., '&')))) %>%
  mutate(split_names = map(split_names, ~str_trim(.))) %>%
  mutate(name_primary = map_chr(split_names, `[`,1)) %>%
  mutate(spouse = map(split_names,2)) %>%
  mutate(spouse = map_chr(spouse, ~ifelse(is.null(.), NA, .))) %>%
  select(-split_names)-> directory_printed

directory_printed %>%
  mutate(unique_phone = map(phone_number, ~unlist(str_extract_all(., '\\d{3}[-\\. ]*\\d{3}[-\\. ]*\\d{4}')))) %>%
  unnest() %>%
  mutate(unique_email = map(email, ~unlist(str_extract_all(., '[A-Za-z0-9-_\\.]+@[A-Za-z\\.-]+')))) %>%
  unnest() -> directory_printed
readr::write_csv(directory_printed, path = "data/raw/Sanskriti_directory.csv")
openxlsx::write.xlsx(directory_printed, file = "data/raw/Sanskriti_directory.xlsx")

saveRDS(directory_printed, file = "data/rda/printed_directory.rds", compress = T)

# End old code --------------------------------------------------------------------------------


## %######################################################%##
#                                                          #
####              Sanskriti web directory (old way)      ####
#                                                          #
## %######################################################%##

# TODO: Update this for new way to get data from Wordpress, see GDrive note

directory_db <- tbl(con, 'dev_swpm_members_tbl') %>% collect() %>%
  select(member_id, first_name, last_name, starts_with('address'), country, email, phone, notes) %>%
  rename(spouse = notes) %>%
  mutate(spouse = str_trim(spouse))

directory_db[directory_db == ''] <- NA

# directory_db <- read_tsv("data/raw/members_20180120.tsv") %>%
  # select(first_name, last_name, starts_with("address"), country, email, phone)
directory_db <- directory_db %>%
  mutate(
    first_name = tools::toTitleCase(tolower(first_name)),
    last_name = tools::toTitleCase(tolower(last_name)),
    address_street = tools::toTitleCase(tolower(address_street)),
    address_city = tools::toTitleCase(tolower(address_city)),
    address_state = ifelse(address_state %in% state.abb,
      address_state,
      state.abb[grep(tolower(address_state), tolower(state.name))]
    ),
    address_street = ifelse(str_detect(address_street, "\\.{4}"), NA, address_street)
  ) %>%
  mutate(
    name = glue::glue("{last_name}, {first_name}"),
    address = glue::glue("{address_street}, {address_city}, {address_state} {address_zipcode}")
  ) %>%
  mutate(
    address = ifelse(str_detect(address, "NA|NULL"), NA, address),
    phone = ifelse(str_detect(phone, "NA|NULL"), NA, phone)
  ) %>%
  select(name, address, phone, email, everything())

directory_db %>%
  select(name, address, phone, email) %>%
  mutate(pid = 1:nrow(.)) %>%
  mutate(uniq_no = map(phone, ~unlist(str_extract_all(., "\\(*\\d{3}[-\\.\\) ]*\\d{3}[-\\. ]*\\d{4}")))) %>%
  select(-phone) %>%
  unnest() -> directory_online



directory_online <- directory_online %>%
  mutate(
    uniq_phone = std_phone(uniq_no),
    uniq_phone = ifelse(str_detect(uniq_no, "NA"), NA, uniq_no)
  ) %>%
  arrange(name)

saveRDS(directory_online, file = "data/rda/online_directory.rds", compress = T)


user_meta = tbl(con, 'dev_usermeta') %>% collect()
DBI::dbDisconnect(con)

user_meta %>%
  select(-umeta_id) %>%
  spread(meta_key, meta_value) %>%
  select(user_id, starts_with('billing')) -> user_directory

##%######################################################%##
#                                                          #
####        Sanskriti Online Directory (from WP)        ####
#                                                          #
##%######################################################%##
library(tidyverse)
library(readxl)
online <- read_excel('data/raw/OnlineMembership Aug 2018.xlsx') %>%
  set_tidy_names(syntactic = TRUE) %>% # Use make.names to make classically valid names
  set_names(tolower(names(.))) # Make everything lower case

## Normalizing this data

online <- online %>%
  mutate_at(vars(last.name, first.name, spouse), str_to_title) %>%
  mutate(fullnames = paste(last.name, first.name, sep = ', ')) %>%
  mutate(fullnames = ifelse(!is.na(spouse), paste(fullnames, spouse, sep = ' & '), fullnames)) %>%
  arrange(fullnames) %>%
  select(fullnames, everything())

states <- online$state %>% str_to_title()
states <- case_when(
  states == 'District Of Columbia' ~ 'DC',
  states == 'Maryland' ~ 'MD',
  states == 'Virginia' ~ 'VA',
  states == 'West Bengal' ~ 'WB',
  TRUE ~ states
  ) %>%
  str_to_upper()
online$state <- states

phones <- str_match_all(online$phone, regex_phone) %>%
  map(~apply(., 1, std_phone_format)) %>%
  map_chr(paste, collapse=', ')
phones[phones == 'NA-NA-NA'] <- NA
online$phone <- phones


tmpnames = normalized_data %>%
mutate(tmpnames = str_match(FullName, '^([A-Za-z, \\-]+)[ \\&]*')[,2] %>% str_trim) %>%
pull(tmpnames)
