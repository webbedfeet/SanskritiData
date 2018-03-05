library(tidyverse)

## %######################################################%##
#                                                          #
####            Sanskriti printed dictionary            ####
#                                                          #
## %######################################################%##

dat <- readLines("data/raw/Sanskriti_directory.md")
d <- strsplit(paste(dat, collapse = "\n"), "\n\n")[[1]]


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

dat <- tibble("orig" = d, name, phone_number, email, life_member, life_member_ind)
dat <- dat %>%
  mutate(address = orig %>%
    str_replace(fix_strings(name), "") %>%
    str_replace(fix_strings(phone_number), "") %>%
    str_replace(fix_strings(email), "") %>%
    str_replace("\\*\\*[Ll]ife [Mm]ember\\*\\*", "") %>%
    str_replace("^\n", "") %>%
    str_replace("[\n]+$", "") %>%
    str_replace("\n", ", ")) %>%
  mutate(pid = 1:nrow(.)) %>%
  select(pid, name, address, phone_number, email, life_member, life_member_ind) -> directory_printed

directory_printed <- directory_printed %>%
  mutate(names_limited = str_extract(name, "^[:alnum:]+, [:alnum:]+"))

readr::write_csv(directory_printed, path = "data/raw/Sanskriti_directory.csv")
openxlsx::write.xlsx(directory_printed, file = "data/raw/Sanskriti_directory.xlsx")

saveRDS(directory_printed, file = "data/rda/printed_directory.rds", compress = T)

## %######################################################%##
#                                                          #
####              Sanskriti web directory               ####
#                                                          #
## %######################################################%##

directory_db <- read_tsv("data/raw/members_20180120.tsv") %>%
  select(first_name, last_name, starts_with("address"), country, email, phone)
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

std_phone <- function(s) {
  x <- as.data.frame(str_match(s, "\\(*(\\d{3})[-\\.\\) ]*(\\d{3})[-\\. ]*(\\d{4})")[, 2:4])
  y <- x %>% mutate(no = glue("{V1}-{V2}-{V3}")) %>% pull(no)
  return(y)
}

directory_online <- directory_online %>%
  mutate(
    uniq_no = std_phone(uniq_no),
    uniq_no = ifelse(str_detect(uniq_no, "NA"), NA, uniq_no)
  ) %>%
  arrange(name)

saveRDS(directory_online, file = "data/rda/online_directory.rds", compress = T)
