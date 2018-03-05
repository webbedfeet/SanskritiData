library(tidyverse)
wp_members <- read_tsv("data/raw/members_20180120.tsv")
wp_emails <- wp_members %>% filter(account_state == "active") %>% pull(email)

gmail1 <- read_csv("data/raw/community.csv", skip = 1)
gmail2 <- read_csv("data/raw/community1.csv", skip = 1)
gmail3 <- read_csv("data/raw/community2.csv", skip = 1)

gmail <- bind_rows(list(gmail1, gmail2, gmail3))
gmail <- gmail %>% set_names(make.names(names(.)))

Overall <- union(gmail$Email.address, wp_members$email)

not_in_gmail <- setdiff(wp_emails, gmail$Email.address)

# Life members
wp_members %>%
  filter(membership_level == 3) %>%
  select(member_id, first_name, last_name, member_since, email) %>%
  openxlsx::write.xlsx(
    file = "LifeMembers.xlsx", asTable = F,
    withFilter = FALSE
  )
