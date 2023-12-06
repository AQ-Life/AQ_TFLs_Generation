# TEAE by SOC and PT

SOCPTTable <- function(inpopdata, indata, inlevel1, inlevel2){

  inlevel1 <- enquo(inlevel1)
  inlevel2 <- enquo(inlevel2)

#calculate big N from ADSL
bign <- rbind.fill(inpopdata,
                  mutate(inpopdata,
                         TRT01AN = 9)) %>% 
  group_by(TRT01AN) %>% 
  dplyr::summarise(bign = n_distinct(USUBJID)) %>% 
  dplyr::rename(TRTAN = TRT01AN)


adae0_1 <- indata %>%
  mutate(bodsys_ = !!inlevel1,
         decod_ = !!inlevel2)

bodsys_data <- as.data.frame(as.vector(adae0_1$bodsys_))
bodsys_data <- bodsys_data %>% 
  rename(bodsys = colnames(bodsys_data))

decod_data <- as.data.frame(as.vector(adae0_1$decod_))
decod_data <- decod_data %>% 
  rename(decod = colnames(decod_data))

adae0 <- cbind(adae0_1, bodsys_data, decod_data)

adae1 <- rbind.data.frame(adae0, 
                          mutate(adae0,
                           TRTAN = 9))

#calculate the freq of any teae
teae_any <- adae1 %>% 
  group_by(TRTAN) %>% 
  dplyr::summarise(n = n_distinct(USUBJID)) %>% 
  ungroup() %>% 
  mutate(ord = 0,
         ord_soc = 0)

#calculate the freq by SOC 
teae_soc <- adae1 %>% 
  group_by(TRTAN, bodsys) %>% 
  dplyr::summarise(n = n_distinct(USUBJID), .groups = "keep") %>% 
  ungroup()

teae_soc_ord <- teae_soc %>% 
  filter(TRTAN == 9) %>% 
  arrange(desc(n), bodsys) %>% 
  dplyr::mutate(ord_soc = row_number())

teae_soc1 <- left_join(teae_soc, 
                       select(teae_soc_ord, bodsys, ord_soc),
                       by = c("bodsys")) %>% 
  mutate(ord = 1)

#calculate the freq by SOC and PT 
teae_pt <- adae1 %>% 
  group_by(TRTAN, bodsys, decod) %>% 
  dplyr::summarise(n = n_distinct(USUBJID), .groups = "keep") %>% 
  ungroup()

teae_pt_ord <- teae_pt %>% 
  filter(TRTAN == 9) %>% 
  left_join(select(teae_soc_ord,bodsys, ord_soc),
            by = c("bodsys")) %>% 
  arrange(ord_soc, desc(n), decod) %>% 
  dplyr::mutate(ord_pt = row_number())


teae_pt1 <- left_join(teae_pt,
                      select(teae_pt_ord, bodsys, decod, ord_soc, ord_pt),
                      by = c("bodsys", "decod")) %>% 
  mutate(ord = 2) %>% 
  arrange(ord_soc, ord_pt)

#combine SOC and PT
final <- rbindf(teae_any, teae_soc1, teae_pt1) %>%
  left_join(bign,
            by = c("TRTAN")) %>%
  mutate(percent = round(100*n/bign, 1),
         col = str_c(as.character(n),
               " (",
               as.character(percent),
               ")"),
         item = case_when(is.na(bodsys) ~ "Any TEAE",
                          is.na(decod) ~ bodsys,
                          TRUE ~ str_c("  ", decod))) %>%
  pivot_wider(id_cols = c(item, ord, ord_soc, ord_pt),
              names_from = TRTAN,
              values_from = col,
              names_prefix = "col",
              values_fill = "0") %>%
  arrange(ord_soc, ord, ord_pt) %>%
  group_split(ord_soc) %>%
  map_dfr(~add_row(.x, .after = Inf)) %>%
  select(item, starts_with("col"))

return(final)
}

