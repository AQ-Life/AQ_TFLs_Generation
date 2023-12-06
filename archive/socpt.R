# TEAE by SOC and PT

SOCPTTable <- function(indata){

#calculate big N from ADSL
bign <- rbind.fill(filter(adsl,
                           SAFFL == "Y"),
                    filter(mutate(adsl,
                                  TRT01AN = 9),
                           SAFFL == "Y")) %>% 
  group_by(TRT01AN) %>% 
  dplyr::summarise(bign = n_distinct(USUBJID)) %>% 
  dplyr::rename(TRTAN = TRT01AN)

adae1 <- rbind.fill(filter(indata,
                           SAFFL == "Y"), 
                    filter(mutate(indata,
                                  TRTAN = 9),
                           SAFFL == "Y"))

#calculate the freq of any teae
teae_any <- adae1 %>% 
  group_by(TRTAN) %>% 
  dplyr::summarise(n = n_distinct(USUBJID)) %>% 
  ungroup() %>% 
  mutate(ord = 0,
         ord_soc = 0,
         AEBODSYS = "Any TEAE")

#calculate the freq by SOC 
teae_soc <- adae1 %>% 
  group_by(TRTAN, AEBODSYS) %>% 
  dplyr::summarise(n = n_distinct(USUBJID), .groups = "keep") %>% 
  ungroup()

teae_soc_ord <- teae_soc %>% 
  filter(TRTAN == 9) %>% 
  arrange(desc(n), AEBODSYS) %>% 
  dplyr::mutate(ord_soc = row_number())

teae_soc1 <- left_join(teae_soc, 
                       select(teae_soc_ord, AEBODSYS, ord_soc),
                       by = c("AEBODSYS")) %>% 
  mutate(ord = 1)

#calculate the freq by SOC and PT 
teae_pt <- adae1 %>% 
  group_by(TRTAN, AEBODSYS, AEDECOD) %>% 
  dplyr::summarise(n = n_distinct(USUBJID), .groups = "keep") %>% 
  ungroup()

teae_pt_ord <- teae_pt %>% 
  filter(TRTAN == 9) %>% 
  left_join(select(teae_soc_ord,AEBODSYS, ord_soc),
            by = c("AEBODSYS")) %>% 
  arrange(ord_soc, desc(n), AEDECOD) %>% 
  dplyr::mutate(ord_pt = row_number())


teae_pt1 <- left_join(teae_pt,
                      select(teae_pt_ord, AEBODSYS, AEDECOD, ord_soc, ord_pt),
                      by = c("AEBODSYS", "AEDECOD")) %>% 
  mutate(ord = 2) %>% 
  arrange(ord_soc, ord_pt)

#combine SOC and PT
final <- rbind.fill(teae_any, teae_soc1, teae_pt1) %>% 
  left_join(bign,
            by = c("TRTAN")) %>% 
  mutate(percent = round(100*n/bign, 1),
         col = str_c(as.character(n),
               " (",
               as.character(percent),
               ")"),
         item = case_when(is.na(AEDECOD) ~ AEBODSYS,
                          TRUE ~ str_c("  ", AEDECOD))) %>% 
  pivot_wider(id_cols = c(item, ord, ord_soc, ord_pt),
              names_from = TRTAN,
              values_from = col,
              names_prefix = "col",
              values_fill = "0") %>% 
  arrange(ord_soc, ord, ord_pt) %>% 
  filter(ord_soc <= 4) %>% 
  group_split(ord_soc) %>% 
  map_dfr(~add_row(.x, .after = Inf)) %>% 
  select(item, starts_with("col"))

return(final)
}

