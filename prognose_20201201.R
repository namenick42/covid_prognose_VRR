

# opzet -------------------------------------------------------------------

# Rstudio > new project > version controle > https://github.com/namenick42/covid_prognose_VRR/
# tab files:
# New folder "data"
# New folder "resultaat"
# tab packages
# packages hieronder installeren (alles met library)
# Eerste keer RIVM handmatig doen.
# infectie_data_raw <- import("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv") 
# export(infectie_data_raw, "data/COVID-19_aantallen_gemeente_per_dag.csv")


# borging laatste versie --------------------------------------------------
# let op, eigen wijzigen gaan verloren. 
system("git pull --force")

# packages ----------------------------------------------------------------

library(tidyverse)
options(dplyr.summarise.inform=F)
library(lubridate)
library(rio)
library(tidyquant)
library(zoo)
library(janitor)



# data laden --------------------------------------------------------------

# infecties RIVM

if(file.info("data/COVID-19_aantallen_gemeente_per_dag.csv")$mtime < paste0(Sys.Date(), " 14:30:00 CET") & Sys.time() > paste0(Sys.Date(), " 14:30:00 CET")) {
  infectie_data_raw <- import("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv") 
  export(infectie_data_raw, "data/COVID-19_aantallen_gemeente_per_dag.csv")
} else {
  infectie_data_raw <- import("data/COVID-19_aantallen_gemeente_per_dag.csv")
}





# bekijken RIVM gegevens --------------------------------------------------

infectie_data_raw %>% 
  filter(Security_region_code %in% c("VR17")) %>% # eventueel andere VR's vergelijken. 
  group_by(datum = Date_of_publication, VR = Security_region_code) %>% 
  summarise(
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased)
  ) %>% 
  group_by(VR) %>% 
  mutate(
    infecties = zoo::rollmean(Total_reported, k = 7, fill = NA),
    opnames = zoo::rollmean(Hospital_admission, k = 7, fill = NA),
    overlijden = zoo::rollmean(Deceased, k = 7, fill = NA)
  ) %>%
  filter(!is.na(Total_reported)) %>% 
  pivot_longer(cols=c(infecties, opnames, overlijden), names_to = "soort", values_to = "aantal") %>% 
  mutate(
    soort = factor(soort, levels = c("infecties","opnames","overlijden"))
  ) %>%
  ggplot(aes(datum, aantal, color = VR)) +
  geom_line(size = 1) +
  facet_wrap(~soort, ncol = 1, scales = "free") +
  coord_x_date(xlim = c("2020-07-01", "2020-12-01")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W", minor_breaks = NULL) + 
  ggtitle("RIVM corona VRR", "voortschrijdend gemiddelde 7 dagen") +
  #scale_y_log10() + # eventueel LOG as.
  xlab("weken") +
  theme_bw()




# wrangling ---------------------------------------------------------------
# data in de goede vorm zetten

infectie_data <-
  infectie_data_raw %>% 
  filter(
    Security_region_code %in% c("VR17")
  ) %>% 
  group_by(datum = Date_of_publication, VR_code = Security_region_code) %>% 
  summarise(
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased)
  ) %>% 
  filter(!is.na(Total_reported)) %>% 
  mutate(
    datum = as_date(datum), 
    dagen = yday(datum),
    deel = "infectie"
  ) 





# parameters --------------------------------------------------------------

vr_selectie = "VR17" # "VR17", "VR18",  "VR19"  # 18 zhz, 19 zee
vanaf_datum = as_date("2020-07-01")

aantal_sim <- 10

dagen_projecteren <- 50      # aantal dagen voorbij vandaag

r = 1.02                    # R0 waarde voor projectie.
r_dag <- 1-(1-r)/7            # RO per dag maken, r geldt voor incubatie van 7 dagen

sceanrio = paste0("R", r)     # de naam komt van de R. Bij andere variatie moet er een andere naam gekozen worden. 

factor_pop_ha_centa = 0.0016  # prop uit infectueus RIVM naar corona
factor_pop_zh = 0.036        # prop uit infectueus RIVM naar ziekenhuis
factor_zh_centra = 0.2        # prop uit ziekenhuis naar corona

vertraging_opname_ha <- 0     # dagen vertraging tussen infectueus RIVM en HA/centra
vertraging_opname_zk <- 5    # dagen vertraging tussen infectueus RIVM en ZA

gem_ligduur_zh <- 9.6           #dagen ligduur ziekenhuis als black box.
gem_ligduur_centra <- 17      # dagen ligduur coronacentra op basis van zorghotel.




# projectie infecties -----------------------------------------------------
# Aanvullen RIVM infecties op basis van R waarde.

infectie_selectie <-
  infectie_data %>% 
  filter(
    VR_code == vr_selectie,
    datum >= vanaf_datum
  )

projectie_data <-
  infectie_selectie %>% 
  filter(
    VR_code == vr_selectie,
    datum >= vanaf_datum
  ) %>% 
  bind_rows(
    tibble(
      datum = seq(max(infectie_selectie$datum)+1, max(infectie_selectie$datum) + dagen_projecteren, 1),
      dagen = yday(datum),
      deel = "projectie",
      Total_reported = mean(last(infectie_selectie$Total_reported, n=3), na.rm = T) * r_dag^(1:dagen_projecteren), # doortellen met R0.
      Hospital_admission = mean(last(infectie_selectie$Hospital_admission, n=3), na.rm = T) * r_dag^(1:dagen_projecteren)
    )
  ) 





# simulaties --------------------------------------------------------------

for(i in 1:aantal_sim) {
  
  # stromen maken -----------------------------------------------------------
  
  instoom <-    
    projectie_data %>% 
    mutate(
      fractie_huisarts = (Total_reported * factor_pop_ha_centa), # fractie dat gebruikt wordt als instroom
      huisarts_naar_centra = rpois(length(fractie_huisarts), fractie_huisarts),
      fractie_ziekenhuis = (Total_reported * factor_pop_zh),
      infectie_naar_ziekenhuis = rpois(length(fractie_ziekenhuis), fractie_ziekenhuis),
      ziekenhuis_naar_centra = round(infectie_naar_ziekenhuis * factor_zh_centra)
    ) %>% 
    select(datum, huisarts_naar_centra, ziekenhuis_naar_centra, infectie_naar_ziekenhuis) %>% 
    pivot_longer(-datum, names_to = "stroom", values_to = "aantal")
  
  
  # patient simulatie -------------------------------------------------------
  
  patienten <-
    tibble(
      id = seq(1, sum(instoom$aantal), 1),
      instroom_datum = as_date(rep(as.character(instoom$datum), instoom$aantal)),
      stroom = (rep(as.character(instoom$stroom), instoom$aantal)),
      opname_centra = case_when(
        stroom == "infectie_naar_ziekenhuis" ~ instroom_datum + rpois(length(stroom == "infectie_naar_ziekenhuis"), vertraging_opname_zk),
        stroom == "huisarts_naar_centra" ~ instroom_datum + rpois(length(stroom == "huisarts_naar_centra"), vertraging_opname_ha),
        stroom == "ziekenhuis_naar_centra" ~ instroom_datum + rpois(length(stroom == "uitstroom_ziekenhuis"), vertraging_opname_zk) + rpois(length(stroom == "uitstroom_ziekenhuis"), gem_ligduur_zh),
        TRUE ~ instroom_datum
      ),
      ontslagen_centra = case_when(
        stroom == "infectie_naar_ziekenhuis" ~  opname_centra + rpois(length(id), gem_ligduur_zh),
        stroom != "infectie_naar_ziekenhuis" ~  opname_centra + rpois(length(id), gem_ligduur_centra),
        TRUE ~ opname_centra
      )
    )
  
  # verwerken resultaat -----------------------------------------------------
  
  resultaat <-
    tibble(
      datum = as_date(seq(as_date("2020-07-01"), max(infectie_data$datum) + dagen_projecteren, 1))
    ) %>% 
    left_join(
      patienten %>% filter(stroom == "infectie_naar_ziekenhuis") %>% group_by(datum = opname_centra) %>% summarise(instroom_naar_ziekenhuis = n()), by = "datum"
    ) %>% 
    left_join(
      patienten %>% filter(stroom == "infectie_naar_ziekenhuis") %>% group_by(datum = ontslagen_centra) %>% summarise(uitstroom_uit_ziekenhuis = n()), by = "datum"
    ) %>% 
    left_join(
      patienten %>% filter(stroom == "ziekenhuis_naar_centra") %>% group_by(datum = opname_centra) %>% summarise(instroom_ziekenhuis = n()), by = "datum"
    ) %>% 
    left_join(
      patienten %>% filter(stroom == "ziekenhuis_naar_centra") %>% group_by(datum = ontslagen_centra) %>% summarise(uitstroom_ziekenhuis = n()), by = "datum"
    ) %>% 
    left_join(
      patienten %>% filter(stroom == "huisarts_naar_centra") %>% group_by(datum = opname_centra) %>% summarise(instroom_huisarts = n()), by = "datum"
    ) %>% 
    left_join(
      patienten %>% filter(stroom == "huisarts_naar_centra") %>% group_by(datum = ontslagen_centra) %>% summarise(uitstroom_huisarts = n()), by = "datum"
    ) %>% 
    left_join(
      projectie_data %>% select(datum, deel), by = "datum"
    ) %>% 
    replace_na(list(instroom_naar_ziekenhuis = 0, uitstroom_uit_ziekenhuis = 0, instroom_ziekenhuis = 0, uitstroom_ziekenhuis = 0, instroom_huisarts = 0, uitstroom_huisarts = 0, deel = "projectie")) %>% 
    mutate(
      in_ziekenhuis = cumsum(instroom_naar_ziekenhuis) - cumsum(uitstroom_uit_ziekenhuis),
      instroom_totaal = instroom_ziekenhuis + instroom_huisarts,
      uitstroom_totaal = uitstroom_ziekenhuis + uitstroom_huisarts,
      stroom_zh = cumsum(instroom_ziekenhuis) - cumsum(uitstroom_ziekenhuis),
      stroom_ha = cumsum(instroom_huisarts) - cumsum(uitstroom_huisarts),
      in_centra_totaal = cumsum(instroom_totaal) - cumsum(uitstroom_totaal),
      run = i,
      vr_code = vr_selectie,
      sceanrio = sceanrio
    ) 
  
  # opslaan per keer
  saveRDS(resultaat, paste0("resultaat/", vr_selectie, "_", sceanrio, "_", make_clean_names(as.character(Sys.Date())), "_", i ))
  
  print(i)
  
}



# read, bind  -----------------------------------------------------------

verzameling_resultaten <- 
  list.files("resultaat/", full.names = T, pattern = make_clean_names(as.character(Sys.Date())) ) %>% # filter alleen vandaag
  map_dfr(readRDS) %>% 
  mutate(id = paste0(vr_code, sceanrio, run))

aggregatie_resultaten <-
  verzameling_resultaten %>% 
  group_by(datum, vr_code, sceanrio) %>% 
  summarise(
    in_centra_totaal = as.integer(mean(in_centra_totaal)),
    in_ziekenhuis = as.integer(mean(in_ziekenhuis)),
    stroom_zh = as.integer(mean(stroom_zh)),
    stroom_ha = as.integer(mean(stroom_ha)),
    deel = first(deel)
  ) %>% 
  ungroup()


# plot vrr -----------------------------------------------------------------

plot_data <-
  aggregatie_resultaten %>% 
  filter(vr_code == "VR17") %>% 
  select(-deel) %>% 
  pivot_longer(-c(datum, vr_code, sceanrio), names_to = "type", values_to = "prog") %>% 
  filter(type == "in_centra_totaal")

ggplot() +  
  geom_line(aes(datum, in_centra_totaal, group = id), alpha = 0.5, color = "grey85", data = verzameling_resultaten %>% filter(vr_code == "VR17"), size = 0.5) +
  geom_step(aes(datum, prog, color = sceanrio), data = plot_data, size = 1, show.legend = FALSE) +
  directlabels::geom_dl(aes(datum, prog, label = sceanrio), method = "last.qp", data = plot_data) +
  geom_vline(xintercept = max(verzameling_resultaten %>% filter(deel == "infectie") %>% .$datum), linetype = "dashed") +
  ggtitle(
    paste0("Prognose coronacentra beddencapaciteit VRR"), 
    paste0("op basis van RIVM infecties, projectie met R. De dato ", Sys.Date())
  ) +
  ylab("bedden") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%m", minor_breaks = NULL) + 
  theme_bw() +
  labs(caption = paste0("instr.HA=",factor_pop_ha_centa, "  vert.HA=",vertraging_opname_ha , "  instr.ZH=",factor_pop_zh, "  vert.ZH=",vertraging_opname_zk, "  ZH > Cn=",factor_zh_centra,"  ligd.ZH=",gem_ligduur_zh,"  ligd.Ce=",gem_ligduur_centra))


# plaatje uniform opslaan
ggsave(paste0("VRR_", Sys.Date(), "prognose_beddruk.png"), width = 35, height = 20, units = c("cm"), dpi = 400)











