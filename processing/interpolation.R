## To interpolate between gcam years 

withdrawal <- rbind(getQuery(prj_gfdl,'water withdrawals by water mapping source') %>% mutate(gcm="gfdl"),
                    getQuery(prj_ipsl,'water withdrawals by water mapping source')%>% mutate(gcm="ipsl"),
                    getQuery(prj_miroc,'water withdrawals by water mapping source')%>% mutate(gcm="miroc"),
                    getQuery(prj_hadgem,'water withdrawals by water mapping source')%>% mutate(gcm="hadgem"),
                    getQuery(prj_noresm,'water withdrawals by water mapping source')%>% mutate(gcm="noresm")) %>%
  dplyr::rename(withdrawal=value)

irr.withdrawal <- withdrawal[grep("irr", withdrawal$input), ]
irr.withdrawal <- irr.withdrawal %>%
  left_join(field.eff,by=c("region")) %>%
  mutate (withdrawal = withdrawal/conveyance.eff) %>%
  dplyr::select(-GCAM_region_ID, -field.eff, -conveyance.eff)

irr.withdrawal <- separate(irr.withdrawal, input, c('water','id','irr','basin','W'), sep="_") %>% group_by(scenario,gcm,year,region,basin) %>% summarise(withdrawal = sum(withdrawal)) %>% ungroup()

irr.withdrawal %>% dplyr::filter(year==2020) %>%
  rename(withdrawal.y0=withdrawal) %>%
  dplyr::select(-year) %>%
  left_join(irr.withdrawal %>% dplyr::filter(year==2100),
            by =c("scenario","gcm","region", "basin")) %>%
  replace_na(list(withdrawal=0)) %>%
  rename(withdrawal.yn=withdrawal) %>%
  dplyr::select(-year) %>%
  left_join(irr.withdrawal %>% dplyr::filter(year>2020&year<2100) %>%
              replace_na(list(withdrawal=0)) %>%
              group_by(scenario,gcm,region, basin) %>%
              summarise(withdrawal=sum(withdrawal)) %>%
              ungroup(), by=c("scenario","gcm","region", "basin")) %>%
  group_by(scenario,gcm,basin) %>%
  summarise(withdrawal.y0 = sum(withdrawal.y0),
            withdrawal.yn = sum(withdrawal.yn),
            withdrawal = sum(withdrawal)) %>%
  ungroup() %>%
  mutate(cumsum = 5 * (((withdrawal.y0 + withdrawal.yn)/2) + withdrawal)) %>%
  #mutate(cumsum = (5/2) * (withdrawal.y0 + withdrawal.yn) * withdrawal) %>%
  left_join(basin.id.swap, by=c("basin"="basin.name"))%>% mutate(basin.name.old = gsub(" ","_",basin.name.old)) %>% replace_na(list(basin.name.old="Madasgacar")) %>% na.omit() %>% rename(subRegion=basin.name.old) %>%
  dplyr::select(-basin)->
  y