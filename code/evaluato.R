
source('../shiny/helperFunctions.R')

all_frames <-
  list.files('data/Auswertungsdateien/',
             recursive = T,
             pattern = 'P.xlsx',
             full.names = T)%>% 
  map(~makeDataAccessible(.))



origPars <- list(Pause = 0,
                 Übergang = 2,
                 Kernel = 5000)

# find optimal solution
png('imgs/all_opt.png')
all_optiPars <- optiSol(oldPars = origPars,frames =  all_frames,its =  15, windowSize = 11)
dev.off()


# all_optiPars <- list(Pause = 2.089787,
#                     Übergang = 2.196899895,
#                     Kernel = 2500) 

res <- all_frames %>%  map(~meanWithPars(frames = ., oldPars =  origPars, newPars =  all_optiPars))
all_kripp <- evalIt(res,T,T)
all_kripp_indiv <- evalIt(res,T,F)


fs <- list.files('data/Auswertungsdateien/',
                 recursive = T,
                 pattern = '.xlsx',
                 full.names = T)

all_frames <- fs %>% 
  map(~makeDataAccessible(.))
names(all_frames) <- str_extract(fs,'\\w\\d{1,2}')

res <- all_frames %>%  map(~meanWithPars(frames = ., oldPars =  origPars, newPars =  all_optiPars))


times <- res %>% 
  map(~retime_data(.)) %>% 
  map(~apply_RuleSet(.,20)) %>% 
  map(~takeTimesAndAddStuff(.)) %>% 
  .[names(.)[order(sapply(.,ncol),decreasing = T)]] %>% 
  bind_rows(.id = 'Datensatz') %>% 
  mutate_at(which(str_detect(names(.),('Dauer'))),~replace_na(.,0)) %>% 
  mutate_all(~replace_na(.,'')) %>% 
  openxlsx::write.xlsx(file = 'all_results.xlsx')

