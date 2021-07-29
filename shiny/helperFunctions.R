library(tidyverse)
library(magrittr)

# graphics ----
makeGraph <- function(frames) {
  if (is.null(frames)) {
    out <- ggplot(tibble(x = 1:10, y = 1:10), aes(x, x)) +
      theme_void() +
      annotate('text',
               5,
               5,
               label = 'Please load file.',
               color = 'darkcyan',
               size = 12)
  } else{
    rectFrame <-
      tibble(
        xmin = rep(0, 4),
        xmax = rep(max(frames$frameNr), 4),
        ymin = rep(c(min(frames$value), 2), times = 2),
        ymax = rep(c(2, max(frames$value)), times = 2),
        name = rep(c('code', 'filtered'), each = 2),
        fill = rep(c('Transit', 'Aktion'), times = 2)
      )
    out <- frames %>%
      ggplot(aes(x = frameNr, y = value, color = label)) +
      geom_rect(
        data = rectFrame,
        aes(
          x = 0,
          y = 0,
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = fill
        ),
        color = 'white',
        alpha = .1,
        show.legend = F
      ) +
      geom_line(color = 'darkgrey') +
      geom_point(size = .05) +
      facet_wrap( ~ name) +
      theme_minimal() +
      geom_point(aes(y = min(frames$value) - .1, color = filCode), size = .1) +
      geom_point(aes(y = min(frames$value) - .2, color = phase), size = .1) +
      scale_color_manual(
        values = c(
          'Action' = 'cornflowerblue',
          'Transition' = 'chartreuse4',
          'Overlap' = 'darkcyan',
          'Pause' = 'white'
        )
      ) +
      scale_fill_manual(values = c('Aktion' = 'blue', 'Transit' = 'green')) +
      scale_x_continuous(sec.axis = dup_axis(trans = ~ . / 30, name = 'time[s]'),
                         name = 'frames') +
      annotate(
        'text',
        x = mean(frames$frameNr),
        y = c(min(frames$value) - .05),
        label = c('algorithm'),
        color = '#dddddd'
      ) +
      annotate(
        'text',
        x = mean(frames$frameNr),
        y = c(min(frames$value) - .25),
        label = c('human'),
        color = '#dddddd'
      ) +
      theme(strip.background.x = element_rect(fill = '#dddddd', color = 'white')) +
      ylim(min(frames$value) - .4, max(frames$value)) +
      labs(color = '')
  }
  
  return(out)
}

# datahandling ----
removeLastBreaks <- function(x) {
  for (i in seq_along(x)) {
    if (rev(x)[i] != 0) {
      break
    }
  }
  i <- i - 1
  return(rep(c(T, F), times = c(length(x) - i, i)))
}


makeDataAccessible <- function(dataPath) {
  if (!is.null(dataPath)) {
    data <- openxlsx::readWorkbook(dataPath)
    if (ncol(data) == 4) {
      data %<>% rename(
        'Onset_Time' = 1,
        'Offset_Time' = 2,
        'AllCodes' = 3,
        'Phase' = 4
      )
    } else{
      data %<>% rename(
        'Onset_Time' = 1,
        'Offset_Time' = 2,
        'AllCodes' = 3
      ) %>% mutate(Phase = 0)
    }
    frames <- data %>%
      mutate(
        Onset_Time = as.character(Onset_Time),
        Offset_Time = as.character(Offset_Time),
        AllCodes = as.numeric(AllCodes),
        Phase = as.numeric(Phase)
      ) %>%
      filter(!is.na(AllCodes) & removeLastBreaks(AllCodes)) %>%
      mutate(
        Onset_Frame = as.numeric(str_extract(Onset_Time, '\\w{2}$')),
        Offset_Frame = as.numeric(str_extract(Offset_Time, '\\w{2}$')) + 1 ,
        Onset_Time = str_remove(Onset_Time, '\\W\\w{2}$'),
        Offset_Time = str_remove(Offset_Time, '\\W\\w{2}$')
      ) %>%
      mutate(
        Onset_Time = lubridate::seconds(as.POSIXct(
          strptime(Onset_Time, format = '%H:%M:%S')
        )) * 30,
        Offset_Time = lubridate::seconds(as.POSIXct(
          strptime(Offset_Time, format = '%H:%M:%S')
        )) * 30,
        Frame_Count = as.numeric((Offset_Time + Offset_Frame) - (Onset_Time + Onset_Frame))
      ) %$%
      tibble(
        code = rep(AllCodes, Frame_Count),
        phase = rep(Phase, Frame_Count),
        frameNr = seq_along(phase)
      ) %>%
      mutate(
        label = recode(code + 1, 'Pause', 'Transition', 'Overlap', 'Action'),
        phase = recode(phase, '1' = 'Transit', '3' = 'Aktion')
      )
  } else{
    frames <- NULL
  }
  return(frames)
}


retime_data <- function(data){
  data %<>% filter(name == 'filtered')
  numCodes <- data %>%
    .[['filCode']] %>% 
    as_factor() %>% 
    as.numeric()
  changes <- which(numCodes - lag(numCodes)<0)
  phaseNr <- (changes - lag(changes)) %>% 
    replace_na(changes[1]-1) %>% 
    c(.,length(numCodes)-sum(.)) %>% 
    rep(1:(length(changes)+1), times = .) %>% 
    ifelse(nchar(.) == 1, paste0('0',.),.)
  data %>% 
    mutate(filCode = as_factor(paste0(filCode,'_', phaseNr))) %>% 
    group_by(filCode) %>% 
    summarise(time = n()) %>% 
    transmute(phase = as.character(filCode),
              set = str_extract(phase,'\\d+'),
              time = time/30)
}

apply_RuleSet <- function(data,timeCutOff = 20){
  data %<>% 
    mutate(phase = str_extract(phase,'[A-Za-z]+'))
  i <- 1
  while(i<nrow(data)){
    if(data$time[i] < timeCutOff){
      data$time[i-1] <- sum(data$time[(i-1):(i+1)])
      data[i:(i+1),3] <- 0
      i <- i+1
    }
    i <- i+1
  }
  data <- data[data$time>0,] %>% 
    mutate(phase = factor(phase,levels = c('Transit', 'Aktion')),
           cor = as.numeric(phase),
           cor = cor - lag(cor,default = -1))
  if(any(data$cor == 0)){
    data[which(data$cor == 0) - 1,'time'] <- sum(data[-1:0 + which(data$cor == 0),'time'])
    data %<>% .[-1*which(.$cor==0),-4]
  }else{
    data %<>% .[,-4]
  }
  if(table(data$phase)['Aktion'] > table(data$phase)['Transit']){
    data <- bind_rows(tibble(phase = 'Transit', set = '01', time = 0),
                      data)
  }
  data %>% 
    mutate(set = rep(1:ceiling(length(set)/2),each = 2)[1:length(set)],
           set = ifelse(set<10,paste0(0,set),as.character(set)),
           phase = paste(phase,set,sep = '_'))
}



takeTimesAndAddStuff <- function(data){
  addition <- data %>%  
    group_by(set) %>%  
    summarise(Verh = divide_by(time[1],time[2]),
              Dauer = sum(time)) %>% 
    pivot_wider(names_from = set,values_from = c(Verh, Dauer)) %>% 
    mutate(Anzahl_Phasen = nrow(data),
           Dauer = sum(.[,str_detect(names(.),'Dauer')]),
           Frequenz = Dauer,
           Streuung_GP = sd(.[,str_detect(names(.),'Dauer_')],na.rm = T),
           Varianz_GP = var(as.numeric(.[,str_detect(names(.),'Dauer_')]),na.rm = T),
           M_PV = mean(as.numeric(.[,str_detect(names(.),'Verh')]),na.rm = T),
           Streuung_PV = sd(.[,str_detect(names(.),'Verh')],na.rm = T),
           Varianz_PV = var(as.numeric(.[,str_detect(names(.),'Verh')]),na.rm = T)) 
  data %>%  select(phase, time) %>% 
    pivot_wider(names_from = phase, values_from = time) %>% 
    bind_cols(addition)
}



# smoothing ----

localMeaner <- function(frames, kernel = 3) {
  if (is.null(frames)) {
    NULL
  } else{
    kernelPoints <- dnorm(x = seq(-2, 2, length.out = kernel))
    frames %>%  mutate(
      filtered = na.omit(stats::filter(
        c(code[1:(floor(kernel / 2) - 1)],
          code,
          code[(length(code) - ceiling(kernel /
                                         2) + 1):length(code)]),
        kernelPoints / sum(kernelPoints)
      )),
      filtered = ifelse(is.na(filtered), code, filtered),
      filCode = ifelse(filtered > 2, 'Action', 'Transition')
    ) %>%
      pivot_longer(cols = c('code', 'filtered'))
  }
}


meanWithPars <- function(frames, oldPars, newPars) {
  frames %>%
    mutate(
      code = ifelse(code == oldPars[[1]], newPars[[1]], code),
      code = ifelse(code == oldPars[[2]], newPars[[2]], code)
    ) %>%
    localMeaner(kernel = newPars[[3]])
}


stackMeanNewPars <- function(frameList, ops, nps) {
  require(parallel)
  require(pbapply)
  relPar <- which(sapply(nps, length) > 1)
  print(paste('adjusting', names(nps)[relPar]))
  pblapply(nps[[relPar]], function(parRep) {
    dummyPars <- nps
    dummyPars[[relPar]] <- parRep
    mclapply(frameList,
             function(frames) {
               meanWithPars(frames, ops, dummyPars)
             },
             mc.cores = 6)
  })
}


makeParWindow <- function(x,acc,windowSize = 9,relPar,rnd = F) {
  options(scipen = 999)
  if (nchar(as.character(x)) > 1) {
    startDec <- unlist(str_split(as.character(x), '\\.'))
    startDec <-
      ifelse(
        length(startDec) > 1,
        -1 * str_count(startDec[2], '\\d'),
        nchar(startDec[1]) - min(c(
          unlist(str_locate_all(startDec[1], '0+$')), nchar(startDec[1]) - 2
        )) + 1
      ) - acc
  } else{
    startDec <- 0 - acc
  }
  dist <- floor(windowSize / 2) * 10 ^ startDec
  ret <-
    seq(x - dist, x + dist, length.out = windowSize)
  if (rnd) {
    if (relPar != 3) {
      ret[sample(c(1:(mean(1:windowSize) - 1), (mean(1:windowSize) + 1):windowSize), floor(.5 *
                                                                                             windowSize), F)] <-
        sample(-10:10, floor(.5 * windowSize), T)
    } else{
      ret[sample(c(1:(mean(1:windowSize) - 1), (mean(1:windowSize) + 1):windowSize), floor(.5 *
                                                                                             windowSize), F)] <-
        sample(10:10000, floor(.5 * windowSize), T)
    }
  }
  if (relPar == 3) {
    ifelse(ret < 1, 10, round(ret))
  } else{
    ret
  }
}


optiSol <- function(oldPars,frames,its,windowSize = 9,goodnessAim = .95) {
    its <- its * 3
    acc <- c(0, 0, 0)
    i <-  1
    origpars <- oldPars
    pars <- oldPars
    goodness <-
      matrix(as.numeric(rep(NA, its * windowSize)), ncol = windowSize)
    ranger <- matrix(as.numeric(rep(NA, its * 2)), ncol = 2)
    while (i < its) {
      relPar <- ifelse(i %% 3 == 0, 3, i %% 3)
      pars[[relPar]] <-
        makeParWindow(
          x = oldPars[[relPar]],
          acc = acc[relPar],
          windowSize = windowSize,
          relPar = relPar
        )
      res <-
        stackMeanNewPars(frameList = frames,
                         ops = origpars,
                         nps = pars)
      eval <-
        parallel::mclapply(res, function(frameList)
          evalIt(frameList, irr = T)$goodness,
          mc.cores = 6)
      goodness[i,] <-
        sapply(eval, function(values)
          min(values))
      selection <- which(goodness[i,] == max(goodness[i,]))[1]
      ranger[i, ] <-
        lapply(eval, function(values)
          range(values))[[selection]]
      matplot(goodness,
              ylim = c(-1, 1),
              ylab = "Krippendorff's alpha",
              xlab = 'iteration')
      abline(h = goodnessAim, col = 'red')
      ms <- sapply(data.frame(t(goodness)), max)
      lines(ms, col = 'darkgrey')
      points(x = 1:its, y = ms)
      segments(x0 = 1:its,
               y0 = ranger[, 1],
               y1 = ranger[, 2])
      if (oldPars[[relPar]] == pars[[relPar]][selection]) {
        acc[relPar] <- acc[relPar] + 1
      } else{
        acc[relPar] <- 0
      }
      oldPars[[relPar]] <- pars[[relPar]][selection]
      pars <- oldPars
      if (max(goodness[i, ]) >= goodnessAim) {
        break
      }
      i = i + 1
    }
    return(pars)
    options(scipen = 0)
  }


fast_kripp_alpha <- function(mat) {
  lev <- levels(factor(mat))
  n <- length(mat)
  obs <-
    table(data.frame(
      x = factor(mat[, 1], levels = lev),
      y = factor(mat[, 2], levels = lev)
    ), useNA = 'always')
  obs <- obs + t(obs)
  o <-
    obs[rep(1:nrow(obs), each = nrow(obs)) == rep(1:nrow(obs), times = nrow(obs))]
  nc <- rowSums(obs)
  ((n - 1) * sum(o) - sum(nc * (nc - 1))) / (n * (n - 1) - sum(nc * (nc -
                                                                       1)))
}


evalIt <- function(frameList,irr = F,all = F) {
  require(magrittr)
  # take all solutions as one and calculate goodness
  ret <- frameList %>%
    bind_rows(.id = 'origin') %>%
    filter(name == 'filtered') %>%
    transmute(
      filCode = as.numeric(factor(filCode)),
      phase = as.numeric(factor(phase)),
      origin = origin
    )
  if (!all) {
    ret %<>%
      mutate(correct = filCode == phase) %>%
      group_by(origin)
    if (!irr) {
      ret %>% summarise(goodness = sum(correct) / length(correct))
    } else{
      ret %>% summarise(goodness = matrix(c(filCode, phase), ncol = 2, byrow = F) %>% fast_kripp_alpha())
    }
  } else{
    ret %>% transmute(goodness = matrix(c(filCode, phase), ncol = 2, byrow = F) %>% fast_kripp_alpha()) %$% goodness[1]
  }
}



