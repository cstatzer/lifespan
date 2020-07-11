#############################################################
################ Install and load packages ##################
#############################################################

CRAN_packageList = c(
  "survival",
  "survminer",
  "tidyverse",
  "rms",
  "readxl",
  "ggplot2",
  "scales",
  "grDevices",
  "reshape2",
  "viridis",
  "gridExtra",
  "grid",
  "gridGraphics",
  "gplots",
  "statmod",
  "readr",
  "xml2",
  "pryr",
  "circlize",
  "tidyr",
  "kableExtra",
  "dplyr",
  "RColorBrewer",
  "xtable",
  "flexdashboard",   # tidypackages
  "shiny",
  #"Cairo",
  "shinythemes",
  "googleAuthR",
  "shinyjs",
  "data.table",
  "shinydashboard",
  "DT",
  "shinyDND",
  "shinyjqui",
  "shinyWidgets",
  "dygraphs",
  "xts",
  "colourpicker",
  "stringr",
  "readxl",
  "tm",
  "tinytex" # recommended for Rmd PDF output
  #"googleID" #(needs devtools to download from github)
  
)

biocPackageList <- c("biomaRt")

installPackages <- function(packageList=c(), biocPackageList=c(), loadAll=F){
  
  if ( length(setdiff(packageList, rownames(installed.packages()))) > 0 ) {
    install.packages(setdiff(packageList, rownames(installed.packages())))
  }
  
  if( length(setdiff(biocPackageList, rownames(installed.packages())))>0 ){source("http://bioconductor.org/biocLite.R")}
  for(package in setdiff(biocPackageList, rownames(installed.packages())) ){
    biocLite(package)
  }
  
  if(loadAll){
    for(package in packageList){library(package, character.only = TRUE)}
    for(package in biocPackageList){library(package, character.only = TRUE)}
  }
  
}

installPackages(packageList = CRAN_packageList,biocPackageList = biocPackageList,loadAll = TRUE)



#############################################################
####################  Import & Preprocess ###################
#############################################################

import_lifespan_csvs <- function(){
  # Import all csv files in folder
  wd <- getwd()
  csv_files <- list.files(wd,pattern = ".csv")
  csv_list <- lapply(csv_files, function(x) read.csv(x,stringsAsFactors = FALSE))
  data <- do.call(rbind,csv_list)
  return(data)
}

import_rename_columns <- function(upload){
  old <- c("Condition 1","Condition 2", "Condition 3", "Plate Name", "Plate Row", "Plate Column", "Plate Position Name")
  new <- c("Condition.1","Condition.2", "Condition.3", "Plate.Name", "Plate.Row", "Plate.Column", "Plate.Position.Name")
  idx <- which(old %in% colnames(upload))
  renamed <- setnames(upload, 
                      old = old[idx], 
                      new = new[idx])
  return(renamed)
}

exclude_autocomplete <- function(data,machine_vs_human){
  if(nrow(data) < 1) return(NULL)
  # add potentially missing columns
  if("Event Frequency" %in% colnames(data)) colnames(data)[which(colnames(data) == "Event Frequency")] <- "Frequency"
  if(!"Frequency" %in% colnames(data)) data$Frequency <- 1
  # Simplify column names
  colnames(data)[which(colnames(data) == "Experiment Temperature" )] <- "Temp."

  ### autocomplete if present columns are empty:
  if(all(is.na(data$Strain))) data$Strain <- "Strain"
  if(all(is.na(data$Condition.1))) data$Condition.1 <- "Condition.1"
  if(all(is.na(data$Condition.2))) data$Condition.2 <- "Condition.2"
  if(all(is.na(data$Device))) data$Device <- "Device"
  if(all(is.na(data$Plate.Name))) data$Plate.Name <- "Plate.Name"
  if(all(is.na(data$Experiment))) data$Experiment <- "Experiment.1"
  if(all(is.na(data$Temp.))) data$Temp. <- " "
  
  if (machine_vs_human %in% c("machine_only","combine_human_first")) {
    data <- data %>% mutate(Death = `Machine Death Age (Days)` %>% as.numeric(),
                            Cessation_Fast = `Machine Fast Movement Cessation Age (Days)`,
                            Cessation_Slow = `Machine Slow Movement Cessation Age (Days)`,
                            Duration_Slow = `Machine Slow Movement Duration (Days)`,
                            Duration_Posture = `Machine Posture Changing Duration (Days)`)
  }
  if (machine_vs_human == "combine_human_first") {
    data$Death[is.na(data$Death)] <- data$`By Hand Death Age (Days)`[is.na(data$Death)]
    data$Cessation_Fast[is.na(data$Cessation_Fast)] <- data$`Machine Fast Movement Cessation Age (Days)`[is.na(data$Cessation_Fast)]
    data$Cessation_Slow[is.na(data$Cessation_Slow)] <- data$`Machine Slow Movement Cessation Age (Days)`[is.na(data$Cessation_Slow)]
    data$Duration_Slow[is.na(data$Duration_Slow)] <- data$`Machine Slow Movement Duration (Days)`[is.na(data$Duration_Slow)]
    data$Duration_Posture[is.na(data$Duration_Posture)] <- data$`Machine Posture Changing Duration (Days)`[is.na(data$Duration_Posture)]
  }
  if (machine_vs_human == "human_only") {
    data <- data %>% mutate(Death = `By Hand Death Age (Days)`,
                            Cessation_Fast = `By Hand Fast Movement Cessation Age (Days)`,
                            Cessation_Slow = `By Hand Slow Movement Cessation Age (Days)`,
                            Duration_Slow = `By Hand Slow Movement Duration (Days)`,
                            Duration_Posture = `By Hand Posture Changing Duration (Days)`)
  }

  #### Data manipulation ### 
  if (!is.null(data$Excluded)) {
    data <- data[data$Excluded == 0,]   # remove excluded rows
    print("No observations were excluded")
  }
  
  # repeat every row as many times as stated in the frequency column
  data <- data[rep(row.names(data), data$Frequency), ]
  
  # censor animals which are still alive at the end of the experiment (Set Censor to 1 if the Death value is NA)
  data[is.na(data$Death),"Censored"] <- 1 # censor animals that outlive the experiment
  data[is.na(data$Death),"Death"] <- data[is.na(data$Death),"Cessation_Slow"]
  data[is.na(data$Death),"Death"] <- data[is.na(data$Death),"Cessation_Fast"]
  data$Death <- as.numeric(data$Death)
  
  # Max & Min thresholds (outsourced to function below)
  print(paste("nrow(data) before thresholding by plate:",nrow(data)))
  data_thresh <- Min_Max_plate_thr(data = data)
  print(paste("nrow(data_thresh):",nrow(data_thresh)))
  
  return(data)
}

apply_factor_levels <- function(data, levels_Strain,
                                levels_Condition.1, levels_Device){
  # Convert to factors, NO order specified
  data$Plate.Row = factor(as.character(data$Plate.Row))
  data$Plate.Column = factor(as.character(data$Plate.Column))
  
  # Convert to factors, WITH order specified
  if (!is.null(levels_Strain))        data$Strain <- factor(data$Strain, levels = levels_Strain)
  if (!is.null(levels_Condition.1))   data$Condition.1 <- factor(data$Condition.1, levels = levels_Condition.1)
  if (!is.null(levels_Device))        data$Device <- factor(data$Device, levels = levels_Device)
  
  # create a new combined factor
  levels_ID <- c()
  # Strain & Condition
  if (!is.null(levels_Strain) & !is.null(levels_Condition.1)) {
    for(s in levels_Strain){
      for (c in levels_Condition.1) {
        lvl <- paste(s,c,sep = "/")
        levels_ID <- c(levels_ID,lvl)
        print(lvl)
      }
    }
  }
  # only Strain
  if (!is.null(levels_Strain) & is.null(levels_Condition.1)) {
    levels_ID <- levels_Strain
  }
  # only Condition.1
  if (is.null(levels_Strain) & !is.null(levels_Condition.1)) {
    levels_ID <- levels_Condition.1
  }

  # create an ID variable and reorder it based on the input of levels_Strain and levels_Condition.1
  data$ID <- paste(data$Strain, data$Condition.1,sep = "/")
  data$ID <- factor(data$ID, levels = levels_ID)
  
  
  return(data)
  
}



#############################################################
##############  Colors & linetype & palettes ################
#############################################################

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

lighten_robust <- function(color, factor=2){
  col <- col2rgb(color)
  min_idx <- which(min(col) == col)
  min_idx <- min_idx[1]
  col[min_idx] <- col[min_idx] + factor
  col_out <- rgb(t(col), maxColorValue=255)
  col_out
}

# generate base palettes
create_base_palettes <- function(data,ctr.strain.col = "black",color_range = c(0.3,0.7),col_option = "viridis",direction = 1){
  # Strain color
  strain.col <- NULL
  if ( length(levels(data$Strain)) > 1 )   strain.col <- viridis(length(levels(data$Strain))-1,begin = color_range[1],end = color_range[2],alpha = 1,option = col_option,direction = as.numeric(direction))
  strain.col <- c(ctr.strain.col,strain.col)
  names(strain.col) <- levels(data$Strain)
  
  # Condition.1 color for additional plots
  Condition.1.col <- NULL
  if ( length(levels(data$Condition.1)) > 0 ) Condition.1.col <- rep("grey3",length(levels(data$Condition.1))-1)
  Condition.1.col <- c("black",Condition.1.col)
  names(Condition.1.col) <- levels(data$Condition.1)
  # alpha values
  Condition.1.alpha <- NULL
  if ( length(levels(data$Condition.1)) > 0 ) Condition.1.alpha <- rep(0.5,length(levels(data$Condition.1))-1)
  Condition.1.alpha <- c(1,Condition.1.alpha)
  names(Condition.1.alpha) <- levels(data$Condition.1)
  
  return(list(strain.col = strain.col, Condition.1.col = Condition.1.col, Condition.1.alpha = Condition.1.alpha))
}


# DEPRECIATED, use function make_col_palette
mix_pal_SCDPlates_fit <- function(data,strain.col,fit,factor = 0.01,prune_output = FALSE, strata_blacklist = c("Strain=|Condition.1=|Device=|Plate.Name=|Trial=")){
  # then build separate color and lty palettes, always for all strata containing the query string
  c <- fit
  strata <- names(c$strata)
  # strata <- str_replace_all(string = strata,pattern = strata_blacklist,replacement = "") # replace the added Strain= labels and others.
  # print(strata)
  col.pal <- c()
  if(is.null(strata)) return(list(col.pal = unname(strain.col),strata = NULL))
  for (i in 1:length(levels(data$Strain))) {
    # print(paste("i = ",i))
    strain = names(strain.col)[i]
    n <- strata[strata %in% strain]
    # col <- rep(strain.col[i],length(n))
    if (strain == levels(data$Strain)[1]) {
      #col.mod <- gray(seq(0,0.3,len = length(n)))
      col.mod <- sapply(1:length(n),function(x) lighten_robust(color = strain.col[i],factor = x))
    } else{
      col.mod <- sapply(1:length(n),function(x) lighten_robust(color = strain.col[i],factor = x))
      # col.mod <- sapply(seq(from = 1.0,to = 1 + factor * (length(n)-1), by = factor),function(x) lighten(color = unique(col),factor = x))
    }
    names(col.mod) <- n
    col.pal <- c(col.pal,col.mod)
    # print(paste("mix_pal_SCDPlates_fit ran successfully"))
  }
  if(prune_output){
    col.pal <- col.pal[names(col.pal) %in% strata]
  }
  return(list(col.pal = col.pal,strata = strata))
}  


make_col_palette <- function(data, colname_to_color = "Strain",base_col,fit,factor = 0.01, strata_blacklist = c("Strain=|Condition.1=|Device=|Trial=|Plate.Name=")){
  # only look at the levels present in the subset
  data <- droplevels(data)
  strata <- names(fit$strata)
  col.pal <- c()
  if(is.null(strata)) return(list(col.pal = unname(strain.col),strata = NULL))
  col_var <- data[,colname_to_color] %>% unlist()
  for (i in 1:length(levels(col_var))) {
    current_level = levels(col_var)[i]
    # grep all strata where this level occurs in the colname_to_color and is not a substring of another level (this is why I added a white space)
    # current_level_pattern <- paste0(colname_to_color,"=",current_level)
    current_level_pattern <- current_level
    occ_length <- lapply(strata, function(x){
      split <- strsplit(x," ")[[1]]
      split <- split[grep(pattern = current_level_pattern,split)]
      str_length(split)
    })
    
    shortes_occurence <-  lapply(occ_length,function(x) any(unlist(x) %in% min(unlist(occ_length)))) %>% unlist()
    occ <- strata[shortes_occurence]
    occ <- occ[!is.na(occ)]

    if (current_level == levels(col_var)[1]) {
      col.mod <- sapply(1:length(occ),function(x) lighten_robust(color = base_col[i],factor = x))
    }
    if (current_level %in% levels(col_var)[-1]){
      col.mod <- sapply(1:length(occ),function(x) lighten_robust(color = base_col[i],factor = x))
    }
    if (length(occ) == 0) {
      col.mod <- NA
    }
    names(col.mod) <- occ
    if (!is.na(col.mod[1])) col.pal <- c(col.pal,col.mod)
    # print(col.pal)
    
    # print(paste("mix_pal_SCDPlates_fit ran successfully"))
  }
  return(col.pal)
} 
 









make_col_palette2 <- function(data, colname_to_color = "Strain",base_col,fit,factor = 0.01, strata_blacklist = c("Strain=|Condition.1=|Device=|Trial=|Plate.Name=")){
  # only look at the levels present in the subset
  data <- droplevels(data)
  strata <- names(fit$strata)
  col.pal <- c()
  if(is.null(strata)) return(list(col.pal = unname(strain.col),strata = NULL))
  
  col_var <- data[,colname_to_color] %>% unlist()
  for (lvl in levels(col_var)) {
    current_level = lvl
    # grep all strata where this level occurs in the colname_to_color and is not a substring of another level (this is why I added a white space)
    # current_level_pattern <- paste0(colname_to_color,"=",current_level)
    current_level_pattern <- current_level
    occ_length <- lapply(strata, function(x){
      split <- strsplit(x," ")[[1]]
      split <- split[grep(pattern = current_level_pattern,split)]
      str_length(split)
    })
    
    shortes_occurence <-  lapply(occ_length,function(x) any(unlist(x) %in% min(unlist(occ_length)))) %>% unlist()
    occ <- strata[shortes_occurence]
    occ <- occ[!is.na(occ)]
    
    if (current_level == levels(col_var)[1]) {
      col.mod <- sapply(1:length(occ),function(x) lighten_robust(color = base_col[current_level],factor = x))
    }
    if (current_level %in% levels(col_var)[-1]){
      col.mod <- sapply(1:length(occ),function(x) lighten_robust(color = base_col[current_level],factor = x))
    }
    if (length(occ) == 0) {
      col.mod <- NA
    }
    names(col.mod) <- occ
    if (!is.na(col.mod[1])) col.pal <- c(col.pal,col.mod)
    # print(col.pal)
    
    # print(paste("mix_pal_SCDPlates_fit ran successfully"))
  }
  return(col.pal)
} 





























# DEPRECIATED, use make_lty_palette
mix_lty_fit <- function(data,strata,Strain = FALSE, Condition.1 = FALSE, Device = FALSE, Trial = FALSE,prune_output = FALSE){
  if (is.null(strata)) return(lty.pal <- 1)
  if (Strain) input <- data$Strain
  if (Condition.1) input <- data$Condition.1
  if (Device) input <- data$Device
  if (Trial) input <- data$Trial
  if (!(Strain | Condition.1 | Device)) return(1) # break if nothing is specified
  cond.lty <- 1:length(levels(input))
  # print(cond.lty)
  names(cond.lty) <- levels(input)
  # print(names(cond.lty))
  # generate lty palette
  lty.pal <- c()
  for (i in cond.lty) {
    cond = names(cond.lty)[i]
    n <- strata[grepl(pattern = cond,strata)]
    lty <- rep(cond.lty[i],length(n))
    names(lty) <- n
    lty.pal <- c(lty.pal,lty)
  }
  if(prune_output) {
    lty.pal <- lty.pal[!duplicated(names(lty.pal))]
  }
  return(lty.pal)
}

make_lty_palette <- function(data,fit,colname_to_set_lty = "Condition.1",prune_output = FALSE){

  strata <- names(fit$strata)
  # only look at the levels present in the subset
  data <- droplevels(data)
  input <- data[,colname_to_set_lty] %>% unlist()
  if (is.character(input)) { 
    input <- as.factor(input)
    print(paste("No levels where assigned to the selected variable:",colname_to_set_lty))
  }
  if (length(levels(input)) <= 1) return(1) # early return, if only one level or less
  
  cond.lty <- 1:length(levels(input))
  # print(cond.lty)
  names(cond.lty) <- levels(input)
  # print(names(cond.lty))
  # generate lty palette
  lty.pal <- c()
  for (i in cond.lty) {
    cond = names(cond.lty)[i]
    n <- strata[grepl(pattern = cond,strata)]
    lty <- rep(cond.lty[i],length(n))
    names(lty) <- n
    lty.pal <- c(lty.pal,lty)
  }
  if(prune_output) {
    lty.pal <- lty.pal[!duplicated(names(lty.pal))]
  }
  return(lty.pal)
}


facilitatenames <- function(vector,exclude_terms = c("Strain=","Condition.1=","Device=","Trial=","Experiment=","Phase=")){
  facilitated <- tm::removeWords(vector,exclude_terms)
  return(facilitated)
}



#############################################################
##################### Survival stats ########################
#############################################################
# survival object and AFT statistic - DEPRECIATED
# ls_stats <- function(data = data){
#   data_s = Surv(data$Death,1-data$Censored)
#   censored <- length(which(data$Censored==1))/length(data$Censored==1)
#   print(paste("Be aware that ",round(censored*100,2),"% of animals were censored (crawled outside of the detection region or got otherwise lost).", sep = ""))
#   return(list(data_s = data_s))
# }

# what lifespans to compare
ls_fit <- function(data,yvar = 'Surv(Death,1-Censored)',xvars = c("Strain", "Condition.1", "Device", "Plate.Name"),facilitate_names = FALSE){
  my.formula <- paste( yvar, '~', paste( xvars, collapse=' + ' ) )
  my.formula <- as.formula(my.formula)
  # sdf <- survdiff(formula = my.formula,data=data)
  # sdf_pair <- pairwise_survdiff(formula = my.formula,data=data) # COMPARE PVALUE FOR EACH COMPARISON!!!!!!:)
  # data[,xvars] <- as.factor(data[,xvars]) # to obtain pvalues for each factor level and not a continuous distribution we convert to factor first.
  fit <- survfit( my.formula, data=data, conf.type = "log-log")
  fit$call$formula <- my.formula # ggsurvplot needs the function call arguments
  # summary(fit)$table # returns mean, standard error, median
  # pvalue calculation strategies
  # fit.coxph <- survival::coxph(formula = fit$call$formula,data = data)
  # survminer::ggforest(fit.coxph, data = data)
  # data$Strain <- as.factor(data$Strain)
  # surv_pvalue(fit =  fit, data = data, method = "survdiff",group.by = "Strain")
  if (facilitate_names) names(fit$strata) <- facilitatenames(vector = names(fit$strata))
  return(fit)
}


# what lifespans to compare
AFT_stats <- function(data,yvar = 'Surv(Death,1-Censored)',levels_Strain,levels_Condition.1,levels_Device){
  # build xvars
  xvars = c()
  if (length(levels_Strain) > 1) xvars <- c(xvars,"Strain")
  if (length(levels_Condition.1) > 1) xvars <- c(xvars,"Condition.1")
  if (length(levels_Device) > 1) xvars <- c(xvars,"Device")
  
  my.formula <- paste( yvar, '~', paste( xvars, collapse=' + ' ) )
  my.formula <- as.formula(my.formula)
  AFT <- bj( my.formula, data=data)
  AFT$call$formula <- my.formula # ggsurvplot needs the function call arguments
  return(AFT)
}
#AFT  <- bj(Surv(Death,1-Censored) ~ Strain + Device, data = data) #apply model




#############################################################
################### Supporting ggplots ######################
#############################################################

plot_bar_obs_deaths <- function(data,levels_Condition.1,strain.col,Condition.1.col,Condition.1.alpha,title = "Number of observed deaths by plate - is the distribution equal and within a sensible range (15 - 65 animals)?"){
  
  obs_deaths <- aggregate(Death ~ Plate.Position.Name + Strain + Condition.1 + Device + Plate.Row + Trial, data = data, FUN = length)
  bar <- ggplot(obs_deaths,aes(x = Plate.Position.Name, y = Death, fill = Strain,color = Condition.1,linetype = Condition.1, alpha = Condition.1)) +
    geom_bar(stat="identity",size = 0.3,width=0.5) +
    scale_fill_manual(values =  strain.col) +
    scale_color_manual(values =  Condition.1.col) +
    scale_alpha_manual(values = Condition.1.alpha) +
    labs(title = title, x = "Plate coordinate on the scanner flatbed (a::0 to c ::3)", y = "# of observed deaths per plate")

  bar <- bar + theme(panel.background = element_blank()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  if (is.null(levels_Condition.1)) bar <- bar + guides(color=FALSE) + guides(linetype=FALSE) + guides(alpha=FALSE) # while Strain and Device are useful even if uspecified the Condition.1 is not.

  
  return(bar)
}



plot_dot_corr_numb_vs_ls <- function(data = data, levels_Condition.1,strain.col,Condition.1.col,Condition.1.alpha,fullrange = FALSE, title = "Correlation between loading number and lifespan"){
  # aggregate dataframe
  number <- aggregate(Death ~ Plate.Name + Strain + Condition.1 + Device + Trial, data = data, FUN = length)
  colnames(number)[colnames(number) == "Death"] <- "individuals"
  lifespan <- aggregate(Death ~ Plate.Name + Trial, data = data, FUN = median)
  numb_ls <- full_join(number,lifespan, by = c("Plate.Name","Trial")) %>% unite(col = "idx",Trial,Device,remove = FALSE,sep = " ")
  

  
  # linear model and correlation
  lm <- lm(formula = individuals ~ Death,numb_ls)
  cor(numb_ls$individuals,numb_ls$Death)
  
  # compute the information matrix
  Measurement_device <- c()
  correlation <- c()
  intercept <- c()
  slope <- c()
  for (id in unique(numb_ls$idx)) {
    s <- numb_ls[numb_ls$idx == id,]
    corr <- cor(s$individuals,s$Death)
    Measurement_device <- c(Measurement_device,id)
    correlation <- c(correlation,corr)
    intercept <- c(slope,lm(formula = individuals ~ Death,s)$coefficients[[1]])
    slope <- c(slope,lm(formula = individuals ~ Death,s)$coefficients[[2]])
    
  }
  fit_tab <- cbind(correlation,intercept,slope)
  rownames(fit_tab) <- Measurement_device
  fit_tab <- round(fit_tab,2)
  
  
  # prepare plot
  g <- ggplot(numb_ls,aes(x = Death, y = individuals)) +
    geom_smooth(method = "lm", color="black",fill="grey3",lty =2, formula = y ~ x, fullrange = fullrange) +
    # geom_rug(aes(color = Strain)) +
    geom_point(aes(color = Condition.1, # color all points
                   fill = Strain,
                   shape = Condition.1,
                   alpha = Condition.1),
               size = 6, stroke = 0.5)
    
  # specify plot parameters  
  g <- g +
    scale_fill_manual(values =  strain.col) +
    scale_shape_manual(values=c(21:25,1:20)) +
    scale_color_manual(values=Condition.1.col) +
    scale_alpha_manual(values = Condition.1.alpha) +
    labs(title = title, x = "Median lifespan per plate [days]", y = "# of observed deaths per plate")

  # conditionally add Condition.1 information
  if (length(unique(data$Condition.1)) > 1){
    # g <- g + geom_point(aes(alpha = Condition.1),data=numb_ls[numb_ls$Condition.1 == levels_Condition.1[1],],shape = 4,size = 1.5, stroke = 1.5) # mark Condition.1 plates
  }else{
    g <- g + guides(color=FALSE) + guides(shape=FALSE) + guides(alpha=FALSE) #if there is no information on Condition.1 then don't show its legend
  }
  
  # plot themes
  g <- g + guides(fill = guide_legend(override.aes = list(shape = 21))) # display the actual fill values that I set in the manual scale
  g <- g + theme_bw()
  return( list(g = g, fit_tab = fit_tab) )
}


plot_movement_with_age <- function(data = data, levels_Condition.1,strain.col,Condition.1.col,Condition.1.alpha,title = "Movement behavior as a function of age"){
  
  g <- ggplot(data,aes(x = Death, y = Duration_Slow)) +
    geom_point(aes(color = Condition.1, # color all points
                   fill = Strain,
                   shape = Condition.1),
               size = 2, stroke = 0.3, alpha=0.5) +
    
    scale_fill_manual(values =  strain.col) +
    scale_shape_manual(values=c(21:25)) +
    scale_alpha_manual(values = Condition.1.alpha) +
    scale_color_manual(values=Condition.1.col) +
    labs(title = title, x = "Lifespan [days]", y = "Duration slow moving [days]")
  
  if (is.null(levels_Condition.1)) g <- g + guides(color=FALSE) + guides(shape=FALSE) + guides(alpha=FALSE)
  
  g <- g + guides(fill = guide_legend(override.aes = list(shape = 21, size = 6, alpha = 1)),
                  shape = guide_legend(override.aes = list(size = 6)))
  g <- g + theme_bw()
  
  return(g)
}

plot_box_plate_position <- function(data = data, strain.col,Condition.1.col,Condition.1.alpha, title = "Number of observed deaths by plate - are there observable trends?"){
  
  box <- ggplot(data,aes(x = Plate.Position.Name, y = Death, fill = Strain,color = Condition.1, alpha = Condition.1)) +
    geom_boxplot(outlier.shape = NA,varwidth = FALSE,position = 'dodge') + 
    geom_dotplot(binaxis="y", stackdir="center", binwidth=0.3, fill = NA,dotsize = 0.2, alpha = 0.8) +
    scale_fill_manual(values =  strain.col) +
    scale_color_manual(values =  Condition.1.col) +
    scale_alpha_manual(values = Condition.1.alpha) +
    labs(title = title, x = "Plate position on scanner flatbed", y = "Single animal survival [days]")
  
  box <- box + theme(panel.background = element_blank()) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  return(box)
}

# Preprocessing for ensuing plate loading visualization
mutate_row_col_fill <- function(data = data){
  a <- data
  cols <- c("a","b","c","d")
  rows <- c(0,1,2,3)
  # add missing column and row levels for all Devices
  for (dev in levels(a$Device)) {
    miss_rows <- rows[!rows %in% a[a$Device == dev,"Plate.Row"]]
    if (length(miss_rows) > 0) {
      replace <- as_data_frame(cbind(rep(dev,length(miss_rows)),miss_rows))
      a[(nrow(a) + 1):(nrow(a) + length(miss_rows)),c("Device","Plate.Row")] <- replace
    }
    miss_cols <- cols[!cols %in% a[a$Device == dev,"Plate.Column"]]
    if (length(miss_cols) > 0) {
      replace <- as_data_frame(cbind(rep(dev,length(miss_cols)),miss_cols))
      a[(nrow(a) + 1):(nrow(a) + length(miss_cols)),c("Device","Plate.Column")] <- replace
    }
  }
  # remove NAs
  a <- a[(!is.na(a$Plate.Column) & !is.na(a$Plate.Row)),]
  return(a)
}

plot_scanner_loading <- function(row_col_filled_data,levels_Condition.1, strain.col,Condition.1.col,Condition.1.alpha,title = c("Plate order in scanners, as viewed from above")){
  
  # if("by_hand" %in% row_col_filled_data$Device) return(NULL)
  a <- row_col_filled_data
  a$Plate.Row <- factor(a$Plate.Row, levels = c(3,2,1,0))
  a$Plate.Column <- factor(a$Plate.Column, levels = c("d","c","b","a"))
  
  g <- ggplot(data = a,aes(x = Plate.Column, y = Plate.Row)) +
    
    geom_point(aes(color = Condition.1, # color all points
                   fill = Strain,
                   shape = Condition.1,
                   alpha = Condition.1),
               size = 6, stroke = 0.5)
  
  g <- g +
    scale_fill_manual(values =  strain.col) +
    scale_shape_manual(values=c(21:25)) +
    scale_color_manual(values=Condition.1.col) +
    scale_alpha_manual(values = Condition.1.alpha) +
    labs(title = title, x = "Columns on scanner flatbed [left to right]", y = "Rows on scanner flatbed [front to back]")
  
  # if (!is.null(levels_Condition.1)){
  #   g <- g + geom_point(aes(alpha = Condition.1),data=a[a$Condition.1 != levels_Condition.1[1],],show_guide = FALSE,shape = 4,size = 9, stroke = 1.5) 
  # }else{
  #   g <- g + guides(color=FALSE) + guides(shape=FALSE) + guides(alpha=FALSE) #if there is no information on Condition.1 then don't show its legend
  # }
  
  # plot themes
  g <- g + guides(fill = guide_legend(override.aes = list(shape = 21))) # display the actual fill values that I set in the manual scale
  g <- g + theme_bw()
  g <- g + theme(legend.position="bottom")
  
  g <- g + coord_fixed() # we want square plots to represent the scanner flatbed
  return(g)
}


# inspect all plates (not necessarily containing all biol. information, only the machine-assigned plate names)
initial_plate_rank_plot <- function(data){
  fit <- ls_fit(data = data,yvar = 'Surv(Death,1-Censored)',xvars = c("Plate.Name"))
  ggsurv <- ggsurvplot(fit = fit,data = data ,conf.int = F,legend = "top",title ="Lifespan",pval = TRUE,surv.median.line = "hv",censor =FALSE,risk.table = TRUE,tables.height = 0.2, xlab = "Time [days]",linetype = rep_len(x = c(1,2,3),length.out = length(fit$strata)))
  
  plate_order <- data %>% group_by(Plate.Name) %>% summarise(median(Death,na.rm = TRUE))
  plate_order = plate_order[order(unlist(plate_order[,2])),]
  return(list(plot = ggsurv$plot, plate_order = plate_order))
}
initial_plate_rank <- function(data){
  plates <- data %>% group_by(Plate.Name) %>% summarise(median(Death,na.rm = TRUE))
  plates = plates[order(unlist(plates[,2])),]
  return(plates)
}


### Shiny functions (input, values & output dependent)

exclude_autocomplete_manual <- function(data,
                                        expected_colnames = c("strain","plate","frequency","time","censored","condition","comments","device","Plate.Row","Plate.Column","Plate.Position.Name","temperature","trial"),
                                        LS_convention_colnames = c("Strain","Plate.Name","Frequency","Death","Censored","Condition.1","Comments","Device","Plate.Row","Plate.Column","Plate.Position.Name","Temp.","Trial")
                                        ){
  
  # return specific error messages for the following cases
  if (is.null(dim(data))) alert(data)

  # only keep expected columns
  colnames(data) <- tolower(colnames(data))
  data <- data[,colnames(data) %in% expected_colnames]
  
  # add missing columns
  missing <- expected_colnames[!expected_colnames %in% colnames(data)]
  data[,missing] <- NA
  data <- data[,expected_colnames]
  
  # rename columns to lifspan conventions
  colnames(data) <- LS_convention_colnames
  
  ### autocomplete if necessary columns stay empty:
  if(all(is.na(data$Strain))) data$Strain <- "C.elegans"
  if(all(is.na(data$Trial))) data$Trial <- "Trial"
  if(all(is.na(data$Condition.1))) data$Condition.1 <- "Condition.1"
  if(all(is.na(data$Device))) data$Device <- "by_hand"
  if(all(is.na(data$Plate.Name))) data$Plate.Name <- "Plates"
  if(all(is.na(data$Frequency))) data$Frequency <- 1
  if(all(is.na(data$Censored))) data$Censored <- 0
  if(all(is.na(data$Temp.))) data$Temp. <- 20
  
  data$Comments <- "comment_ommited"
  data$Plate.Row <- 1
  data$Plate.Column <- "a"
  data$Plate.Position.Name <- data$Plate.Name

  # remove rows with missing information in Strain, Plate.Name, Frequency, Death, Censored, Condition.1
  keep_rows <- apply(data, 1, function(row){
    all(!is.na(row))
  })
  data <- data[keep_rows,]
  
  # repeat every row as many times as stated in the frequency column
  data <- data[rep(row.names(data), data$Frequency), ]

  
  print("COLNAMES")
  print(colnames(data))
  
  # data <- Min_Max_plate_thr(data = data)
  
  return(data)
}

exclude_autocomplete_scoresheet <- function(data,
                                            n_scoring_conditions = 5,
                                            plate_col_start = 8,
                                            cens_cond = c("C","E","B")
){
  # using the provided excel file for scoring lifespans
  ###  data <- read_xlsx("./data/Muster2016.xlsx") # for debugging
  # meta information
  Experiment <- colnames(data)[grep(pattern = "Experiment",x = colnames(data))]
  Experiment <- strsplit(x = Experiment,split = "Experiment.")[[1]][2]
  Experiment <- str_trim(string = Experiment, side = "both")
  # Temp. <- 20
  
  # 
  colnames(data) <- data[1,]
  data <- data[-1,]
  # n_plates <- grep(pattern = "D",x = colnames(data)[plate_col_start:ncol(data)])
  subcols <- colnames(data)[plate_col_start:ncol(data)]
  n_plates <- length(subcols)/n_scoring_conditions
  
  # gather by plate
  plate <- rep(1:n_plates,each = n_scoring_conditions)
  plate <- paste0(plate,subcols)
  colnames(data)[plate_col_start:ncol(data)] <- plate
  
  # deaths
  d_col <- unique( c(1:plate_col_start-1 , grep(pattern = "D",x = colnames(data))) )
  df_d <- data[,d_col]
  df_d <- gather(data = df_d, colnames(df_d)[plate_col_start:ncol(df_d)] , key = "Plate",value = "Deaths")
  df_d$Plate <- sapply(df_d$Plate, function(x) gsub("[^0-9.]", "", x)  )
  df_d$ID <- paste(df_d$Age,df_d$Strain,df_d$Cond,df_d$Trial,df_d$Plate,sep = "-")
  df_d <- df_d %>% filter(Deaths > 0)
  df_d <- df_d[rep(row.names(df_d), df_d$Deaths), ] %>% mutate(Deaths = 1)
  
  
  # censored
  c_cond <- sapply(cens_cond, function(pattern) grep(pattern = pattern,x = colnames(data)))
  c_cond <- unname(unlist(c_cond))
  c_col <- unique( c(1:plate_col_start-1 , c_cond) )
  df_c <- data[,c_col]
  df_c <- gather(data = df_c, colnames(df_c)[plate_col_start:ncol(df_c)] , key = "Plate",value = "Censored")
  df_c$Plate <- sapply(df_c$Plate, function(x) gsub("[^0-9.]", "", x)  )
  df_c$ID <- paste(df_c$Age,df_c$Strain,df_c$Cond,df_c$Trial,df_c$Plate,sep = "-")
  df_c$Censored <- as.numeric(df_c$Censored)
  
  # summarize identical plates
  df_c <- plyr::ddply(df_c,~Age + Day + Strain + Cond + Trial + Time + Plate + ID ,summarise,Censored=sum(Censored,na.rm = TRUE))
  df_c <- tibble::as_tibble(df_c)
  df_c <- df_c %>% filter(Censored != 0)
  df_c <- df_c[rep(row.names(df_c), df_c$Censored), ] %>% mutate(Censored = 1)
  
  # join death and censored
  tbl <- bind_rows(df_d,df_c)
  tbl$Censored <- replace_na(tbl$Censored,0)
  tbl$Deaths <- 1
  tbl <- tibble::add_column(.data = tbl,.before = "Age",Experiment)
  
  # make compatible with downstream analysis
  tbl$Frequency <- 1
  tbl$Excluded <- 0
  tbl$Comments <- "comment_ommited"
  tbl$Plate.Row <- 1
  tbl$Plate.Column <- "a"
  tbl$Device <- "by_hand"
  tbl$Plate.Position.Name <- tbl$Plate
  data.table::setnames(x = tbl,old = c("Age","Cond","Plate"),new = c("Death","Condition.1","Plate.Name"))
  tbl[,c("Death","Censored","Frequency","Excluded")] <- tbl[,c("Death","Censored","Frequency","Excluded")] %>% mutate_if(is.character, as.numeric)
  
  # repeat every row as many times as stated in the frequency column
  # tbl$Temp. <- Temp.
  tbl <- tbl %>% mutate(Temp. = Temp. %>% as.numeric())
  
  
  
  return(tbl)
}



Min_Max_plate_thr <- function(data,min_max_individuals_per_plate = c(0,1000)){
  # Max & Min thresholds
  obs_deaths <- aggregate(Death ~ Plate.Name, data = data, FUN = length)
  obs_deaths_max_thr <- obs_deaths[obs_deaths$Death <= min_max_individuals_per_plate[2],]
  obs_deaths_min_thr <- obs_deaths[obs_deaths$Death >= min_max_individuals_per_plate[1],]
  to_keep <- intersect(obs_deaths_max_thr,obs_deaths_min_thr)
  keep_plates <- unlist(to_keep$Plate.Name)
  data <- data[data$Plate.Name %in% keep_plates,]
  return(data)
}


############### print survfit object to readable table
# fit_table_output <- function(fit){
#   strata <- fit$strata
#   n <- fit$n
#   mat <- cbind(strata,n)
#   return(mat)
# }



# calculate stats output to be displayed below the lifespan plots
lifespan_summary_stats <- function(data, fit){
  # individual stats output
  percentile25 <- quantile(fit,probs = 0.25)$quantile   # 75% percentile
  percentile75 <- quantile(fit,probs = 0.75)$quantile   # 75% percentile
  fit_summary <- summary(fit)
  if(nrow(fit_summary$table) %>% is.null()){
    stat_table <- fit_summary$table[c('n.start','events','*rmean','median','*se(rmean)')] %>% as.matrix() %>% t()
  }else{
    stat_table <- fit_summary$table[,c('n.start','events','*rmean','median','*se(rmean)')]  #  events = number of events (which is records - right censored) for each group.
  }
  stat_table <- cbind(stat_table, percentile25,percentile75)
  colnames(stat_table)[c(6,7)] <- c("25","75")
  mean_unrounded <- stat_table[,'*rmean']
  stat_table <- cbind(stat_table,mean_unrounded)
  no_round <- which(colnames(stat_table) == "mean_unrounded")
  stat_table[,-no_round] <- round(stat_table[,-no_round],digits = 1)
  mean_se <- paste(stat_table[,'*rmean'],stat_table[,'*se(rmean)'],sep = " ± ")
  
  stat_table <- cbind(stat_table, mean_se)
  stat_table <- as.data.frame(stat_table)
  data.table::setnames(x = stat_table,old = c("n.start","events","*rmean",      "median", "25",             "75",             "mean_se"),
                                      new = c("start","end" , "mean_values","median", "25th per.","75th per.","mean ± se"))
  stat_table[,colnames(stat_table) != "mean ± se"] <- stat_table[,colnames(stat_table) != "mean ± se"] %>% mutate_all(as.character) %>% mutate_all(as.numeric)

  # ADDITIONS
  # calculate mean survival change
  ctr <- stat_table[1,"mean_unrounded"]
  Mean_LS_change <- c()
  for (row in 2:nrow(stat_table)) {
    change_to_ctr <- round(((stat_table[row,"mean_unrounded"] / ctr) - 1) * 100,0)
    Mean_LS_change <- c(Mean_LS_change, change_to_ctr)
  }
  Mean_LS_change <- c(NA,Mean_LS_change)
  stat_table <- cbind(stat_table,Mean_LS_change)

  # pvalues
  pval_mat_raw <- pairwise_survdiff(formula = fit$call$formula,data=data, rho = 0, p.adjust.method = "BH") # rho = 0 corresponds to log-rank test
  pval_mat_raw <- pval_mat_raw[["p.value"]]
  # export pval_matrix
  blank_row <- rep(NA,ncol(pval_mat_raw))
  pval_mat_exp <- rbind(blank_row,pval_mat_raw)
  rownames(pval_mat_exp)[1] <- colnames(pval_mat_exp)[1]
  
  # process the first column of the pval matrix for the table
  pval_mat <- pval_mat_raw[,1]
  Pval <- NA
  for (val in pval_mat) {
    p <- format(val, digits = 1, scientific=TRUE)
    Pval <- c(Pval,p)
  }
  
  stat_table <- cbind(stat_table,Pval)
  n_assayed <- paste(stat_table$end,stat_table$start,sep = " / ")
  stat_table <- cbind(stat_table,n_assayed)
  
  stat_table <- as.data.frame(stat_table)
  data.table::setnames(x = stat_table,old = c("Mean_LS_change","Pval","n_assayed"),
                       new = c('mean lifespan change [%]', 'p-value (log-rank)','n assayed / n total'))
 
 # arrange column order
  stat_table <- stat_table[,c(12,1:11)]
    
  return(list(stat_table = stat_table,pval_mat_exp = pval_mat_exp))
}

pval_matrix <- function(data,Strain = FALSE,Condition.1 = FALSE){
  # comparative stats output (add an empty row with the first factor level to the top to make the output comparable to the statistics output)
  if (Strain & Condition.1){
    pvals <- pairwise_survdiff(Surv(Death,1-Censored) ~ ID,data = data,p.adjust.method = 'BH')$p.value
    pvals <- rbind(rep(NA,ncol(pvals)),pvals)
    rownames(pvals)[1] <- levels(data$ID)[1]
  }
  if (Strain & !Condition.1){
    pvals <- pairwise_survdiff(Surv(Death,1-Censored) ~ Strain,data = data,p.adjust.method = 'BH')$p.value
    pvals <- rbind(rep(NA,ncol(pvals)),pvals)
    rownames(pvals)[1] <- levels(data$Strain)[1]
  }
  if (!Strain & Condition.1){
    pvals <- pairwise_survdiff(Surv(Death,1-Censored) ~ Condition.1,data = data,p.adjust.method = 'BH')$p.value
    pvals <- rbind(rep(NA,ncol(pvals)),pvals)
    rownames(pvals)[1] <- levels(data$Condition.1)[1]
  }
  print("data$ID")
  print(unique(data$ID))
  # idx of very small pvals
  idx_10_10 <- pvals < 10e-10
  idx_10_5 <- pvals < 10e-5
  idx_005 <- pvals < 0.05
  
  # round & replace selected numeric elements by strings
  pvals <-round(pvals,digits = 3)
  pvals[idx_005] <- "< 0.05"
  pvals[idx_10_5] <- "< 10e-5"
  pvals[idx_10_10] <- "< 10e-10"
  return(pvals)
}






############### make kable tables
kable_table_assemble <- function(data, #values$data
                              xvars = c("Strain","Condition.1"),
                              force_experiment_column = NULL, #not yet working for all columns
                              facilitate_names = FALSE,
                              round_digits = 2,
                              force_merge_trials = FALSE,
                              remove_combined_stats = TRUE){
  
  if ("Experiment" %in% colnames(data)) experiment_column <- "Experiment"             # use Experiment to group
  if ("Trial" %in% colnames(data)) experiment_column <- "Trial"                       # if Trial is available this superseeds Experiment
  if(!is.null(force_experiment_column)) experiment_column <- force_experiment_column  # If a grouping value is forced this wins over both the above.
  # All trials plus combined trial
  experiments <- unique(data[,experiment_column]) %>% unlist()
  experiments <- split(experiments,names(experiments))
  experiments <- lapply(experiments,unname)
  names(experiments) <-  experiments %>% unlist() %>% unname()
  experiments$Combined <- unname(unlist(experiments))
  if(force_merge_trials) experiments <- experiments[length(experiments)]
  # experiments %>% add_row(Trial = list("A","B"))
  # Combined_Trials <- c("A","B")
  # names(Combined_Trials) <- Combined_Trials
  # compute stats for all experiments
  stat_tables <- list()
  pval_mats <- list()
  for (i in 1:length(experiments)) {
    exp <- experiments[[i]]
    name <- names(experiments[i])
    sub <- data[unlist(data[,experiment_column]) %in% exp,]
    fit <-  ls_fit(data =  sub,yvar = 'Surv(Death,1-Censored)',xvars = xvars)
    out <- lifespan_summary_stats(data = sub,fit = fit)
    stat_table <- out$stat_table
    Strata <- rownames(stat_table)
    stat_tables[[name]] <- cbind(Strata,stat_table)
    pval_mat_exp <- out$pval_mat_exp
    pval_mats[[name]] <- pval_mat_exp
  }
  # bind the individual experiment stats to one dataframe (and remove the experiment column)
  original_names <- colnames(stat_tables[[1]])
  
  if(remove_combined_stats){
    stat_tables <- stat_tables[names(stat_tables) != "Combined"]
    pval_mats <- pval_mats[names(pval_mats) != "Combined"]
  } 

  df <- plyr::ldply(stat_tables, data.frame)
  colnames(df) <- c(".id",original_names)
  
  # add additional values to table
  Temp. <- paste0(data$Temp.[1],"°C")
  df <- tibble::add_column(.data = df,.before = "start",Temp.)
  
  # remove unneeded columns
  blacklist <- c("start","end","mean_values", "*se(rmean)", "mean_unrounded")
  df <- df[,!colnames(df) %in% blacklist]
  
  
  data.table::setnames(x = df,old = c(".id","Strata"),new = c("Experiment","Strata"))
  df <- df[,2:ncol(df)]
  
  # How many rows are in each experiment
  num_rows <- purrr::map(1:length(names(stat_tables)), function(x){nrow(stat_tables[[x]]) }) %>% unlist()
  
  # # add explanatory links to table for the legend (symbol for columns and alphabet for rows)
  # names(df)[c(2)] <- paste0(names(df)[2], footnote_marker_symbol(1))
  # names(df)[c(5)] <- paste0(names(df)[5], footnote_marker_symbol(2))
  if(facilitate_names){
    df$Strata <- facilitatenames(vector = as.character(df$Strata))
    pval_mats <- lapply(pval_mats,function(x){
      colnames(x) <- facilitatenames(vector = as.character(colnames(x)))
      rownames(x) <- facilitatenames(vector = as.character(rownames(x)))
      return(x)
    }) 
    stat_tables <- lapply(stat_tables,function(x){
      colnames(x) <- facilitatenames(vector = as.character(colnames(x)))
      rownames(x) <- facilitatenames(vector = as.character(rownames(x)))
      return(x)
    }) 
  }
  return(list(df = df, num_rows = num_rows, stat_tables = stat_tables, pval_mats = pval_mats))
}
  
kable_table_plot_html <- function(df,num_rows,stat_tables,footnote_general = NULL){
  grouping <- setNames(num_rows, names(stat_tables))
  # make table
  df %>%
    kable("html", align = "c",escape = F) %>%
    kable_styling("hover",position = "center") %>%
    add_header_above(c(" "," "," ", "Population statistics" = 4, "Relative to control" = 2)) %>%
    column_spec(1, width = "5cm") %>%
    column_spec(2, width = "2cm") %>%
    column_spec(3:6, width = "2em") %>%
    column_spec(7, width = "2cm") %>%
    column_spec(8:9, width = "1cm") %>%
    kableExtra::pack_rows(index = grouping, latex_gap_space = "2em") %>%
    footnote(symbol = c("se: refers to the standard error of the mean"),
             general = footnote_general)
}

kable_table_plot_latex <- function(df,num_rows,stat_tables,footnote_general = NULL, caption = "Survival table"){
  # install.packages('tinytex') # tested successfully with tinytex_0.16
  # tinytex::install_tinytex()
  grouping <- setNames(num_rows, names(stat_tables))
  # make table
  df %>%
    kable(format = "latex",longtable = T,linesep = "", booktabs = T,caption = caption) %>% # add linesep = "\\addlinespace" to increase line spacing
    kable_styling(latex_options = c("repeat_header"),full_width = T, position = "center",font_size = 7) %>%
    add_header_above(c(" "," "," ", "Population statistics" = 4, "Relative to control" = 2)) %>%
    column_spec(1, width = "5cm") %>%
    column_spec(2, width = "2cm") %>%
    column_spec(3:6, width = "2em") %>%
    column_spec(7, width = "2cm") %>%
    column_spec(8:9, width = "1cm") %>%
    kableExtra::pack_rows(index = grouping, latex_gap_space = "2em") %>%
    footnote(symbol = c("se: refers to the standard error of the mean"),
           general = footnote_general)
}

kable_pvalmat_latex <- function(df,footnote_general = NULL, caption = "P-value matrix"){
  df <- df %>%
      kable("latex",longtable = T, booktabs = T,linesep = "",caption = caption) %>%
      kable_styling(latex_options = c("striped","repeat_header"),full_width = T, position = "center",font_size = 4) %>%
      column_spec(1, width = "2cm")
  return(df)

}

table_display_reactive <- function(single_table,lengthMenu = c(5,10,25,50,100,500), pageLength = 5, dom = 'tl',
                                   plot_cols = c("start","end","median","mean ± se mean","mean lifespan change [%]","p-value (log-rank)")){
  if(plot_cols[1] == "all"){
    data <- single_table
  }else{
    data <- single_table[,colnames(single_table) %in% plot_cols]
  }
  DT::datatable(data = data,options = list(lengthMenu = lengthMenu, pageLength = pageLength, dom = dom))
}


simplify_pvals <- function(df,modification = "cutoff"){
  out <- df
  if (modification == "round"){
    out <- round(x = df,digits = 3)
  } 
  if (modification == "scientific"){
    out <- format(x = df,scientific = TRUE,digits = 3)
  } 
  if (modification == "stars"){
    out <- apply(df, 2, function(x){
      case_when(is.na(x) ~ "",
                x >= 0.05 ~ "n.s.",
                x >= 0.01 ~ "*",
                x >= 0.001 ~ "**",
                x >= 0.0001 ~ "***",
                TRUE ~ "****")
    })
    rownames(out) <- rownames(df)
  }
  if (modification == "cutoff") {
    out <- round(x = df,digits = 3)
    out <- ifelse(out < 0.0001,"<0.0001",out)
    out <- out %>% replace_na("")
    out <- ifelse(out >= 0.05,paste0("n.s.(",out,")"),out)
  }
  return(out)
}

minimal_reproducable_export <- function(data,
                                        columns = c("Trial","Strain","Condition.1","Frequency","Death","Censored","Excluded","Temp.","Device","Food.Source"),
                                        set_freq_to_one = TRUE){
  idx <- lapply(columns, function(x) which(colnames(data) == x))
  idx <- idx %>% unlist() %>% unname()
  export <- data[,idx]
  export <- export %>% rename(Condition = Condition.1, Time = Death)
  if(set_freq_to_one) export$Frequency <- 1
  return(export)
}

export_for_jmp_freq1 <- function(data,
                                 columns = c("Trial","Strain","Condition.1","Plate.Name","Frequency","Death","Censored","Excluded","Device"),
                                 add_merged_cols = TRUE){
  idx <- sapply(columns, function(x) which(colnames(data) == x))
  idx <- idx %>% unlist() %>% unname()
  export <- data[,idx]
  export <- export %>% 
    filter(!is.na(Death)) %>%
    mutate_if(is.factor,as.character) %>%
    mutate(Frequency = 1) %>%
    mutate(Strain_Condition = paste(Strain,Condition.1,sep = "_"),
           Strain_Condition_Device = paste(Strain,Condition.1,Device,sep = "_"),
           Strain_Condition_Plate = paste(Strain,Condition.1,Plate.Name,sep = "_"))
  # test output
  test_excluded <- all(export$Excluded == 0)
  test_freq <- all(export$Frequency == 1)
  test_death <- all(!is.na(export$Death))
  # reformat for reproducibility 
  export <- export %>% rename(Condition = Condition.1, Time = Death, Plate = Plate.Name)
  
  if(test_excluded & test_freq & test_death) {
    return(export)
  }else(stop("Data could not be exported to jmp"))
}





########## animation ############
# # reproducible example:
# library(gganimate)
# library(survival)
# library(survminer)
# library(tidyverse)
# fit <- survfit(Surv(time, status) ~ sex, data = lung)
# g <- ggsurvplot(fit, data = lung)
# g$plot + transition_reveal(time)
# anim <- g$plot + transition_reveal(time)
# a <- animate(anim, renderer = ffmpeg_renderer(),rewind = TRUE,start_pause = 10,end_pause = 10,nframes = 150, fps = 5,height = 800, width = 1300)
# anim_save("animation_elegans.mp4", a)




# # Working
# values <- readRDS("values.RDS")
# data <- values$data %>% select(Strain,Censored,Death)
# write.csv(x = data,file = "animation_input.csv")
# data <- read_csv("animation_input.csv")
# data$time <- abs(rnorm(nrow(data),mean = 15))
# # data <- tibble(Strain = rep(c("N2","daf2"),100), time = 1:200, censor = rep(c(1,0,0,0),50))
# fit <- survfit(Surv(time, 1-Censored) ~ Strain, data = data)
# g <- ggsurvplot(fit, data = data)
# anim <- g$plot + transition_reveal(time)
# anim_save("animation_Rilm.mp4", anim)


