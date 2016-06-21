# get condition-wise samples from calculated mean posteriors 
get_condition_mean_sample = function(intercept, effect, add, space){
  if (add) {
    condition_mean_trans = intercept + effect/2
  } else {
    condition_mean_trans = intercept - effect/2
  }
  if (space == "log") {
    condition_mean = exp(condition_mean_trans)*250
  } else if (space == "null") {
    condition_mean = (condition_mean_trans)*250
  } else if (space == "logit") {
    condition_mean = plogis(condition_mean_trans)
  } else if (space == "log_free") {
    condition_mean = exp(condition_mean_trans)
  } else {
    print("choose 'space' argument from 'log', 'null', 'log_free'")
  }
  condition_mean_reps = sample(condition_mean, 50, replace = T)
  
  return(condition_mean_reps)
}



# do TOJ Posterior Predictive Check 
do_toj_ppc = function(pss, jnd, main, factors, levels, y_lab = "left proportion", real = real_toj) {
  df = NULL
  for (i in 1:length(pss)) {
    df_temp = data.frame(SOA = SOAs, left_prop = pnorm( SOAs, mean = pss[i], sd = jnd[i]) )
    df = rbind(df, df_temp)
  }
  
  gg = ggplot(data = df, aes(x = SOA, y = left_prop))+
    geom_point(alpha = 0.3, colour = "turquoise")+
    ylab(y_lab)+
    xlab("SOA")+
    ggtitle(main)
  
  i = length(factors)
  while (i > 0) { 
    real = real[real[,factors[i]] == levels[i], ]
    i = i - 1
  }
  
  if (y_lab == "left proportion") {
    real_use = data.frame(SOA = SOAs, value = real$left_first_TF) 
  } else if (y_lab == "safe proportion") {
    real_use = data.frame(SOA = SOAs, value = real$safe)
  } else {
    ("ERROR: y_lab must be 'left proportion' or 'safe proportion'")
  }
  
  gg = gg + geom_point(data = real_use, aes(x = SOA, y = value), colour = "blue")
  
  return(gg)
}



# do Color Posterior Predictive Check
do_color_ppc = function(rho, kappa, main, factors, levels) {
  df = NULL
  for (i in 1:length(rho)) {
    color_dev = c( 
      rvm(1000*(rho[i]), pi, kappa[i]) - pi
      , runif(1000*(1- rho[i]), -pi, pi) 
    )
    df_temp = data.frame(
      repi = rep(i, length(color_dev))
      , color_dev = color_dev
    )
    df = rbind(df, df_temp)
  }
  
  gg = ggplot(data = df, aes(color_dev, group = factor(repi)))+
    geom_density(colour = "turquoise")+
    xlab("radian deviation")+
    ggtitle(main)+
    theme(legend.position = "none")
  
  real = color_trials
  i = length(factors)
  while (i > 0) { 
    real = real[real[,factors[i]] == levels[i], ]
    i = i - 1
  }
  
  real = data.frame(
    repi = rep(i, length(real$color_diff_radians))
    , color_dev = real$color_diff_radians
  )
  
  gg = gg + geom_density(data = real, aes(color_dev, group = factor(repi)), colour = "blue")
  
  return(gg)
}



# Highest Density Interval Functions
get_95_HDI = function(y) {
  HDI = HPDinterval( as.mcmc( as.vector(y) ), prob = .95 )
  Den = density( as.vector(y) )
  min = HDI[1]
  # mod = Den$x[which(Den$y == max(Den$y))] # mode as indicator of central tendency
  med = median(y)
  max = HDI[2]
  return( c( ymin = min, y = med, ymax = max) )
}

get_50_HDI = function(y) {
  HDI = HPDinterval( as.mcmc( as.vector(y) ), prob = .50 )
  Den = density( as.vector(y) )
  min = HDI[1]
  # mod = Den$x[which(Den$y == max(Den$y))]  # mode as indicator of central tendency
  med = median(y)
  max = HDI[2]
  return( c( ymin = min, y = med, ymax = max) )
}



# get samples for given parameter from extract(stan) object 
extract_samples = function(parameter, SD = FALSE) {
  if (SD) {
    df2 = data.frame(value = tan(ex_toj_color_post[[parameter]]))
  } else {
    df2 = data.frame(value = ex_toj_color_post[[parameter]])
  }
  df2$iteration = rownames(df2)
  df = melt( df2 )$value
  return(df)
}



# get scatterplot and associated correlation
get_corr = function( cor_val, cor_lab) {
  
  gg2 = ggplot(
    data = pos_corr[pos_corr$parameter == cor_val,]
    , aes(x = parameter, y = value)
  )+
    geom_violin()+
    labs(x = cor_lab, y = "Correlation Coefficient (r)")+
    stat_summary(fun.data = get_95_HDI, size = 0.7)+
    stat_summary(fun.data = get_50_HDI, size = 2.5)+  
    geom_hline(yintercept = 0, linetype = 2, size = 1)+
    theme_gray(base_size = 30)+
    theme(panel.grid.major = element_line(size = 1.5)
          ,panel.grid.minor = element_line(size = 1)
          , axis.text.x = element_blank()
          , axis.ticks.x = element_blank()) 
  
  print(gg2)
  
  HDI95 = get_95_HDI( pos_corr[pos_corr$parameter == cor_val,]$value ) 
  
  return(HDI95)
}



# get violin plots
get_violin = function(value1, label1, value2 = NULL, label2 = NULL, y_lab, hline = TRUE, facet = FALSE, samps = 80000) {
  df = data.frame(  
    value = c(
      value1
      , value2
    )
    , parameter = c(
      rep(label1, samps)  
      , rep(label2, samps)  
    )
  )
  
  gg = ggplot(data = df)+
    geom_violin(aes(x = parameter, y = value))+
    labs(x = "", y = y_lab)+
    stat_summary(aes(x = parameter, y = value), fun.data = get_95_HDI, size = 0.7)+  
    stat_summary(aes(x = parameter, y = value), fun.data = get_50_HDI, size = 2.5)
  
  if (hline) {
    gg = gg + geom_hline(yintercept = 0, linetype = 2, size = 1)
  }
  
  if (facet) {
    gg = gg + facet_wrap(~parameter, scales = "free")
  } 
  
  gg = gg + theme_gray(base_size = 30)+
    theme(
      panel.grid.major = element_line(size = 1.5)
      , panel.grid.minor = element_line(size = 1)
      , strip.background = element_blank()
      , strip.text.x = element_blank() 
      , axis.ticks.x = element_blank() 
    ) 
  
  print(gg)
  
  print("value 1"); print(get_95_HDI(value1) )
  if ( is.null(value2) ) {
    to_print = NA
  } else {
    to_print = get_95_HDI(value2)
  } 
  print("value 2"); print( to_print )
}


