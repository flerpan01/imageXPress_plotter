## Reference: Ritz, C.; Baty, F.; Streibig, J. C.; Gerhard, D. Dose-Response Analysis Using R. PLOS ONE 2015, 10 (12), e0146021. https://doi.org/10.1371/journal.pone.0146021.

Results <- read_excel("24h_TCS.xlsx")
#View(X2h_2_4_DNP_lowVol)

data = Results
cmin <- min(data$conc)+0.01 # need to shift conc == 0 a bit up, otherwise there are problems with coord_trans
cmax <- max(data$conc) 
lowerlimit <- mean(data$R_G[data$conc == cmax])
coeff <- mean(data$R_G[data$conc == 0])

head(data) 

# calculate the log-logistic model for acute toxicity data

daphnia_acute.m <- drm(alive/total ~ conc, weights = total, data = data, fct = LL.2(),  type = "binomial")
summary(glht(daphnia_acute.m ))


#calculating EC5/EC10 and EC50
ED(daphnia_acute.m , c(5, 10, 50), interval = "delta")

# For the plot: Calculation of confidence intervals and ribbon

# new dose levels as support for the line
newdata_acute <- expand.grid(conc=exp(seq(log(cmin), log(cmax), length=100)))  #log
#newdata_acute <- expand.grid(conc=seq(0.5, cmax, length=10000))
# predictions and confidence intervals
pm_acute <- predict(daphnia_acute.m , newdata=newdata_acute, interval="confidence")

# new data with predictions
newdata_acute$p <- pm_acute[,1]
newdata_acute$pmin <- pm_acute[,2]
newdata_acute$pmax <- pm_acute[,3]

## Calculation of log logistic model for the JC-1 values (normalized to control) defining the lower limit as the average of the dead control

lowerlimit_norm <- mean(data$R_G_norm[data$conc == cmax])
coeff_norm <- 1

# JC-1 analysis 
daphnia_JC1_norm.m <- drm(R_G_norm ~ conc, data = data, fct = LL.4(fixed = c(NA, lowerlimit_norm, NA,NA)),  type = "continuous") # fixed bottom value 2.3 
  summary(glht(daphnia_JC1_norm.m ))
  
  #calculating EC5/EC10 and EC50
  ED(daphnia_JC1_norm.m , c(5, 10, 50), interval = "delta")
  
  # new dose levels as support for the line
  newdata_JC1_norm <- expand.grid(conc=exp(seq(log(cmin), log(cmax), length=1000)))   #log
  # newdata_JC1 <- expand.grid(conc=seq(0.5, cmax, length=10000))
  # predictions and confidence intervals
  pm_JC1_norm <- predict(daphnia_JC1_norm.m , newdata=newdata_JC1_norm, interval="confidence")
  
  # new data with predictions
  newdata_JC1_norm$p <- pm_JC1_norm[,1]
  newdata_JC1_norm$pmin <- pm_JC1_norm[,2]
  newdata_JC1_norm$pmax <- pm_JC1_norm[,3]
  
  ## Adjusting the data from JC-1 signal so both graphs plotted in the same range: code was taken from:  
  ##https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales   
  
  
ylim.prim <- c(0, 1)   # live/Dead
ylim.sec <- c(lowerlimit_norm, 1)    # JC-1 signal

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]
  
# need to shift conc == 0 a bit up, otherwise there are problems with coord_trans
 
 data$conc0 <- data$conc 
  #data$conc0[data$conc0 == 0] <- 0.3 # moving only concentration 0 to conc 0.5 (log)
  
     # plotting the curve
    
  #coeff <- 17  # range difference between left and right y-axis

    theme_set(theme_bw()) # changes the theme
    ggplot(data, aes(x = conc0, y = alive/total)) +
      geom_point(size =3, color = "darkcyan") + #data points
      geom_ribbon(data=newdata_acute, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2, fill= "darkcyan")+ # confidence intervall 
      geom_line(data=newdata_acute, aes(x=conc, y=p), color = "darkcyan", linewidth = 0.8, linetype = "solid") + # line of model
      ## JC-1
      geom_point( aes(y=a+R_G_norm*b), color = "firebrick",size =3, shape = "diamond" ) +
      geom_ribbon(data=newdata_JC1_norm, aes(x=conc, y=a+p*b, ymin=a+pmin*b, ymax= a+pmax*b), alpha=0.2, fill ="firebrick") + # confidence intervall 
      geom_line(data=newdata_JC1_norm, aes(x=conc, y=a+p*b), color = "firebrick", linewidth = 0.8, linetype = "solid") + # line of model
         scale_x_continuous(trans='log10')+ #logtransformation of x-axis
   
         
      
      coord_cartesian(ylim=c(0, 1.19)) + 
      xlab("TCS (mg/L)") +
   scale_y_continuous(name="Mobile / Total",breaks = seq(0,1,0.2), sec.axis = sec_axis(~ (. - a)/b, name = "JC-1 (Red/green)", breaks = seq(0,1,0.2)))+
      #  breaks=seq(0, 1, 0.2)+

    theme(
      axis.title.x = element_text(color = "black", size=17),
      axis.text.x = element_text(color = "black", size = 15),
      axis.title.y = element_text(color = "darkcyan", size=17),
       axis.title.y.right = element_text(color = "firebrick", size=17,margin = margin(t = 0, r = 0, b = 0, l = 10)),
      axis.text.y.left = element_text(color = "darkcyan", size = 15),
      axis.text.y.right = element_text(color = "firebrick", size = 15)
      
    )+
      geom_text(aes(x= cmax-1, y= 1.1, label ="24h", family="serif"), size=15)

        
