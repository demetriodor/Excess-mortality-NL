### Data preparation for the figure
# Start with loading the gender-specific datasets
load(file = './data/nl_80_mortality_pop_covid_weather2020.RData')
load(file = './data/nl_65_mortality_pop_covid_weather2020.RData')
load(file = './data/nl_0_mortality_pop_covid_weather2020.RData')

# Source the necessary packages and functions
source('00 common packages and functions.R')

# Source the graphical settings
source('00 common graphical settings.R')

# Figure specific data manipulation for 80plus  ----------------------------
# Prepare data for model building and evaluation
i = 2020
d.model <- d.80plus %>% filter (year > (i - 9), year < i) 
d.eval  <- d.80plus %>% filter (year == i) 

# Model for predictions
m4d<-rlm(deaths ~ sin(2*pi*week/52) + cos(2*pi*week/52) + counter + t.min + t.max + n_days, data=d.model, psi = psi.hampel)
predict.m4d.set<-predict (m4d, newdata=d.eval, se.fit=TRUE, interval="prediction", level=0.95)
predict.m4d<-predict.m4d.set$fit[,1]
predict.m4d.up<-predict.m4d.set$fit[,2]
predict.m4d.down<-predict.m4d.set$fit[,3]

# Define and calculate some quantities
mae.m4d<-mean(abs(predict.m4d - d.eval$deaths))
mse.m4d<-mean((predict.m4d - d.eval$deaths)^2)
total.deaths = sum(d.eval$deaths)
total.covid = sum (d.eval$covid.80plus)
total.predicted = sum(predict.m4d)
net.excess = total.deaths - total.predicted
net.excess.percent = net.excess/total.predicted*100

d.eval$excess <- d.eval$deaths - predict.m4d
ci <- mean(head(predict.m4d.down-predict.m4d))
d.eval$covid.80plus[47]<-NA

# Start the figure for 80plus --------------------------------------------------------
y.min <- -200
y.max <- 1400

w = 52
s = 3


png ('./figures/F_NL_age_excess_mortality_19012021.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,3), # number and distribution of plots
    oma=c(1,0,4,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(3,3,0,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    #yaxs="i",
    bg=background.color, # background color
    family='Quattrocento' # font family
    
)

plot(NULL, xlim=c(1.5, 51.5), ylim=c(y.min, y.max), yaxt = 'n', xaxt = 'n') 

axis (1, 
      line = -0.5, # position
      tck = -0.00,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.60,
      font=2, # font type (bold)
      at=seq(1,w, by=10), # where to put labels  
      labels= c('Week 1:\n1-9 January', 'Week 11:\n9-15 March','Week 21:\n18-24 May','Week 31:\n27 July-2 August','Week 41:\n5-11 October','Week 51:\n14-20 December'),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.001,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.7, 
      font=2, # font type (bold)
      at=seq(y.min, y.max, 200), # where to put labels  
      labels= format(seq(y.min, y.max, 200), big.mark=','), # text of labels 
      las=1 # orientation of the labels
)

segments (x0=seq(1,52,10), x1=seq(1,52,10), y0=rep(y.min-50,6), y1=rep(y.max,6), col='white', lwd=1*s )
segments (x0=rep(0,13), x1=rep(52.5,13), y0=seq(y.min,y.max, 200), y1=seq(y.min,y.max, 200), col='white', lwd=1*s )

rect (xleft = seq(0.60, 0.60+51, 1), xright = seq(1.40, 1.40+51, 1), ybottom = pmin(rep(0, 52), d.eval$excess), ytop = pmax(d.eval$excess, rep(0,52)), col=ifelse(d.eval$excess>0,new.red, new.blue), border='white')
segments (x0=rep(0,1), x1=rep(52.5,1), y0=0, y1=0, col=dark.color, lwd=2*s, lty=1)
segments (x0=rep(0,1), x1=rep(52.5,1), y0= -ci, y1=-ci, col=dark.color, lwd=2*s, lty=3)
segments (x0=rep(0,1), x1=rep(52.5,1), y0=ci, y1=ci, col=dark.color, lwd=2*s, lty=3)
lines(x=1:52, y=d.eval$covid.80plus, col='darkgrey', lwd=2*s)

text1 = paste0('Net excess mortality for\n80+ is ', my.round(net.excess), ' deaths, which is\n', my.round(net.excess.percent), '% over the expected.')
text (x = 18.75, y = 1100, text1, col=dark.color, cex = 0.70, adj = 0)
text (x = 1.75, y = 1350, '80+', col=new.blue, cex = 2, adj = 0)


#title
mtext(expression(bold('Excess mortality by age group in the Netherlands during 2020, per week')),
      side = 3, line = 3, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col=dark.color, cex = mtext.title)
mtext(expression(italic('Grey lines show confirmed COVID-19 deaths')),
      side = 3, line = 1, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='darkgrey', cex = mtext.subtitle)

#data statement
mtext(text = fontawesome('fa-table'), 
      side=1, line=-1, outer=T,
      col=new.reddark, cex=mtext.sign.emo, at = offset, 
      font=1, family='fontawesome-webfont', 
      adj=0, padj=0.8)

mtext(text=expression("Data: " * phantom("CBS StatLine, ECDC")), 
      side=1, line=-1, outer=T, at = offset + 0.03,
      col=dark.color, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=0, padj=1)
mtext(text=expression(phantom("Data: ") * "CBS StatLine, ECDC"), 
      side=1, line=-1, outer=T, at = offset + 0.03,
      col=new.reddark, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=0, padj=1)

#signature
mtext(text=expression(phantom("@DToshkov        ") * " http://dimiter" * phantom(".eu")), 
      side=1, line=-1, outer=T, at = 1 - offset - 0.02,
      col=dark.color, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=1, padj=1)

mtext(text=expression(phantom("@DToshkov         http://dimiter") * ".eu"),
      side=1, line=-1, outer=T, at = 1 - offset - 0.02,
      col=new.reddark, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=1, padj=1)

mtext(text=expression("@DToshkov        " * phantom(" http://dimiter.eu")), 
      side=1, line=-1, outer=T, at = 1 - offset - 0.02,
      col=blue.twitter, cex=mtext.sign,
      font=1, family='Quattrocento Sans', 
      adj=1, padj=1)

mtext(text= fontawesome('fa-twitter'), 
      side=1, line=-1, outer=T,
      col=blue.twitter, cex=mtext.sign.emo, at = 1 - 0.14, 
      font=1, family='fontawesome-webfont', 
      adj=1, padj=0.8)

mtext(text= fontawesome('fa-rss'), 
      side=1, line=-1, outer=T,
      col=new.reddark, cex=mtext.sign.emo, at = 1 - offset, 
      font=1, family='fontawesome-webfont', 
      adj=1, padj=0.8)
#dev.off()

# Figure specific data manipulation for 65to80  --------------------------
# Prepare data for model building and evaluation
i = 2020
d.model <- d.65to80 %>% filter (year > (i - 9), year < i) 
d.eval  <- d.65to80 %>% filter (year == i) 

# Model for predictions
m4dw<-rlm(deaths ~ sin(2*pi*week/52) + cos(2*pi*week/52) + counter + t.min + t.max + n_days, data=d.model, psi = psi.hampel)
predict.m4dw.set<-predict (m4dw, newdata=d.eval, se.fit=TRUE, interval="prediction", level=0.95)
predict.m4dw<-predict.m4dw.set$fit[,1]
predict.m4dw.up<-predict.m4dw.set$fit[,2]
predict.m4dw.down<-predict.m4dw.set$fit[,3]

# Define and calculate some quantities
mae.m4dw<-mean(abs(predict.m4dw - d.eval$deaths))
mse.m4dw<-mean((predict.m4dw - d.eval$deaths)^2)
total.deaths.w = sum(d.eval$deaths)
total.covid.w = sum (d.eval$covid.65to80)
total.predicted.w = sum(predict.m4dw)
net.excess.w = total.deaths.w - total.predicted.w
net.excess.percent.w = net.excess.w/total.predicted.w*100

d.eval$excess <- d.eval$deaths - predict.m4dw
ci <- mean(head(predict.m4dw.down-predict.m4dw))
d.eval$covid.65to80[47]<-NA

# Start the figure for 65to80 --------------------------------------------------------
plot(NULL, xlim=c(1.5, 51.5), ylim=c(y.min, y.max), yaxt = 'n', xaxt = 'n') 

axis (1, 
      line = -0.5, # position
      tck = -0.00,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.60,
      font=2, # font type (bold)
      at=seq(1,w, by=10), # where to put labels  
      labels= c('Week 1:\n1-9 January', 'Week 11:\n9-15 March','Week 21:\n18-24 May','Week 31:\n27 July-2 August','Week 41:\n5-11 October','Week 51:\n14-20 December'),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.001,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.7, 
      font=2, # font type (bold)
      at=seq(y.min, y.max, 200), # where to put labels  
      labels= format(seq(y.min, y.max, 200), big.mark=','), # text of labels 
      las=1 # orientation of the labels
)

segments (x0=seq(1,52,10), x1=seq(1,52,10), y0=rep(y.min-50,6), y1=rep(y.max,6), col='white', lwd=1*s )
segments (x0=rep(0,13), x1=rep(52.5,13), y0=seq(y.min,y.max, 200), y1=seq(y.min,y.max, 200), col='white', lwd=1*s )


rect (xleft = seq(0.60, 0.60+51, 1), xright = seq(1.40, 1.40+51, 1), ybottom = pmin(rep(0, 52), d.eval$excess), ytop = pmax(d.eval$excess, rep(0,52)), col=ifelse(d.eval$excess>0,new.red, new.blue), border='white')
segments (x0=rep(0,1), x1=rep(52.5,1), y0=0, y1=0, col=dark.color, lwd=2*s, lty=1)
segments (x0=rep(0,1), x1=rep(52.5,1), y0= -ci, y1=-ci, col=dark.color, lwd=2*s, lty=3)
segments (x0=rep(0,1), x1=rep(52.5,1), y0=ci, y1=ci, col=dark.color, lwd=2*s, lty=3)
lines(x=1:52, y=d.eval$covid.65to80, col='darkgrey', lwd=2*s)

text1 = paste0('Net excess mortality for\n65-80 is ', my.round(net.excess.w), ' deaths, which is\n', my.round(net.excess.percent.w), '% over the expected.')
text (x = 1.75, y = 1350, '65-80', col=new.blue, cex = 2, adj = 0)
text (x = 18.75, y = 1100, text1, col=dark.color, cex = 0.70, adj = 0)

#dev.off()

# Figure specific data manipulation for 0to65  --------------------------
# Prepare data for model building and evaluation
i = 2020
d.model <- d.0to65 %>% filter (year > (i - 9), year < i) 
d.eval  <- d.0to65 %>% filter (year == i) 

# Model for predictions
m4d0<-rlm(deaths ~ sin(2*pi*week/52) + cos(2*pi*week/52) + counter + t.min + t.max + n_days, data=d.model, psi = psi.hampel)
predict.m4d0.set<-predict (m4d0, newdata=d.eval, se.fit=TRUE, interval="prediction", level=0.95)
predict.m4d0<-predict.m4d0.set$fit[,1]
predict.m4d0.up<-predict.m4d0.set$fit[,2]
predict.m4d0.down<-predict.m4d0.set$fit[,3]

# Define and calculate some quantities
mae.m4d0<-mean(abs(predict.m4d0 - d.eval$deaths))
mse.m4d0<-mean((predict.m4d0 - d.eval$deaths)^2)
total.deaths.0 = sum(d.eval$deaths)
total.covid.0 = sum (d.eval$covid.0to65)
total.predicted.0 = sum(predict.m4d0)
net.excess.0 = total.deaths.0 - total.predicted.0
net.excess.percent.0 = net.excess.0/total.predicted.0*100

d.eval$excess <- d.eval$deaths - predict.m4d0
ci <- mean(head(predict.m4d0.down-predict.m4d0))
d.eval$covid.0to65[47]<-NA

# Start the figure for 0to65 --------------------------------------------------------
plot(NULL, xlim=c(1.5, 51.5), ylim=c(y.min, y.max), yaxt = 'n', xaxt = 'n') 

axis (1, 
      line = -0.5, # position
      tck = -0.00,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.60,
      font=2, # font type (bold)
      at=seq(1,w, by=10), # where to put labels  
      labels= c('Week 1:\n1-9 January', 'Week 11:\n9-15 March','Week 21:\n18-24 May','Week 31:\n27 July-2 August','Week 41:\n5-11 October','Week 51:\n14-20 December'),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.001,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.7, 
      font=2, # font type (bold)
      at=seq(y.min, y.max, 200), # where to put labels  
      labels= format(seq(y.min, y.max, 200), big.mark=','), # text of labels 
      las=1 # orientation of the labels
)

segments (x0=seq(1,52,10), x1=seq(1,52,10), y0=rep(y.min-50,6), y1=rep(y.max,6), col='white', lwd=1*s )
segments (x0=rep(0,13), x1=rep(52.5,13), y0=seq(y.min,y.max, 200), y1=seq(y.min,y.max, 200), col='white', lwd=1*s )


rect (xleft = seq(0.60, 0.60+51, 1), xright = seq(1.40, 1.40+51, 1), ybottom = pmin(rep(0, 52), d.eval$excess), ytop = pmax(d.eval$excess, rep(0,52)), col=ifelse(d.eval$excess>0,new.red, new.blue), border='white')
segments (x0=rep(0,1), x1=rep(52.5,1), y0=0, y1=0, col=dark.color, lwd=2*s, lty=1)
segments (x0=rep(0,1), x1=rep(52.5,1), y0= -ci, y1=-ci, col=dark.color, lwd=2*s, lty=3)
segments (x0=rep(0,1), x1=rep(52.5,1), y0=ci, y1=ci, col=dark.color, lwd=2*s, lty=3)

lines(x=1:52, y=d.eval$covid.0to65, col='darkgrey', lwd=2*s)

text1 = paste0('Net excess mortality for\n0-65 is ', my.round(net.excess.0), ' deaths, which is\n', my.round(net.excess.percent.0), '% over the expected.')
text (x = 1.75, y = 1350, '0-65', col=new.blue, cex = 2, adj = 0)
text (x = 18.75, y = 1100, text1, col=dark.color, cex = 0.70, adj = 0)

dev.off()
