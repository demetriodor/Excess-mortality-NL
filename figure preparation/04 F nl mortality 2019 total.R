### Data preparation for the figure

# Start with loading the general dataset
load(file = './data/nl_mortality_pop_covid_weather2020.RData')

# Source the necessary packages and functions
source('00 common packages and functions.R')

# Source the graphical settings
source('00 common graphical settings.R')


# Figure specific data manipulation
# Prepare data for model building and evaluation
i = 2019
d.model <- d %>% filter (year > (i - 9), year < i) 
d.model5 <- d %>% filter (year > (i - 5), year < i) 
d.eval  <- d %>% filter (year == i) 

# Model for predictions
m4d<-rlm(deaths ~ sin(2*pi*week/52) + cos(2*pi*week/52) + share.65 + t.min + t.max + n_days, data=d.model, psi = psi.hampel)
predict.m4d.set<-predict (m4d, newdata=d.eval, se.fit=TRUE, interval="prediction", level=0.95)
predict.m4d<-predict.m4d.set$fit[,1]
predict.m4d.up<-predict.m4d.set$fit[,2]
predict.m4d.down<-predict.m4d.set$fit[,3]

m1<-lm(deaths ~ as.character(week) + n_days, data=d.model5)
predict.m1.set<-predict (m1, newdata=d.eval, se.fit=TRUE, interval="prediction", level=0.95)
predict.m1<-predict.m1.set$fit[,1]

m2<-lm(deaths ~ as.character(week) + pop + n_days, data=d.model5)
predict.m2.set<-predict (m2, newdata=d.eval, se.fit=TRUE, interval="prediction", level=0.95)
predict.m2<-predict.m2.set$fit[,1]

# Start the figure --------------------------------------------------------
y.min <- 2500
y.max <- 3900

w = 52
s = 3

png ('./figures/F_NL_2019_mortality_19012021.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(1,0,4,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(3,3,0,0), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
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
      tck = -0.01,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.70,
      font=2, # font type (bold)
      at=seq(1,w, by=10), # where to put labels  
      labels= c('Week 1:\n1-9 January', 'Week 11:\n9-15 March','Week 21:\n18-24 May','Week 31:\n27 July-2 August','Week 41:\n5-11 October','Week 51:\n14-20 December'),
      las=1 # orientation of the labels
)

axis (2, 
      line = -1, # position
      tck = -0.001,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.9, 
      font=2, # font type (bold)
      at=seq(0, 6000, 500), # where to put labels  
      labels= format(seq(0, 6000, 500), big.mark=','), # text of labels 
      las=1 # orientation of the labels
)

segments (x0=seq(1,52,10), x1=seq(1,52,10), y0=rep(0,6), y1=rep(y.max,6), col='white', lwd=1*s )
segments (x0=rep(0,13), x1=rep(52,13), y0=seq(0,y.max, 500), y1=seq(0,y.max, 500), col='white', lwd=1*s )

lines (x=d.eval$week, y=d.eval$deaths, col=new.red, lwd=3*s)
lines (x=d.eval$week, y=predict.m4d, col=new.blue, lwd=3*s)
lines (x=d.eval$week, y=predict.m2, col='lightgrey', lwd=3*s)
points (x=d.eval$week, y=d.eval$deaths, bg=new.red, cex=1, pch=21, col='white')
points (x=d.eval$week, y=predict.m4d, bg=new.blue, cex=1, pch=21, col='white')
points (x=d.eval$week, y=predict.m2, bg='lightgrey', cex=1, pch=21, col='white')


text1 = paste0('The total number of deaths recorded in 2020 is ', my.round(total.deaths), ', which is ', my.round(net.excess), ', or ', my.round(net.excess.percent), '%, more than than the expected.\nConfirmed COVID-19 deaths are ', my.round(total.covid), ', which makes for ', my.round(total.covid/(net.excess)*100), '% of the excess mortatlity.')

text (x = 1.2, y =5750, text1, col=dark.color, cex = 0.70, adj = 0)
text (x = 1.2, y =6150, 'Expected mortality is based on the predictions from a robust statistical model with a smooth weekly trend, the population share of old people and temperature extremes.', 
      col=dark.color, cex = 0.70, adj = 0)

text (x = 15.75, y =4750, 'The darker shade of red shows confirmed COVID-19 related deaths.', 
      col=new.reddark, cex = 0.70, adj = 0)

#title
mtext(expression(bold('Mortality (number of deaths) in the Netherlands during 2019, per week')),
      side = 3, line = 3, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col=dark.color, cex = mtext.title)

mtext(expression('Observed mortality is in red. ' * phantom('Expected mortality is in blue. Average of past 5-years is in grey.')),
      side = 3, line = 1, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col=new.red, cex = mtext.subtitle)

mtext(expression(phantom('Observed mortality is in red. ') * 'Expected mortality is in blue.' * phantom(' Average of past 5-years is in grey.')),
      side = 3, line = 1, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col=new.blue, cex = mtext.subtitle)

mtext(expression(phantom('Observed mortality is in red. Expected mortality is in blue.') * ' Average of past 5-years is in grey.'),
      side = 3, line = 1, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='darkgrey', cex = mtext.subtitle)

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
dev.off()
