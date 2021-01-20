### Data preparation for the figure
# Start with loading the general dataset
load(file = './data/nl_mortality_pop_covid_weather2020.RData')

# Source the necessary packages and functions
source('00 common packages and functions.R')

# Source the graphical settings
source('00 common graphical settings.R')

# Define the statistical model
m20<-rlm(deaths ~ n_days + sin(2*pi*week/52) + cos(2*pi*week/52) + share.65 + t.min + t.max, data=d, psi = psi.hampel)

# Prepare the output table
out2 <- data.frame (matrix(NA, nrow = 4, ncol = 2 + 1*(length(2011:2020))))

colnames(out2) <- c('Model','Measure', paste0('M.', 2011:2020))
out2$Model <- 'rlm.sine.share65.temp'
out2$Measure <- c('MSE', 'MAE', 'Net excess', 'Net excess share')

for (i in 2011:2020) {
  d.model <- d %>%  
    filter (year > (i - 9), year < i) 
  d.eval <- d %>%  
    filter (year == i) 
  
    out2[1,2+(i-2010)]<- round(mean((predict (update(m20, data=d.model), type='response', newdata=d.eval) - d.eval$deaths)^2), 0) # MSE
    out2[2,2+(i-2010)]<- round(mean(abs(predict (update(m20, data=d.model), type='response', newdata=d.eval) - d.eval$deaths)), 0) # MAE
    out2[3,2+(i-2010)]<- round(sum(d.eval$deaths) - sum(predict (update(m20, data=d.model), type='response', newdata=d.eval)), 0) # Net excess
    out2[4,2+(i-2010)]<- round((sum(d.eval$deaths) - sum(predict (update(m20, data=d.model), type='response', newdata=d.eval))) /
                          sum(predict (update(m20, data=d.model), type='response', newdata=d.eval))*100, 0) # Net excess %
}

out2

# Start the figure --------------------------------------------------------
y.min <- -4500
y.max <- 15000

png ('./figures/F_NL_excess_mortality_overtime_19012021.png', width=1280*s, height=905.5*s, res=96)

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

plot(NULL, xlim=c(0, 11), ylim=c(y.min, y.max), yaxt = 'n', xaxt = 'n') 

axis (1, 
      line = -1.5, # position
      tck = -0.00,
      lwd = 1*s,
      col = background.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 0.9,
      font=2, # font type (bold)
      at=seq(1,10, by=1), # where to put labels  
      labels= seq(2011,2020,1),
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
      at=seq(-4000, y.max, 2000), # where to put labels  
      labels= format(seq(-4000, y.max, 2000), big.mark=','), # text of labels 
      las=1 # orientation of the labels
)

segments (x0=seq(0,11,1), x1=seq(0,11,1), y0=rep(y.min,12), y1=rep(y.max,12), col='white', lwd=1*s )
segments (x0=rep(0,10), x1=rep(11,10), y0=seq(-4000,y.max, 2000), y1=seq(-4000,y.max, 2000), col='white', lwd=1*s )

rect (xleft = seq(0.60, 0.60+9, 1), xright = seq(1.40, 1.40+9, 1), ybottom = pmin(rep(0, 10), out2[3,3:12]), ytop = pmax(out2[3,3:12], rep(0,10)), col=ifelse(out2[3,3:12]>0,new.red, new.blue), border='white')

text (x = 0.2, y =13000, 'Expected mortality for each year is based on the predictions from robust statistical models\nwith a smooth weekly trend, the population share of old people and temperature extremes.', 
      col=dark.color, cex = 0.9, adj = 0)


#title
mtext(expression(bold('Net excess mortality in the Netherlands, 2011-2020')),
      side = 3, line = 3, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col=dark.color, cex = mtext.title)

mtext(expression('Positive excess mortality is in red. ' * phantom('Negative excess mortality is in blue.')),
      side = 3, line = 1, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col=new.red, cex = mtext.subtitle)

mtext(expression(phantom('Positive excess mortality is in red. ') * 'Negative excess mortality is in blue.'),
      side = 3, line = 1, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col=new.blue, cex = mtext.subtitle)

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
