### Data preparation for the figure
# This is the same for the figure F nl mortality 2020 total.R
d.2020$excess <- d.2020$deaths - predict.m4d
ci <- mean(head(predict.m4d.down-predict.m4d[1:52]))
y.min <- -200
y.max <- 2000

w = 53
s = 3

png ('./figures/F_NL_excess_mortality_19012021.png', width=1280*s, height=905.5*s, res=96)

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

plot(NULL, xlim=c(1.5, 52.5), ylim=c(y.min-90, y.max), yaxt = 'n', xaxt = 'n') 

axis (1, 
      line = -1.5, # position
      tck = -0.00,
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
      at=seq(y.min, y.max, 200), # where to put labels  
      labels= format(seq(y.min, y.max, 200), big.mark=','), # text of labels 
      las=1 # orientation of the labels
)

segments (x0=seq(1,53,10), x1=seq(1,53,10), y0=rep(y.min-50,6), y1=rep(y.max,6), col='white', lwd=1*s )
segments (x0=rep(0,13), x1=rep(53.5,13), y0=seq(y.min,y.max, 200), y1=seq(y.min,y.max, 200), col='white', lwd=1*s )


rect (xleft = seq(0.60, 0.60+52, 1), xright = seq(1.40, 1.40+52, 1), ybottom = pmin(rep(0, 53), d.2020$excess), ytop = pmax(d.2020$excess, rep(0,53)), col=ifelse(d.2020$excess>0,new.red, new.blue), border='white')
segments (x0=rep(0,1), x1=rep(53.5,1), y0=0, y1=0, col=dark.color, lwd=2*s, lty=1)
segments (x0=rep(0,1), x1=rep(53.5,1), y0= -ci, y1=-ci, col=dark.color, lwd=2*s, lty=3)
segments (x0=rep(0,1), x1=rep(53.5,1), y0=ci, y1=ci, col=dark.color, lwd=2*s, lty=3)
lines(x=1:53, d.2020$covid, col='darkgrey', lwd=2*s)

text (x = 17.75, y = 890, 'Excess mortality is defined as observed minus expected number of deaths.', 
      col=dark.color, cex = 0.70, adj = 0)
text (x = 17.75, y = 835, 'Expected mortality is based on the predictions from a statistical model.', 
      col=dark.color, cex = 0.70, adj = 0)
text (x = 17.75, y =760, 'Dotted lines show the borders of the 95% prediction intervals.', col=dark.color, cex = 0.70, adj = 0)
text (x = 17.75, y =705, 'Grey line shows confirmed COVID-19 related deaths.', col=dark.color, cex = 0.70, adj = 0)

text1 = paste0('Net excess mortality during 2020 is ', my.round(net.excess), ' deaths, which is ', my.round(net.excess.percent), '% over the expected mortality.')
text (x = 17.75, y =945, text1, col=dark.color, cex = 0.70, adj = 0)


#title
mtext(expression(bold('Excess mortality in the Netherlands during 2020, per week')),
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

