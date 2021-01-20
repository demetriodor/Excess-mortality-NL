library(extrafont) # to embed extra fonts
library(sysfonts) # to check available fonts and download fonts from google
library(showtext) # to use the extra fonts
library(emojifont) # to insert logos and emojis

## add custom fonts
font_add_google('Quattrocento') #get the fonts 
font_add_google('Quattrocento Sans')
font_families() #check that the fonts are installed and available

showtext_auto() #this is to turn on the custom fonts availability
showtext_opts(dpi = 96) #set the resolution: 96 is default

## color settings
background.color = rgb(251, 248, 255, max=255) # color for the background: magnolia
dark.color = rgb(24, 24, 38, max=255) # dark color: almost black

new.red = rgb(255,3,62, max=255)
new.redlight = rgb(255,92,130, max=255)
new.reddark = rgb(209,0,0, max=255)

new.blue = rgb(3,66,255, max=255)
blue.twitter = rgb (29, 161, 242, max=255) # twitter blue

s=3
offset = 0.01
mtext.title = 2.1*s
mtext.subtitle = 1.6*s
mtext.sign = 1.1*s
mtext.sign.emo = 1.4*s

options(scipen=999)
