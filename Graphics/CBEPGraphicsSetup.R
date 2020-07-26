###########################
# Minimal Graphic Defaults
###########################

# A script to set up and support Casco Bay Estuary Partnership 
# technical graphic standards for the 2020 State of the Bay
# This script is to be called at the start of R Notebooks generating
# Final draft graphics State of teh Bay, for transmittal to the designer.
# Editing specifications here will therefore affect the look of output
# from all (or almost all) State of the Bay technical graphics.

#----------------------
##  Supporting Libraries
#----------------------
library(ggplot2)
library(extrafont)
library(ggthemes)

#----------------------
## Colors
#---------------------
# browser()
print('Colors...')
cbep_colors <- c("#006d87", "#ffde6e", "#303030", "#c1ca38", "#328a9f", "#72aebd")
cbep_colors2 <- c("#303030", "#006d87", "#328a9f", "#72aebd", "#c1ca38", "#ffde6e")
#---------------------
## Fonts
#---------------------
print('Fonts...')
font_import(pattern = '[C/c]orbel', prompt=FALSE)
font_import(pattern = '[V/v]erdana', prompt=FALSE)
suppressMessages(loadfonts(device = "win"))

# The following is needed for PDF output or Postscript output.
# For PDF or PS output, you may also want to embed fonts into the final PDF 
#Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.27/bin/gswin64c.exe")
#suppressMessages(loadfonts(device = "pdf"))
#suppressMessages(loadfonts(device = "pdf"))
#---------------------
## Geom Defaults
#---------------------
# The default line weights look thin to my eye with the CBEP colors and the tufte format.
# We also want annotations to use a specific selected CBEP font.

# From a stack overflow page here:
# https://stackoverflow.com/questions/53418209/how-to-set-a-default-line-size-in-a-ggplot2-theme-ggtheme
print('Geom Defaults...')
update_geom_defaults("line", list(size = 1))   #Native default is size=0.5
update_geom_defaults("text", list(family='Corbel'))      # So similar to theme plotted characters
#----------------------
## Custom Theme
#----------------------
print('CBEP Custom Theme...')
theme_cbep <- function(base_size=16,
                       base_family='Corbel',
                       ...)  {
  theme_tufte(base_size=base_size, base_family=base_family, ...)
  # Other theme modifications could be added here with a call to theme()
}

theme_set(theme_cbep())

