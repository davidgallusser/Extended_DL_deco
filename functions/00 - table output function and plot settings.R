########################################################
#### 00 - Table output function and plot settings
########################################################

########################################################
# Saving xtable output in text files
mod_xtable <- function(output.file, ...) {
  output <- capture.output(print(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=FALSE)
}

#########################################################
# Plot settings
ftype <- ".pdf"
w=15 #width in cm of pdf file
h=10 #heigth in cm of pdf file

# Set ggplot theme (light)
# old <- theme_get()
# theme_set(theme_light())
#theme_set(old)

## Set adjusted ggplot light theme
default_theme <- theme_get()
theme_light_adj <- theme_light()
# Set transparent background for printing on color
theme_light_adj$panel.background = element_rect(fill = "transparent") # bg of the panel
theme_light_adj$plot.background = element_rect(fill = "transparent", color = NA) #bg of the plot
theme_light_adj$legend.background = element_rect(fill = "transparent", color= NA) # get rid of legend bg
theme_light_adj$legend.box.background = element_rect(fill = "transparent", color= NA) # get rid of legend panel bg
# Set theme
theme_set(theme_light_adj)


# Change color scheme
scales::show_col(tableau_color_pal('Classic 10')(10)) #There are many tableau_color_pal stored in 'tableau_color_pal')
#?tableau_color_pal
scale_colour_discrete <- function(...) scale_color_brewer(palette="Set1")
scale_colour_discrete <- function(...) scale_color_tableau(palette="Classic 10")
scale_fill_discrete <- function(...) scale_fill_tableau(palette="Classic 10")

scale_colour_discrete <- function(...) scale_color_tableau(palette="Classic Color Blind")
scale_fill_discrete <- function(...) scale_fill_tableau(palette="Classic Color Blind")
