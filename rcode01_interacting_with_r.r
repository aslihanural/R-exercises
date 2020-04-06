############################################################################

# start RStudio

# open this script: Menu 'File' - Open File (or Ctrl+o / Command+o)

# elements (panes) of RStudio:
# - top left:     Script Editor
# - bottom left:  Console
# - top right:    Environment, Command History, ...
# - bottom right: File Browser, Plots, Packages, Help, Viewer

# first check that it says "R version 3.6.3 (2020-02-29)" in the Console; if
# not, you do not have the current version of R installed (maybe you should
# update, especially if the version installed is quite old)

############################################################################

# using R interactively

# click on the Console and type in:

x <- c(4,2,3,6)
mean(x)

# to see the contents of an object, just type its name

x

# that is in fact just a shortcut for using the print() function

print(x)

# note: object 'x' is listed under 'Environment' in RStudio

# try out tab completion (works in the Console and Editor):
# - type 'box' - should get a list with possible matches
# - type 'p' - now 'boxplot' should be the first option
# - hit tab (or enter); should complete to 'boxplot'
# - now complete with '(x)' so we get

boxplot(x)

# remember: you can run a command from the script by putting the cursor
# in the same line as the command (anywhere, doesn't have to be at the
# beginning of the line) and then hitting the keyboard shortcut:
# - Windows: Ctrl+Enter
# - Mac OS:  Command+Enter
# note: cursor automatically moves to the next command
# you can also select/highlight multiple lines and run them all at once
# you can also run the entire script (don't do this now!) with:
# - Windows: Ctrl+Shift+Enter
# - Mac OS:  Command+Shift+Enter

# nesting of commands

mx <- mean(x)
mx
sqrt(mx)

sqrt(mean(x))

# list objects in environment (see also 'Environment' in RStudio)

ls()

# remove objects

rm(x, mx)

# now what are the objects in the environment?

ls()

# close R/RStudio (remember to choose 'no' when asked to save the workspace)

quit()

# or just close the window

# to avoid the prompt for saving the workspace

quit(save="no")

############################################################################

# restart RStudio

# the script should automatically load (i.e., RStudio automatically reopens
# the script(s) you last worked on)

# list objects in environment

ls()

# if ls() shows lots of objects in your workspace, then apparently you saved
# the workspace at some point in the past; RStudio automatically restores it

# to remove all objects from your workspace

rm(list=ls())

# (can also go to Menu 'Session' - 'Clear Workspace')

# to delete the (hidden) file that contains the workspace

unlink(".RData")

# recommendation:
# Menu 'Tools' - Global Options:
# - uncheck 'Restore .RData into workspace at startup'
# - set 'Save workspace to .RData on exit' to 'Never'

# if you don't like that RStudio adds open and closing brackets when
# completing a command: Code - uncheck 'Insert matching parens/quotes'

# depending on your computer / OS, double-clicking on an .r (.R) file can
# automatically load an R script in RStudio (might have to first tell your
# OS to always use RStudio to use for opening .r (.R) files)

############################################################################

# working directory

# the 'working directory' is the directory/folder where R/RStudio will look
# for files (e.g., datasets you want to load) or where it will save files to
# (e.g., graphs you want to save so that they can be imported into a paper or
# presentation; we will learn how to do this later on)

# check your working directory

getwd()

# if this is *not* the directory/folder where you put the course materials:
#
# Menu 'Session' - Set Working Directory - 'To Source File Location'
#
# this sets the working directory to the location of the script (note that
# this actually runs the setwd() command with the correct location)

# check your working directory again

getwd()

# another approach: in the 'Files' tab in the bottom right pane, click your
# way to the directory/folder with the materials, then click 'More', and
# select 'Set As Working Directory'

# don't forget to save the script once in a while (Ctrl+s / Command+s)
# and add comments to the script as needed

############################################################################
