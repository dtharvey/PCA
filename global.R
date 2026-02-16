# global.R file for PCA learning module

# packages to load
library(shiny)
library(shinythemes)
library(plotrix)
library(latex2exp)
library(plotly)

suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("shiny"))
suppressPackageStartupMessages(library("shinythemes"))
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("plot3D"))
suppressPackageStartupMessages(library("scatterplot3d"))
suppressPackageStartupMessages(library("chemCal"))

# set color scheme
okabe_ito = palette("Okabe-Ito")

# set seed to fix random values below
set.seed(123)



