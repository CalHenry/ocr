###########################################################################
###########################################################################
###                                                                     ###
###                           MASTER FILE                               ###
###                           OCR project                               ###
###                           Calixte HENRY                             ###
###                                                                     ###
###########################################################################
###########################################################################



# Packages ----------------------------------------------------------------
library(tidyverse)
library(tesseract)
library(pdftools)
library(magick)
#
library(hunspell)
library(quanteda)
#
library(openxlsx)
# Master file for ----


# functions ----------------------------------------------------------------
source("code/functions/image_processing_function_even.R")
source("code/functions/image_processing_function_odd.R")
source("code/functions/split_and_process_ocr_text.R")

# Documentation for the functions can be found in the "Functions documentation.Rmd" file.