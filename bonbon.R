#link: https://shiring.github.io/text_analysis/2017/07/17/ocr_tesseract
#https://www.opensemanticsearch.org/doc/datamanagement/ocr
#https://github.com/tesseract-ocr/tesseract/wiki/ImproveQuality
#https://blogs.dropbox.com/tech/2017/04/creating-a-modern-ocr-pipeline-using-computer-vision-and-deep-learning/

# -----------------------------------------------------------------------  
# -- 0 Clear workspace
# -----------------------------------------------------------------------

rm(list=ls())

# -----------------------------------------------------------------------  
# -- 1 Packages
# -----------------------------------------------------------------------

library(tesseract) #read text
library(hunspell) #misspellings
library(magick)

# -----------------------------------------------------------------------  
# -- 2 User definied functions and parameters
# -----------------------------------------------------------------------

path = "C:/Users/dbeem/Documents/EUR/2018 2019/Hackathon/bonbon/"
language = "nld"

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

# -----------------------------------------------------------------------  
# -- 3 Load data
# -----------------------------------------------------------------------

i <- image_read(paste0(path,"ah-bon-contrast-schadows.jpg"))

# -----------------------------------------------------------------------  
# -- 4 Improve image 
# -----------------------------------------------------------------------

i <- i %>%                      # rescale
  image_resize("1500") %>%
  image_background("white", flatten = TRUE) %>%   # set background to white
  image_convert(colorspace = 'gray') %>%
  image_trim() %>%                                # Trim edges that are the background color from the image.
  #image_noise() %>%                              # Reduce noise in image using a noise peak elimination filter
  image_enhance() %>%                             # Enhance image (minimize noise)
  #image_deskew(treshold = 40)   %>%              # deskew image -> creates negative offset in some scans
  image_normalize() %>%                           # Normalize image (increase contrast by normalizing the pixel values to span the full range of color values).
  image_contrast(sharpen = 1)                      # increase contrast
  #image_rotate(5)
               

# -----------------------------------------------------------------------  
# -- 5 Read text
# -----------------------------------------------------------------------

lang <- tesseract(language)
text <- tesseract::ocr(i, engine = lang)
cat(text)

#tesseract_params(filter = "")

results <- tesseract::ocr_data(i, engine = lang)
words <- results$word

# -----------------------------------------------------------------------  
# -- 6 Misspelling and context check
# -----------------------------------------------------------------------

dutch <- dictionary(paste0(path,"dict/nl.dic"))

words[1:5]
results$check <- hunspell_check(results$word, dict = dutch)

results$alt <- hunspell_suggest(results$word, dict=dutch)
results[(results$check=="FALSE") & (results$confidence<70),]$alt

# -----------------------------------------------------------------------  
# -- 7 Derive relevant data
# -----------------------------------------------------------------------

# 7.1 Brand & Store  (e.g. Albert Heijn, Van Woustraat 148-150)

# 7.2 List of products and prices (e.g. Speculaas)

# 7.3 Subtotal and total amount 

# 7.4 Payment method 

# 7.5 Date of purchase 


# -----------------------------------------------------------------------  
# -- Tryout: rotation
# -----------------------------------------------------------------------

opt <- c(0,0)
for (k in -10:10) {
  image_k <- image_rotate(i,k)
  results <- tesseract::ocr_data(image_k, engine = lang)
  conf <- colMeans(results[2])
  #print(paste(k,conf,sep=": "))
  if (conf > opt[1])
    opt <- c(conf,k)
}
i_rotated <- image_rotate(i,opt[2])

results <- tesseract::ocr_data(i_rotated, engine = lang)

# -----------------------------------------------------------------------  
# -- Tryout: fast cropping
# -----------------------------------------------------------------------

#maximum margin 
library(stringr)
results[,c("x1","y1", "x2", "y2")] <- as.numeric(str_split_fixed(results$bbox, ",", 4))
minCoord <- as.numeric(colMin(results[,c("x1","y1", "x2", "y2")]))
maxCoord <- as.numeric(colMax(results[,c("x1","y1", "x2", "y2")]))

#cropping
i_rotated_cropped <- image_crop(i,paste0(maxCoord[3]-minCoord[1],"x",maxCoord[4]-minCoord[2],"+",minCoord[1],"x",minCoord[2]))
results <- tesseract::ocr_data(i_rotated_cropped, engine = lang)
