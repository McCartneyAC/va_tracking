# va_tracking
school district level statistics to examine tracking and leveled-ness of courses

There are four main folders: one containing various data files, of which `analytic_data.csv` is the most complete file. One contains `R` code for analysis, of which `eda.R` is the original file. All current work is being done in ` post_feedback_models.R` to incorporate feedback from reviewers in the first round of submission. 

A third folder has data visualizations in no particular organization. A fourth folder contains past versions of this project as submitted to conferences in preparation of this manuscript for publication. 

## lovely pics
![](https://github.com/McCartneyAC/va_tracking/blob/master/data_vis/diversity_and_trackedness_map.png?raw=true)
![](https://github.com/McCartneyAC/va_tracking/blob/master/data_vis/plot_c_names_expenditure.png?raw=true?raw=true)


## ANDREW TO DO: 
* do an apples-to-apples comparison of d_index for the whole district to d_index for ADV studies (d_ADV?) 
  like `d_adv ~ d_index + census + frpl` within the path analysis. 
* poisson-like regression on AP clases ~ d_index + etc. (this isn't really ZIP but it feels ZIPpy) 


## OCR data clean
Gets you just the virginia OCR data. 

```r 
# ocr data clean
library(dplyr)
library(readr)

# list your file path working directory here. 
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis\\va_tracking-master\\data_files\\ocr2017\\")
filepath <- getwd()

files <- list.files(path=filepath, pattern="*.csv", full.names=TRUE, recursive=FALSE)
for (i in 1:length(files)) {
  tmp <- read_csv(files[i])    
  tmp <- tmp %>%     
      filter(LEA_STATE_NAME == "VIRGINIA")    
  write_csv(tmp, files[i])
}
```
