<div align="center">     

# ESPERANTO:      
## a GLP-fied sEmi-SuPERvised toxicogenomics meta-dAta curatioN TOol
</div>
      
      
<div align="justify">
ESPERANTO is a tool developed in R Shiny that enables a standardised semi-supervised harmonisation and integration of toxicogenomics metadata and increases their FAIRness (Findable, Accessible, Interoperable and Reusable) in a Good Laboratory Practice (GLP)-compliant fashion. Biological data repositories are an invaluable source of publicly available research evidence. Unfortunately, the lack of convergence of the scientific community on a common metadata annotation strategy has resulted in large amounts of data with poor FAIRness.      <br /> 
An experienced human curator is not repleaceable at the moment, hence the goal of ESPERANTO is to semi automatise the curation process, involving the user during the process.      <br />
The graphical interface is designed to support the user in metadata harmonisation in a user-friendly manner, regardless of the level of expertise and the background.     <br />
At last, ESPERANTO tracks each performed modification in a detailed GLP-compliant report, ensuring the reconstruction of the whole pipeline behind the curation of reliable, reproducible, and high-quality data.       
</div>             
       
 <br />     
 
## User Guide     
An updated and detailed user guide is available [here](https://github.com/fhaive/esperanto/wiki/ESPERANTO-User-Guide).     
<br />    
 
 
## How do I get set up?      
### Dependencies     
      
magrittr 2.0.3      
dplyr 1.0.10      
data.table 1.14.4      
zeallot 0.1.0      
collections 0.3.6     
reshape2 1.4.4     
tibble 3.1.8      
shinyWidgets 0.7.4     
stringr 1.4.1      
readxl 1.4.1     
DT 0.26     
xlsx 0.6.5     
tidyverse 1.3.2     
shinycssloaders 1.0.0    
kableExtra 1.3.4    
knitr 1.40    
plotly 4.10.1     
shiny 1.7.3    
shinyjs 2.1.0    
shinyBS 0.61.1   
shinyalert 3.0.0    
shinyFeedback 0.4.0    
shinydashboard 0.7.2     
rhandsontable 0.3.8    
<br />    

## Running ESPERANTO from Docker Image file
If needed, you can download and install Docker by following the instructions at [Get-Docker](https://docs.docker.com/get-docker/).        

ESPERANTO Dockerfile is available [here](https://github.com/fhaive/esperanto).       
Once downloaded, open the terminal, select the folder containing the Docker image and launch it:      
```
bash ./run_esperanto.sh 
```     

Open the browser and add the following line in the URL-bar.     
```     
localhost:3838     
```
The input files to upload must be placed in the same folder where the Dockerfile is located.    
<br />

## Using ESPERANTO from GitHub      
### Install R Dependencies      
```
cran_pkgs <- c("magrittr", "dplyr", "data.table", "zeallot", "collections", "reshape2", "tibble", "shinyWidgets", 
               "stringr", "readxl", "DT", "xlsx", "tidyverse", "shinycssloaders", "kableExtra",  "knitr", "plotly", 
               "shiny", "shinyjs", "shinyBS", "shinyalert", "shinyFeedback", "shinydashboard", "rhandsontable")       
cran_pkgs.inst <- cran_pkgs[!(cran_pkgs %in% rownames(installed.packages()))]
if(length(cran_pkgs.inst)>0){
  print(paste0("Missing ", length(cran_pkgs.inst), " CRAN Packages:"))
  for(pkg in cran_pkgs.inst){
    print(paste0("Installing Package:'", pkg, "'..."))
    install.packages(pkg, repo="http://cran.rstudio.org", dependencies=TRUE)
    print("Installed!!!")
  }
}
```      

<div align="justify">
If packages based on rJava (i.e. rlang) are not installed correctly, the user should try to update the rJava package and update/install the most recent Java version from www.java.com.     
<br/ >   

```     
remove.packages(”rJava”)    
```

If the error holds after the update/install of Java, it is probably originated by rJava and Java using different versions (i.e 32 vs 64 bits). In this case, the solution is to install the same Java version and update manually the Java path with the new Java version: i.e, version jre1.8.0_121 (64-bits).     
```     
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_121')    
```      
</div>   

### option 1. Run ESPERANTO from GitHub      
```    
# Load 'shiny' library
  library(shiny)
  library(shinyjs)
# run on the host port 8787 (or any other port you want to map on your system)
  runGitHub("esperanto", "fhaive", subdir= "ESPERANTO_app)     
```     

### option 2. Run ESPERANTO locally     
```    
# Clone the git repository
  git clone https://github.com/fhaive/esperanto ESPERANTOclone

# Start R session, set the folder where the clone was stored and run by using runApp()
  setwd("./ESPERANTOclone")
  library(shiny)
  runApp(".")    
```     
The input files to upload must be placed in "ESPERANTOclone" folder.     
<br />    

## Usage    
The folder "[sample_data](https://github.com/fhaive/esperanto/tree/master/sample_data)" contains the input files used for each dataset curation and the multiple datasets integration steps performed during the case study.      
The whole set of files (inputs and saved intermediate sessions) as well as the generated reports and outcomes of the case study are available in "[case_study_files](https://github.com/fhaive/esperanto/tree/master/case_study_files)" repository.

For detailed information about the use of ESPERANTO, please refer to the [user guide](https://github.com/fhaive/esperanto/wiki/ESPERANTO-User-Guide) provided.
