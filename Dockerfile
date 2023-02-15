FROM rocker/shiny-verse:4.2.2
RUN sudo apt-get update && sudo apt-get install -y default-jre libssh-dev
RUN R CMD javareconf
RUN Rscript -e 'install.packages(c("DT", "collections", "drc", "kableExtra", "knitr", "neo4jshell", "plotly", "rJava", "reshape2", "rhandsontable", "shinyBS", "shinyFeedback", "shinyFiles", "shinyWidgets", "shinyalert", "shinycssloaders", "shinydashboard", "shinyhelper", "shinyjs", "xlsx", "zeallot"))'

