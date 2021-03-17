FROM openanalytics/r-base

MAINTAINER Jason Yang "jason.jian.yang@amway.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 

# system library dependency for word cloud app
RUN apt-get update && apt-get install -y \
    libxml2-dev 

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of word cloud app
RUN R -e "install.packages(c('jiebaR','wordcloud2','dplyr','stringr','pacman','tidyverse') , repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/wordcloud
COPY wordcloud /root/wordcloud

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/wordcloud')"]