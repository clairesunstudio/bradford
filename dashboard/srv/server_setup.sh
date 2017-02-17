#!/bin/bash
# Add new R CRAN
echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" | sudo tee -a /etc/apt/sources.list

# add key for r-base download
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Add ssl reference for install
sudo echo "deb http://security.ubuntu.com/ubuntu lucid-security main" >> /etc/apt/sources.list

# Update and upgrade system
sudo apt -y update
sudo apt -y dist-upgrade

# install packages both for linux and R
declare -a ubuntu_packages=('r-base'
                            'r-cran-xml'
                            'libcurl4-gnutls-dev'
                            'libxml2-dev'
                            'openssl'
                            'gdebi-core'
                            'jq'
                            'postgresql'
                            'postgresql-contrib'
                            'libpq-dev'
                            'libmariadb-client-lgpl-dev'
                            'libcairo2-dev'
                            'r-cran-slam'
                            'awscli'
                            )
for package_name in ${ubuntu_packages[@]}; do
  sudo apt install -y $package_name
done

# Install packages
declare -a packages=('shiny'
                     'reshape2'
                     'magrittr'
                     'dplyr'
                     'scales'
                     'ggplot2'
                     'RCurl'
                     'openssl'
                     'XML'
                     'RGoogleAnalytics'
                     'wordcloud'
                     'stringr'
                     'RPostgreSQL'
                     'forcats'
                     'plotly'
                     'purrr'
                     'lubridate'
                     'shinydashboard'
                     'BreakoutDetection'
                     'devtools'
                     'readr'
                     'shinyjs'
                     )

for package_name in ${packages[@]}; do
  sudo su - -c "R -e \"install.packages('$package_name', repos='https://cran.rstudio.com/', dependencies = TRUE)\""
done

declare -a git_packages=('aoles/shinyURL')

for package_name in ${git_packages[@]}; do
  sudo su - -c "R -e \"devtools::install_github('$package_name')\""
done

# get and install shiny-server
wget -O ~/shiny-server-1.5.1.834-amd64.deb https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
sudo gdebi --non-interactive ~/shiny-server-1.5.1.834-amd64.deb

# get the bradford repo
git clone https://github.com/massgov/bradford ~/

# get the necessary data from s3'
sudo mkdir ~/bradford/dashboard/data/
sudo /home/ubuntu/.local/bin/aws s3 sync s3://mass.gov-analytics/dashboards/bradford/data ~/bradford/dashboard/data/

# get the connection creds
sudo /home/ubuntu/.local/bin/aws s3 cp s3://mass.gov-analytics/dashboards/bradford/query_creds/db_connect.R ~/bradford/dashboard/get_data/

# run the query
Rscript ~/bradford/dashboard/get_data/query.R

# create the bradford app dir
sudo mkdir /srv/shiny-server/bradford

# move the dashboard to the shiny-server directory
sudo cp -r ~/bradford/dashboard/* /srv/shiny-server/bradford

#switch to port 80
sudo sed -i '/listen 3838;/c\  listen 80;' /etc/shiny-server/shiny-server.conf

sudo systemctl restart shiny-server

# hack per https://github.com/ropensci/plotly/issues/494 to get shiny + plotly to play nicely
sudo chown -R shiny:shiny /srv/shiny-server
