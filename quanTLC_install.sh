#install script for OC_manager2
#!/bin/bash
echo""
echo "***********************************************************"
echo ""
echo "You are going to install quanTLC on your Raspberry Pi."
echo ""
echo "Don't worry, this will take some time!"
echo ""
echo "Are you ready to start? (y/n)"
read userinput
if [ "$userinput" == "y" ]
then
  echo "Installing r-base"
  yes | sudo apt-get install r-base
  echo ""
  echo "Installing libraries"
  yes | sudo apt-get install libssl-dev libcurl4-openssl-dev r-cran-rgl libtiff5-dev 
  yes | sudo apt-get install libssh2-1-dev libxml2-dev libgit2-dev libnlopt-dev
  echo ""
  echo "Installing R packages"
  echo ""
  yes | sudo su - -c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
  yes | sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
  yes | sudo su - -c "R -e \"library(devtools)\""
  yes | sudo su - -c "R -e \"devtools::install_github('dimitrif/quanTLC')\""
  yes | sudo su - -c "R -e \"devtools::install_github('jrowen/rhandsontable')\""
  echo "Performing reboot"
  sudo reboot
else
  echo "The installation was skipped"
fi
