# Saklehir's ShinyApps

*A collection of apps for automatizing time-consuming statistical analyses.*

This is a collection of interactive tools, built with shiny. 


---

*You can run the apps by either downloading the app.R file on your devide and by without downloading the app.R file.*

## Run on your device with installation ##

Download or clone the complete repository and save it on your local device. After that, you can simply run the apps with the function `runApp()`. 

## Run locally without installation ##

You can run the apps locally by downloading them temporarily from Github (i.e., you only need an internet connection for the initial download, then the app runs locally. However, when closing R the download is lost and not available locally any more).
When running the apps with this procedure, several packages will be installed on your device as they are required for the app to run. 

When these preconditions are met, you can run an app with a single line of code. The specific app is specified in the `subdir` parameter. Example:
    
    runGitHub("ShinyApps", user="Saklehir", subdir="Simple Slope Analysis Made Easy")


----
All of my apps are **free software**, released under the GNU General Public License v3.0, "which guarantees end users (individuals, organizations, companies) the freedoms to use, study, share (copy), and modify the software" (see [Wikipedia](http://en.wikipedia.org/wiki/GNU_General_Public_License)).
