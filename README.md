# Idaho Public Health District 3 COVID-19 Dashboard

This Shiny App is designed to be used by the public and professionals in Idaho's Public Health District 3 to inform them about the Syphilis outbreak in Southwest Idaho that was declared in 2021.

## Getting Started

If you'd like to test this app yourself, all that is strictly necessary for the app to run is the `data`, `new_data`, and `old_data` directories, and the `app.R` file, as well as R, RStudio, and the packages listed at the top of the `app.R`. Once you download these files, open the `app.R` file. Then ctrl+shift+enter or click the "Run App" button at the top of Rstudio. This will open a separate RStudio window. Sometimes the app will open within that window, but most of the time you will have to click "Open in Browser" and it will open the app in your browser. You can make the app open in your browser by default by clicking the drop-down arrow next to `Run App` in RStudio and selecting `Run External`. The other three .R files are used by the developers to read-in and format the data, and automatically update the app, and thus will not work for other users. They are provided here to maintain as much transparency as possible.

### Prerequisites

R, RStudio, and the packages listed at the top of `app.R`. There is not currently a `renv.lock` file to make the package versions consistent, but there are plans to do so in the future.

## Updating the App

The following is for instructions for users at Southwest District Health. The app relies on a single data set: `syphilis_data.csv`, located in the `processed_data` folder.

### Creating datasets

First, if you haven't already done so for the syphilis internal report, you will need to pull the data from NBS and place this file into the raw_data folder and name it `syphilis_cases.csv`.

Then, open `raw_data.RProj` in the `raw_data` folder. Then, within R, open the `clean_syphilis_case_data.R` and run it. This will create the `syphilis_data.csv` and place it in the `processed_data` folder.

Next, close the raw_data project and open the `syphilis_dashboard.RProj`, wherever you had placed it.

Then, open the `create_data.R` file and run it. This will do a few things. It will create several data sets in the `data` folder. It will also check to see if the data you created in the `processed_data` folder is the same as the data currently located in the `new_data` folder. If it is different (and it should be), it will delete the data that is in the `old_data` folder, move the data in the `new_data` to `old_data`, and make a copy of the data that is in `processed_data` into the `new_data` folder. It also creates a file that grabs the current date and time.

### Testing the app locally.

Open the `app.R` file. Then, check to make sure it works by using ctrl+shift+enter or by clicking "run app" button at the top of Rstudio. Click through the different tabs, change the counties and so on and make sure the plots respond appropriately.

### Posting the app.

Close all r scripts in R studio. Then, within the RStudio terminal, type `source('update_app.R')`. This does three things: First, as much as possible, it styles the `app.R` file to conform to the tidyverse style guide. Don't rely on this and try to write the code using this [style guide](https://style.tidyverse.org/). Second, it takes the current date and time on your machine and edits the `app.R` file so that the app will display when it was last updated. Third, it deploys the app to the shinyapps server.

Go to (<https://swdh.shinyapps.io/syphilis_dashboard/>) and make sure that the app is working like it did when you checked it locally.

### Commit the changes to git and Github.

To do this, click on `Terminal` at the bottom of RStudio. Type `git add .` This stages all the files in the repository. To make sure it worked, type `git status`. This should tell you which files were changed. Then, type `git commit -m 'INSERT MESSAGE HERE'`. Make sure the message is at least somewhat informative of what you did. You can just say something like 'Updated NBS data up to March 31st 2023.' This commits the changes to the local repository, but now you need to push the changes to github. Do this by typing `git push -u origin master`. You can check whether it worked by going to the repository on github. At the very least, the `app.R` file should have the message you typed in for the commit next to it, along with the date.

## Authors

Austin Gallyer, Andy Nutting, Lekshmi Venugopal, Cate Lewis.

## License

This project is licensed under the GNU General Public License v3.0 - see COPYING.txt for details.
