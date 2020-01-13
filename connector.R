install.packages("rsconnect")

rsconnect::setAccountInfo(name='danswell',
                          token= Sys.getenv("shiny_token"),
                          secret=Sys.getenv("shiny_secret"))


getwd()
rsconnect::deployApp('C:/Users/dansw/Documents/R git2/shiny_air/App', appName = "Open_data_airquality")
Y
#redeploy
