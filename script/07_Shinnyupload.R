
#install.packages('rsconnect')
require(rsconnect)
#shinyapp.io
rsconnect::setAccountInfo(name='scoping-review-justicia-reprodutiva',
                          token='9A83326CDF9244501C5BAE2B71B12A7F',
                          secret='R+XptF/4iBRSmGfCOVsNlX/6gUkb1k1MzHmVEvDD')


rsconnect::deployApp("shinny")

# verifying logs and dependencies
rsconnect::showLogs("shinny")
rsconnect::appDependencies("shinny")

# testing app locally
shiny::runApp("shinny")
