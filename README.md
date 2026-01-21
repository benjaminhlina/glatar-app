
<!-- badges: start -->
[![Docker Build Status](https://github.com/benjaminhlina/glatar-app/actions/workflows/build_docker.yaml/badge.svg)](
https://github.com/benjaminhlina/glatar-app/actions/workflows/build_docker.yaml
)
[![Deploy to AWS](https://github.com/benjaminhlina/glatar-app/actions/workflows/deploy_compose_aws.yaml/badge.svg)](
    https://github.com/benjaminhlina/glatar-app/actions/workflows/deploy_compose_aws.yaml
)
[![Deployed App Status](https://github.com/benjaminhlina/glatar-app/actions/workflows/check_shiny_status.yaml/badge.svg)](
    https://github.com/benjaminhlina/glatar-app/actions/workflows/check_shiny_status.yaml
)

<!-- badges: end -->

GLATAR-App 

This is a Shiny App that interfaces with the GLATAR dataabase to provide a 
seamless and rich interface with the GLATAR database. 

This app has the the ability to upload (i.e., 
if you are a contributing member), map, view raw data (i.e., 
if you are a contributing member), view summarised data, 
and view scatter plots of the raw data. 

Many of the functions created in this project have been moved to package
`{glatar}` which will eventually be loaded and a lot of the sturucture of 
this project will fall into being documented in that package. The 
GitHub repo can be found [here](https://github.com/benjaminhlina/glatar) 
with the pkgdown webpage being found 
[here](https://benjaminhlina.github.io/glatar/). 
