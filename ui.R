
navbar <- bs4DashNavbar(
    title = "CREDIT SCORING",
    skin = "dark",
    status = "primary",
    rightUi = uiOutput("logoutbtn")
    
)

sidebar <- bs4DashSidebar(
    id = "sidebarset",
    skin = "dark",
    status =  "primary",
    title = "CREDIT SCORING",
    brandColor = "primary",
    src = "icon_sidebar.png",
    elevation = 3,
    opacity = 0.8,
    uiOutput("sidebarpanel")
)

body <- bs4DashBody(shinyjs::useShinyjs(), uiOutput("body"))


shinyUI(bs4DashPage(
    title = "CREDIT SCORING",
    sidebar_collapsed =  FALSE,
    navbar,
    sidebar, 
    body))



