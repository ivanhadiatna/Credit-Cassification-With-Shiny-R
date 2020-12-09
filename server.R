
model <-readRDS("Riskrating_Model.rds")


#--------------------------------------------------------------------------------
# LOGIN PAGE
#--------------------------------------------------------------------------------
LOGIN <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "creditsc_credit_scoring",
    host = "localhost",
    username = "creditsc_ivanh",
    password = "SqlAdmin")
  on.exit(dbDisconnect(conn, add = TRUE))
  dbGetQuery(conn, "SELECT * FROM user;")
}

A <- LOGIN()
credentials = data.frame(
  username = A[,1],
  passod   = sapply(A[,2],password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F)


# Main login screen
loginpage <- fluidPage(
  div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      bs4Box(width = "50%",
             tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
             textInput("userName", width = "100%", placeholder="Username", label = tagList(icon("user"), "Username")),
             passwordInput("passwd", width = "100%", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
             br(),
             div(
               style = "text-align: center;",
               actionButton("login", "MASUK", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
               shinyjs::hidden(
                 div(id = "nomatch",
                     tags$p("Masukan Username dan Password Dengan Benar!",
                            style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                            class = "text-center")))))))







#--------------------------------------------------------------------------------
# TAB PREDIKSI
#--------------------------------------------------------------------------------
boxdownload <- bs4Card(
  title = "Save To Excel",
  status = "primary",
  solidHeader = FALSE,
  collapsible = TRUE,
  collapsed = TRUE,
  closable = FALSE,
  labelStatus = "primary",
  width = 12,
  fluidRow(
    column(width = 6,dateInput("dates1", label = h5("Dari"), width = "100%")),
    column(width = 6,dateInput("dates2", label = h5("Sampai"), width = "100%")),
    column(width = 6,downloadButton("download"))
  )
)
box.upload <- box(
  gradientColor = "primary",
  collapsible = FALSE, 
  collapsed = FALSE,
  closable = FALSE,
  status = "primary",
  width = 12,
  fluidRow(
    # Input: Select a file ----
    fileInput("file1", "Pilih File Xlsx ", width = "100%",
              multiple = FALSE,
              accept = c(".xlsx")
    )),
  fluidRow(
    column(width = 3,
           actionButton("prediksi2", "Proses", width = 112, icon = icon("wrench")),
           tags$br(),tags$br(),
           downloadButton("downloadData", "Download")),
    column(
      width = 2,
      # Input: Select number of rows to display ----
      radioButtons("disp", "Tampilkan Data",
                   choices = c(Sebagian = "head",
                               Semua = "all"),
                   selected = "head"))))
tabel01 <- function(id, nama){
  bs4Card(
    title = nama,
    status = "primary",
    solidHeader = FALSE,
    collapsible = TRUE,
    collapsed = FALSE,
    closable = FALSE,
    labelStatus = "primary",
    width = 12,
    fluidRow(
      column(width = 12, tableOutput(id)))
  )
}


form <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(width = 3,
           shinyjs::disabled(textInput("id", label = h6("Id"), width = "100%", value = "")),
           textInput("nama", label = h6("Nama"), width = "100%", value = "")),
    column(width = 3,
           numericInput("age", label = h6("Age"), width = "100%", value = 0, min = 0),
           numericInput("dependents", label = h6("Dependents"), width = "100%", value = 0, min = 0)),
    
    column(width = 3,
           numericInput("income", label = h6("Income"), width = "100%", value = 0, min = 0),
           numericInput("tenor", label = h6("Tenor"), width = "100%", value = 0, min = 0)),
    column(width = 3,
           numericInput("debt", label = h6("Debt"), width = "100%", value = 0, min = 0),
           numericInput("empyear", label = h6("Empyear"), width = "100%", value = 0, min = 0)),
    column(width = 3,
           numericInput("asset", label = h6("Asset"), width = "100%", value = 0, min = 0)),
    column(width = 3,
           shinyjs::disabled(textInput("riskrating", label = h6("Riskrating"), width = "100%", value = "-")))
  ),fluidRow(    
    column(
      width = 6,
      actionButton("submit", "Submit"),
      shinyjs::disabled(actionButton("edit", "Edit")),
      actionButton("new", "Reset"),
      actionButton("delete", "Delete"),
      actionButton("prediksi", "Proses")),
    column(width = 6,boxdownload),

    column(width = 12,
           tags$hr(style = "border-top: 100% solid #000000;"),
           DT::dataTableOutput("responses", width = "100%", height =  "auto"))))


#--------------------------------------------------------------------------------
#TAB INSTRUKSI
#--------------------------------------------------------------------------------
help01 <- bs4Callout(
  title = "CARA MEMPREDIKSI",
  width = 12,
  elevation = 1,
  status = "info",
  HTML('<p align="justify">Untuk melakukan prediksi risiko kredit pada aplikasi ini 
  terdapat 2 cara yaitu input data dan upload data. Pada input data, pengguna harus 
  memasukan satu persatu data nasabah yang dibutuhkan untuk melakukan prediksi setelah
  itu dapat disimpan ke database.Sedangkan pada upload data diperuntukan untuk jumlah
       data yang lebih besar dan disimpan pada file excel</p>')
)

help02 <- bs4Callout(
  title = "INPUT DATA",
  width = 12,
  elevation = 1,
  status = "info",
  HTML('<p align="justify">Untuk melakukan prediksi dengan cara input data cukup masukan data nasabah pada masing-masing textinput.
  Kemudian klik tombol prediksi, makan akan hasil prediksi nilai risiko kredit nasabah akan ditampilkan pada textinput riskrating. 
  Kemudian data dapat disimpan ke database dengan klik tombol"Submit"<p>
         <p align="center"><img src="help01_1.PNG"  width="70%" height="auto" ></p><br>
         <p align="justify">Data yang telah tersimpan akan ditampilkan pada tabel output.<p>
         <p align="center"><img src="help01_2.png"  width="70%" height=auto"></p>
         <p align="justify">Untuk merubah data, cukup pilih data yang ingin dirubah, maka data yang akan dirubah akan ditampikan di textinput.
         Setelah itu rubah data pada textinput tersebut dan klik tombol "Edit". Dan Untuk menghapus data, cukup pilih data yang akan dihapus kemudian klik tombol "Delete"<p>'))

help03 <- bs4Callout(
  title = "UPLOAD Data",
  width = 12,
  elevation = 1,
  status = "info",
  HTML('<p align="justify">Untuk melakukan klasifikasi dengan cara upload data yaitu klik tombol "Browse...", 
         kemudian pilih file excel yang akan diklasifikasi, dengan catatan terdapat variabel prediktor pada kolom excel yang sama seperti pada inputan "Input Data".
         Untuk menampilkan data secara menyeluruh pilih "Semua" pada display dan untuk menapilkan 5 data teratas pilih "Sebagian"</p>
         <p align="center"><img src="help02_1.png"  width="70%" height="auto"></p>
         <p align="justify">Setelah file diupload, maka akan ditampilkan pada box "Data klasifikasi".
         Untuk memklasifikasi tekan tombol "Proses" maka hasil klasifikasi akan ditampilkan pada box "Hasil klasifikasi"</p>
         <p align="center"><img src="help02_2.PNG"  width="70%" height="auto"></p>'))


about01 <-  bs4Callout(
  title ="ABOUT",
  width = 12,
  elevation = 1,
  status = "info",
  fluidRow(
    HTML('<p align="justify">Credit score adalah nilai resiko yang diberikan kepada seorang 
             individu atau organisasi yang mengajukan pinjaman berdasarkan rekam jejak pinjaman 
             dan pembayaran yang dilakukan.</p>
             
             <p align="justify">Credit risk adalah resiko yang harus ditanggung oleh seorang individu atau lembaga 
             ketika memberikan pinjaman - biasanya dalam bentuk uang - ke individu atau pihak lain.</p>
            <p align="justify">Resiko ini berupa tidak bisa dibayarkannya pokok dan bunga pinjaman, sehingga mengakibatkan 
            kerugian seperti berikut:</p>
            <ul>
                <li>Gangguan aliran kas (cash flow) sehingga modal kerja terganggu. </li>
                <li>Meningkatkan biaya operasional untuk mengejar pembayaran tersebut (collection).</li>
            </ul>
             
             <p align="justify"> Untuk memperkecil resiko kredit ini, biasanya dilakukan proses yang disebut dengan credit scoring 
             dan credit rating terhadap pihak peminjam.</p>
             
             <p align="justify">Aplikasi ini diperuntukan untuk membantu memberikan penilaian credit (Credit Scoring), 
             apakah peminjam tergolong yang memiliki resiko yang aman atau sangat beresiko </p>')))

#--------------------------------------------------------------------------------
#FUNGSI UPDATE INPUT DAN CLEAR
#--------------------------------------------------------------------------------
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(data["id"]))
  updateTextInput(session, "nama", value = unname(data["nama"]))
  updateTextInput(session, "age", value = unname(data["age"]))
  updateTextInput(session, "dependents", value = unname(data["dependents"]))
  updateTextInput(session, "income", value = unname(data["income"]))
  updateTextInput(session, "empyear", value = unname(data["empyear"]))
  updateTextInput(session, "debt", value = unname(data["debt"]))
  updateTextInput(session, "tenor", value = unname(data["tenor"]))
  updateTextInput(session, "asset", value = unname(data["asset"]))
  updateTextInput(session, "riskrating", value = unname(data["riskrating"]))
}

Clear <- function(session) {
  updateTextInput(session, "nama", value = "")
  updateTextInput(session, "age", value = 0)
  updateTextInput(session, "dependents", value = 0)
  updateTextInput(session, "income", value = 0)
  updateTextInput(session, "empyear", value = 0)
  updateTextInput(session, "debt", value = 0)
  updateTextInput(session, "tenor", value = 0)
  updateTextInput(session, "asset", value = 0)
  updateTextInput(session, "riskrating", value = "-")
}


shinyServer(function(input, output,session) {

  
    #--------------------------------------------------------------------------------
    # FUNGSI CEK ID DAN READ DATA BAGIAN INPUT
    #--------------------------------------------------------------------------------
    cekId <- function(){
        conn <- dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "creditsc_credit_scoring",
            host = "localhost",
            username = "creditsc_ivanh",
            password = "SqlAdmin")
        on.exit(dbDisconnect(conn), add = TRUE)
        count = dbGetQuery(conn, paste0("SELECT COUNT(id) FROM calon_debitur;"))
        if( count > 0 ){
            no = as.integer(count)
            no = no + 1
            id = paste("CNS-", no, sep = "" )
            updateTextInput(session, "id", value = id)
        } else {
            updateTextInput(session, "id", value = "CNS-1")
        }}
    

    ReadData <- function(){
        conn <- dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "creditsc_credit_scoring",
            host = "localhost",
            username = "creditsc_ivanh",
            password = "SqlAdmin")
        on.exit(dbDisconnect(conn), add = TRUE)
        dbGetQuery(conn, paste0("SELECT id, nama, age, dependents, income, empyear, debt, tenor,asset, riskrating FROM calon_debitur;")) 
    }
    
    #-----------------------------------------------------------------------
    #FUNGSI UPLOAD FILE
    #-----------------------------------------------------------------------
    read_data <- reactive({
        inFile <- input$file1
        if(is.null(inFile))
            return(NULL)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".xlsx", sep=""))
        read.xlsx(paste(inFile$datapath, ".xlsx", sep=""), 1)})
    
    output$contents <- renderTable(
        if(input$disp == "head") {
            return(head(read_data()))
        }else {
            return(read_data())
        })

    
    #--------------------------------------------------------------------------------
    #  OUPUT UI
    #--------------------------------------------------------------------------------
    
    login = FALSE
    USER <- reactiveValues(login = login)
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
   
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold;")
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            bs4SidebarMenu(id="sidebarmenu",
                           bs4SidebarMenuItem("Klasifikasi", tabName = "prediksi",icon = "network-wired"),
                           bs4SidebarMenuItem("Bantuan", tabName = "help",icon = "book-open"),
                           bs4SidebarMenuItem("Tentang", tabName = "about",icon = "info")
                           
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            bs4TabItems(
                bs4TabItem(
                    tabName = "prediksi",
                    tabBox(id = "tabBox",
                           status = "primary",
                           solidHeader =  TRUE,
                           closable = FALSE,
                           width = "100%",
                           tabPanel(tabName = "Input Data",form),
                           tabPanel(tabName = "Upload Data", 
                                    fluidRow(
                                        box.upload,
                                        tabel01("contents", "Data"),
                                        tabel01("hasil2", "Hasil"))))),
                bs4TabItem(
                    tabName = "help",
                    fluidRow(
                        help01,
                        help02,
                        help03)),
                bs4TabItem(
                    tabName = "about",
                    fluidRow(
                        about01)))
            }else{
            loginpage
                }
        })
    
    #--------------------------------------------------------------------------------
    # FUNGSI PROSES
    #--------------------------------------------------------------------------------
    
    observe({
        if(USER$login == TRUE){
            cekId()
        }})
    
    InputData <- function(){
        id = input$id
        nama = input$nama
        age = input$age
        dependents =input$dependents
        income =input$income
        empyear =input$empyear
        debt = input$debt
        tenor = input$tenor
        asset = input$asset
        riskrating = input$riskrating
        conn <- dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "creditsc_credit_scoring",
            host = "localhost",
            username = "creditsc_ivanh",
            password = "SqlAdmin")
        on.exit(dbDisconnect(conn), add = TRUE)
        QInsert <- paste0("INSERT INTO calon_debitur (id, nama, age, dependents, income, empyear, debt, tenor,asset, riskrating) VALUES('",id,"','",nama,"',",age,",",dependents,",",
                          income,",",empyear,",",debt,",",tenor,",'",riskrating,"');")
        dbSendQuery(conn, QInsert)}
    
    EditData <- function(){
        id = input$id
        nama = input$nama
        age = input$age
        dependents =input$dependents
        income =input$income
        empyear =input$empyear
        debt = input$debt
        tenor = input$tenor
        asset = input$asset
        riskrating = input$riskrating
        conn <- dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "creditsc_credit_scoring",
            host = "localhost",
            username = "creditsc_ivanh",
            password = "SqlAdmin")
        on.exit(dbDisconnect(conn), add = TRUE)
        QUpdate <- paste0("UPDATE calon_debitur SET nama = '",nama,"',age=  ",age,",dependents=  ",dependents,",income=  ",income,",age=  ",age,",empyear=  ",empyear,",debt=  ",debt,",tenor=  ",tenor,",asset=  ",asset,",riskrating=  '",riskrating, "' WHERE id ='",id,"'")
        dbSendQuery(conn, QUpdate)}
    
    DeleteData <- function(){
        id = input$id
        conn <- dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "creditsc_credit_scoring",
            host = "localhost",
            username = "creditsc_ivanh",
            password = "SqlAdmin")
        on.exit(dbDisconnect(conn), add = TRUE)
        QDelete <- paste0("DELETE From calon_debitur WHERE id = '",id,"'")
        dbSendQuery(conn, QDelete)}
    
    observeEvent(input$new,{
        Clear(session)
        shinyjs::enable(id = "submit")
        shinyjs::disable(id = "edit")
        cekId()})
    
    
    observeEvent(input$delete,{
        DeleteData()
        Clear(session)
        shinyjs::enable(id = "submit")
        shinyjs::disable(id = "edit")
        cekId()})
    
    observeEvent(input$submit,{
        InputData()
        Clear(session)
        cekId()})
    
    observeEvent(input$edit, {
        EditData()
        Clear(session)
        cekId()
        shinyjs::disable(id="edit")
        shinyjs::enable(id="submit")
    })
    
    # Select row in table -> show details in inputs
    observeEvent(input$responses_rows_selected, {
        if (length(input$responses_rows_selected) > 0) {
            data <- ReadData()[input$responses_rows_selected, ]
            UpdateInputs(data, session) 
            shinyjs::enable(id="edit")
            shinyjs::disable(id="submit")
        }})
    
    output$responses <- DT::renderDataTable({
        input$delete
        input$submit
        input$edit
        ReadData()
    }, selection = 'single')
    
    #-----------------------------------------------------------------------
    #PREDIKSI 1
    #-----------------------------------------------------------------------
    
    observeEvent(input$prediksi,{
        inputan <- data.frame(
            income = as.numeric(input$income),
            tenor = as.numeric(input$tenor),
            dependents = as.numeric(input$dependents),
            age = as.numeric(input$age),
            empyear = as.numeric(input$empyear),
            asset = as.numeric(input$asset),
            debt = as.numeric(input$debt))
        
        if(sum(is.na(inputan)) > 0){
            riskrating = ""
            updateTextInput(session, "riskrating", value = riskrating)
        } else{
            riskrating <- predict(model, inputan)
            updateTextInput(session, "riskrating", value = riskrating)
        }
    })
    
    
    file_csv <- function(){
        conn <- dbConnect(
            drv = RMySQL::MySQL(),
            dbname = "creditsc_credit_scoring",
            host = "localhost",
            username = "creditsc_ivanh",
            password = "SqlAdmin")
        tgl_dari = input$dates1
        tgl_sampai =input$dates2
        on.exit(dbDisconnect(conn), add = TRUE)
        QDownload <- paste0("SELECT * FROM calon_debitur WHERE tanggal BETWEEN '",tgl_dari,"%'", " AND '", tgl_sampai,"%';")
        File <- dbFetch(dbSendQuery(conn, QDownload))
    }
    
    output$download <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(file_csv(), con)
        })
    #-----------------------------------------------------------------------
    #PREDIKSI 2
    #-----------------------------------------------------------------------
    predik2 <- eventReactive(input$prediksi2, {
        data <- read_data()
        data$hasil<- predict(model,data)
        data})
    
    
    output$hasil2 <- renderTable({
        predik2()})
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("hasil-", input$file1,sep = "")},
        
        content = function(con) {
            write.xlsx(predik2(), con)}
        )
    
     
    
    
    
})
