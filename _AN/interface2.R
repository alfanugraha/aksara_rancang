###*header####
header <- dashboardHeader(
  title="redcluwe.id", 
  titleWidth = "300px",
  dropdownMenu(type = "tasks",
    icon = icon("info fa-1g"),
    badgeStatus = NULL,
    headerText = "Informasi",
    notificationItem(
      text = actionButton('quickTour', label=" Panduan interaktif",
      icon = icon("hand-o-right")),
      icon = icon(""), status = "primary"
    ) #,
    # notificationItem(
    #   text = actionButton('redcluweInfo', label=" Tentang redcluwe.id",
    #   icon = icon("heart")),
    #   icon = icon(""), status = "primary"
    # )
  )
)

###*sidebar####
sidebar <- dashboardSidebar(width = "300px", collapsed = FALSE,
  sidebarMenu(
    id = "tabs",
    menuItem("Home", icon = icon("home"), tabName = "home"),
    ###sidebar-setting####
    menuItem("Pengaturan", icon = icon("check-circle"), id="pengaturan",
             selectInput("categoryProvince", label = "Pilih Provinsi:", 
               list(`Regional Barat` = list("Aceh" = "Aceh", "Bangka Belitung"="BaBel", "Bengkulu"="Bengkulu", "Jambi"="Jambi", "Kepulauan Riau"="KepRi",
                                   "Lampung"="Lampung", "Riau"="Riau", "Sumatera Barat"="SumBar", "Sumatera Selatan"="SumSel", "Sumatera Utara"="SumUt"),
                `Regional Tengah` = list("Bali"="Bali","Banten"="Banten", "DKI Jakarta"="DKIJakarta", "D.I. Yogyakarta"="DIY", "Jawa Barat"="JaBar",
                                "Jawa Tengah"="JaTeng", "Jawa Timur"="JaTim", "Kalimantan Barat"="KalBar",
                                "Kalimantan Selatan"="KalSel", "Kalimantan Tengah"="KalTeng", "Kalimantan Utara"="KalTara", "Kalimantan Timur"="KalTim", 
                                "Nusa Tenggara Barat"="NTB", "Nusa Tenggara Timur"="NTT"),
                `Regional Timur` = list("Gorontalo"="Gorontalo", "Maluku"="Maluku", "Maluku Utara"="Maluku_Utara",
                               "Papua"="Papua", "Papua Barat"="Papua_Barat", "Sulawesi Selatan"="Sulawesi_Selatan", "Sulawesi Tengah"="Sulawesi_Tengah",
                               "Sulawesi Tenggara"="Sulawesi_Tenggara", "Sulawesi Barat"="Sulawesi_Barat", "Sulawesi Utara"="Sulawesi_Utara"))
             ),
             textInput("username", label = "Nama Pengguna", placeholder = "Masukkan nama pengguna tanpa spasi"),
             passwordInput("password", label = "Password", placeholder= "Masukkan password"),
             actionButton("inputLogin", label = "Masuk")
    ),
    ###sidebar-historis####
    menuItem("Historis", icon = icon("history"), id="historis",
              menuSubItem("Data historis", tabName = "pageOne"),
              # fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("emissionFactorEnergiTable", "Faktor Emisi Energi", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("wasteTable", "Tabel Produk Limbah per Sektor", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("emissionFactorWasteTable", "Faktor Emisi Limbah", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("landDemandTable", "Tabel Permintaan Lahan", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("landDistTable", "Tabel Distribusi Lahan", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # numericInput("popDensTable", "Populasi Penduduk (Jiwa)", min=0, value=1000000),
             
              menuSubItem("Hasil analisis", tabName = "pageTwo"),
              selectInput("categorySector", label="Kategori",
                choices=c("Ekonomi", "Energi", "Limbah", "Lahan")
              ),
              conditionalPanel(
                condition="input.categorySector=='Ekonomi'",
                selectInput("pprkResults", label="Pilih output yang ingin ditampilkan",
                  choices=c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output", 
                            "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"
                          )
                )
              ),
              conditionalPanel(
                condition="input.categorySector=='Energi'",
                selectInput("pprkEnergy", label="Pilih output yang ingin ditampilkan",
                  choices=c("Angka Pengganda Energi", "Koefisien Intensitas Energi", "Emisi dari Penggunaan Energi")
                )
              ),
              conditionalPanel(
                condition="input.categorySector=='Limbah'",
                selectInput("pprkWaste", label="Pilih output yang ingin ditampilkan",
                  choices=c("Angka Pengganda Buangan Limbah", "Koefisien Produk Limbah", "Emisi dari Limbah")
                )
              ),
              conditionalPanel(
                condition="input.categorySector=='Lahan'",
                selectInput("pprkLand", label="Pilih output yang ingin ditampilkan",
                  choices=c("Matriks Distribusi Lahan", "Koefisien Kebutuhan Lahan", "Koefisien Produktivitas Lahan", "Permintaan Lahan")
                )
              ),
              tags$div(style="padding: 12px 15px 5px 15px;", downloadButton('downloadReport', 'Unduh Ringkasan', style="color: #fff; background-color: #00a65a; border-color: #008d4c;"))
    ),
    ###sidebar-bau####
    menuItem("Skenario Bisnis Seperti Biasa", icon = icon("exchange"), id="bau",
                menuSubItem("Pilih tipe proyeksi", tabName = "pageEleven"),
                selectInput('selectProjType', label=" ", 
                         choices=c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan")),
                tags$div(id='inputProjType')
             
              # menuSubItem("Input", tabName = "pageFour"),
              # selectInput("typeIntervention", "Tipe Intervensi", choices = c("Tipe 1", "Tipe 2")),
              # selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2015),
              # selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2030), 
              # fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
              # fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
              # actionButton("generateBAUTable", "Buat Tabel"),
              # menuSubItem("Hasil analisis", tabName = "pageFive"),
              # selectInput("bauResults",
              #           label="Pilih output yang ingin ditampilkan",
              #           choices=c("Proyeksi PDRB", 
              #                     "Proyeksi Upah per Kapita",
              #                     "Proyeksi Upah Gaji",
              #                     "Proyeksi Tenaga Kerja",
              #                     "Proyeksi Konsumsi Energi",
              #                     "Proyeksi Emisi Terkait Konsumsi Energi",
              #                     "Proyeksi Buangan Limbah",
              #                     "Proyeksi Emisi Terkait Buangan Limbah",
              #                     "Proyeksi Total Emisi",
              #                     "Proyeksi Intensitas Emisi"
              #                     )
              #           )
    ),
    ###sidebar-intervention####
    menuItem("Skenario Aksi", icon = icon("random"), id="intervensi",
              menuSubItem("Input", tabName = "pageSeven"),
              selectInput("interTableOutput",
                        label="Pilih tipe intervensi:",
                        choices=c("Permintaan Akhir"
                                  # "Tabel Satelit Sektor Energi",
                                  # "Tabel Satelit Sektor Limbah"
                                  )
                        ),
              textInput("scenarioName", "Nama skenario aksi:", value=""),
              selectInput("yearInter", "Tahun skenario aksi:", choices = 1990:2100, selected=2015),
              uiOutput("selectizeSector"),
              menuSubItem("Hasil analisis", tabName = "pageEight")
    ),
    menuItem("Tentang redcluwe.id", icon = icon("question-circle"), tabName="help")
  )
)

###*body####
body <- dashboardBody(
  ###*tab-home####
  tabItems(
    tabItem(tabName = "home",
        jumbotron(img(src="homepage.jpg", width="100%"), " ", button = FALSE)
    ),
    ###*tab-historis####
    tabItem(tabName = "pageOne",
      fluidRow(
        h3(style="padding-left: 15px;", textOutput("yearIO")),
        
        tabBox(id="tabPanelHistori", width = 12, 
          tabPanel("Table Input-Output", id="boxIO",
            div(style="overflow-x: scroll", dataTableOutput('tableIO'))
          ),
          tabPanel("Tabel Tenaga Kerja", id="boxLabour", 
            div(style="overflow-x: scroll", dataTableOutput('SatelitTenagaKerja'))
          ),
          tabPanel("Tabel Satelit Energi", id="boxEnergy",
            div(style="overflow-x: scroll", dataTableOutput('SatelitEnergi'))
          ),
          tabPanel("Tabel Satelit Limbah", id="boxWaste",
            div(style="overflow-x: scroll", dataTableOutput('SatelitLimbah'))
          ),
          tabPanel("Tabel Satelit Lahan", id="boxLand",
            div(style="overflow-x: scroll", dataTableOutput('SatelitLahan'))
          ),
          tabPanel("Tutupan Lahan", id="boxLC",
            div(style="overflow-x: scroll", dataTableOutput('TutupanLahan'))
          )
        )
      )
    ),
    tabItem(tabName = "pageTwo",
      fluidRow(
        column(width=12,
          tags$div(id='placeholder'),
          hr()
        )
      ),
      hr(),
      fluidRow(
        column(width=12,
          box(width=NULL,
            h3(textOutput("tableDesc")),
            hr(),
            div(style="overflow-x: scroll", dataTableOutput('tableResults')),
            downloadButton('downloadTable', 'Download Table (.csv)')
          )
        )
      ),
      conditionalPanel(
        condition="input.pprkResults=='Perbandingan Angka Pengganda'",
        uiOutput("sectorSelection")
      ),
      conditionalPanel(
        condition="input.pprkResults!='Pendapatan per kapita'",
        plotlyOutput("plotlyResults")
      )
    ),
    ###*tab-bau####
    tabItem(tabName="pageEleven", 
      fluidRow(
        h3(style="padding-left: 15px;", "Pilih Data Input BAU"), 
        tabBox(id="tabPanelBAUData", width=12,
                     tabPanel("Tabel LDM Proporsi", id="TabPanelBAUDataLDMProp",
                              div(style="overflow-x: scroll", 
                                  uiOutput("LDMFileOptions"),
                                  dataTableOutput("LDMListTable"),
                                  dataTableOutput("LDMTampil"),
                                  uiOutput('LDMTableTampilUI'),
                                  uiOutput('modalLDMUI'))
                                  )
        )
      )
    ),
    tabItem(tabName = "pageFour",
      conditionalPanel(
        condition = "input.typeIntervention=='Tipe 1'",
        h3("Tipe 1: Pertumbuhan ekonomi sama untuk setiap lapangan usaha, namun berbeda setiap tahun"),
        selectInput("yearBAUInv", "Pilih tahun:", choices = 2010:2030, selected=2015),
        sliderInput("gdpRate", "Laju pertumbuhan ekonomi:", min=0, max=10, post="%", value=5, step=0.01, width = "600px")
      ),
      conditionalPanel(
        condition = "input.typeIntervention=='Tipe 2'",
        h3("Tipe 2: Pertumbuhan ekonomi berbeda baik untuk setiap lapangan usaha, maupun setiap tahun proyeksi")
      ),
      actionButton("saveTableBauType", "Simpan Tabel"),
      actionButton("buttonBAU", "Jalankan Simulasi"),
      hr(),
      rHandsontableOutput('tableBAUType')
    ),
    tabItem(tabName = "pageFive",
        fluidRow(
          column(width=12,
            tags$div(id='bauplaceholder'),
            hr()
          )
        ),
        conditionalPanel(
          condition="input.bauResults!='Proyeksi Upah per Kapita' & input.bauResults!='Proyeksi Total Emisi'",
          uiOutput("yearSelection")
        ),
        plotlyOutput("plotlyResultsBAU"),
        hr(),
        fluidRow(
          column(width=12,
            box(width=NULL,
              dataTableOutput('tableResultsBAU'),
              downloadButton('downloadTableBAU', 'Download Table (.csv)')
            )
          )
        )
    ),
    tabItem(tabName = "pageTen",
            fluidRow(
              column(width=12,
                     tags$div(id='bauplaceholder'),
                     hr()
              )
            ),
            conditionalPanel(
              condition="input.lahanResults!='Proyeksi Neraca Perdagangan'",
              uiOutput("yearSelection2")
            ),
            plotlyOutput("plotlyResultsBAU_lahan"),
            hr(),
            fluidRow(
              column(width=12,
                     box(width=NULL,
                         dataTableOutput('tableResultsBAU_lahan'),
                         downloadButton('downloadTableBAU_lahan', 'Download Table (.csv)')
                     )
              )
            )
    ),
    tabItem(tabName = "pageNine",
            # actionButton("saveTableLDMProp", "Simpan Tabel"),
            # hr(),
            # rHandsontableOutput('tableLDMProp'),
            # hr(),
            # actionButton("buttonBAULahan", "Jalankan Simulasi")
            actionButton("saveInputBAULahanLandCover", "Simpan Tabel"),
            actionButton("buttonBAULahan", "Jalankan Simulasi"),
            hr(), 
            rHandsontableOutput("inputBAULahanLandCover"), 
            hr()
    ),
    ###*tab-intervention####
    tabItem(tabName = "pageSeven",
            h2("Perubahan permintaan akhir dari lapangan usaha terkait"),
            # render multiple num and slider
            uiOutput("rowIntervention"),
            hr(),
            actionButton("buttonInter", "Jalankan Simulasi")
    ),
    tabItem(tabName = "pageEight",
            fluidRow(
              valueBoxOutput(width=6, "percentOfEmRed"),
              valueBoxOutput(width=6, "percentOfGDPGrowth")
            ),
            hr(),
            plotlyOutput("curveEmRed"),
            plotlyOutput("curveGDPGrowth"),
            plotlyOutput("curveIntensityEmission"),
            hr(),
            downloadButton('downloadResults', 'Unduh Hasil Analisis', style="color: #fff; background-color: #00a65a; border-color: #008d4c"),
            hr(),
            selectInput("interResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("Proyeksi PDRB", 
                                  "Proyeksi Upah per Kapita",
                                  "Proyeksi Upah Gaji",
                                  "Proyeksi Tenaga Kerja",
                                  "Proyeksi Konsumsi Energi",
                                  "Proyeksi Emisi Terkait Konsumsi Energi",
                                  "Proyeksi Buangan Limbah",
                                  "Proyeksi Emisi Terkait Buangan Limbah",
                                  "Proyeksi Total Emisi"
                                  )
                        ),
            conditionalPanel(
              condition="input.interResults!='Proyeksi Upah per Kapita' & input.interResults!='Proyeksi Total Emisi'",
              uiOutput("yearSelectionInter")
            ),
            hr(),
            fluidRow(
              column(width=12,
                tags$div(id='interplaceholder'),
                hr()
              ),
              column(width=12,
                box(width=NULL,
                  dataTableOutput('tableResultsInter'),
                  downloadButton('downloadTableInter', 'Download Table (.csv)')
                )
              )
            ),
            plotlyOutput("plotlyResultsInter")
    ),
    tabItem(tabName = "help",
      h2("Perencanaan Pembangunan Rendah Karbon"),
      tags$div(class = "header", checked = NA,
        tags$p("Pertumbuhan ekonomi selama ini cenderung diikuti dengan penurunan kualitas lingkungan.
                Korelasi negatif ini menjadi tantangan bersama dalam pembangunan berkelanjutan.
                Penerapan Pembangunan Rendah Karbon menjadi perspektif baru dalam mendorong
                perencanaan pembangunan yang lebih baik."),
        tags$p("Pembangunan Rendah Karbon bertujuan Menurunkan emisi Gas Rumah Kaca serta
                mengintegrasikan pertumbuhan ekonomi, pengentasan kemiskinan dan stabilitas sosial.
                Pembangunan Rendah Karbon juga merupakan tindak lanjut dari program penurunan emisi
                Indonesia yang dituangkan dalam Rencana Aksi Nasional maupun Rencana Aksi Daerah
                Gas Rumah Kaca (RAN/RAD GRK)."),
        h3("redcluwe.id"),
        tags$p("Reducing Carbon Intensity of Landuse, Waste and Energy (redcluwe.id) adalah alat bantu proses transformasi Rencana Aksi Daerah - Gas Rumah Kaca (RAD-GRK) menjadi Perencanaan Pembangunan Rendah Karbon (PPRK)."),
        tags$p("Untuk menjalankan panduan interaktif menggunakan redcluwe.id, silahkan klik menu di kanan atas aplikasi ini.")
      ), 
      dataTableOutput('tableUserLog')
    )
  ),
  useShinyjs()
)

shinyUI(
  tagList(
    introjsUI(),
    dashboardPage(
      skin = 'green', 
      header,
      sidebar,
      body
    )
  )
)