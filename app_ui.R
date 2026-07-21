library(shiny)
library(shinydisconnect)
library(bslib)
library(gridlayout)
library(DT)
library(leaflet) 

app_ui <- page_navbar(
  title = "Fauna Rapid Analysis - Camera Trap Edition",
  selected = "Validation & Process",
  collapsible = TRUE,
  theme = bslib::bs_theme(preset = "minty"),
  
  header = disconnectMessage(
    text = "Koneksi terputus. Pastikan Exiftool terpasang dengan benar di sistem Anda.",
    refresh = "Segarkan Kembali",
    background = "#f8d7da",
    colour = "#721c24",
    overlayColour = "#000",
    overlayOpacity = 0.5
  ),
  
  # ==========================================
  # TAB 1: VALIDATION & PROCESS
  # ==========================================
  nav_panel(
    title = "Validation & Process",
    grid_container(
      layout = c("sidebar main_panel"),
      row_sizes = c("1fr"),
      col_sizes = c("350px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "sidebar",
        card_header("Input Data & Folder"),
        card_body(
          fileInput(inputId = "file_effort", label = "1. Upload Effort CSV (CTTasikBesarSerkap.csv)",
                    multiple = FALSE, accept = c("text/csv", ".csv")),
          textInput(inputId = "path_raw1", 
                    label = "2. Path Folder Kamera Trap 1", 
                    value = "Trial/CTSMTBS"), #change with your directory of CT
          textInput(inputId = "path_raw2", 
                    label = "3. Path Folder Kamera Trap 2 (Opsional)", 
                    value = "Trial/CTSMTBS2"),
          hr(),
          actionButton(inputId = "btn_process", label = "Ekstrak & Proses Metadata", class = "btn-primary w-100"),
          br(), br(),
          downloadButton("download_detection", "Download Hasil Deteksi (.csv)", class = "w-100")
        )
      ),
      grid_card(
        area = "main_panel",
        card_body(
          grid_container(
            layout = c("top_row", "bottom_row"),
            row_sizes = c("1fr", "1fr"), col_sizes = c("1fr"), gap_size = "10px",
            grid_card(area = "top_row", full_screen = TRUE, card_header("Data Effort (Kamera Aktif)"), card_body(DTOutput("table_effort"))),
            grid_card(area = "bottom_row", full_screen = TRUE, card_header("Hasil Ekstraksi Metadata (5 Data Teratas)"), card_body(DTOutput("table_detection_head")))
          )
        )
      )
    )
  ),
  
  # ==========================================
  # TAB 2: SUMMARY & RAI - UPDATE
  # ==========================================
  nav_panel(
    title = "Summary & RAI",
    grid_container(
      # Mengubah layout agar terbagi menjadi 2 baris (atas dan bawah)
      layout = c(
        "table_camop_eff",
        "table_rai"
      ),
      row_sizes = c("0.8fr", "1.2fr"), # Rasio tinggi jendela atas dan bawah
      col_sizes = c("1fr"), 
      gap_size = "10px",
      
      # Jendela Atas: Matriks Operasi Kamera & Total Effort
      grid_card(
        area = "table_camop_eff", 
        full_screen = TRUE, 
        card_header("Matriks Operasi Kamera & Ringkasan Effort"), 
        card_body(
          # Teks deskriptif dengan highlight total hari efektif
          h4(textOutput("text_camdays_effective"), style = "color: #2E7D32; font-weight: bold; margin-bottom: 10px;"),
          p("Keterangan: Angka 1 menunjukkan kamera aktif, NA/0 menunjukkan kamera tidak aktif/bermasalah pada tanggal tersebut.", style = "color: #666; font-size: 12px; margin-bottom: 15px;"),
          
          # Output tabel operasi kamera (bisa scroll horizontal otomatis di server)
          DTOutput("table_camop")
        )
      ),
      
      # Jendela Bawah: Tabel RAI yang sudah ada sebelumnya
      grid_card(
        area = "table_rai", 
        full_screen = TRUE, 
        card_header("Tabel RAI & Psi Naive per Spesies (Independent Event 30 Menit)"), 
        card_body(
          DTOutput("table_sum_rai")
        )
      )
    )
  ),
  
  # ==========================================
  # TAB 3: SPATIAL MAPS & ACTIVITY
  # ==========================================
  nav_panel(
    title = "Spatial Maps & Activity",
    grid_container(
      layout = c("map_area activity_area"),
      row_sizes = c("1fr"), col_sizes = c("1.2fr", "0.8fr"), gap_size = "10px",
      grid_card(area = "map_area", full_screen = TRUE, card_header("Peta Interaktif Kekayaan Spesies (Spatial Maps)"), card_body(leafletOutput("map_spatial", height = "100%"))),
      grid_card(area = "activity_area", full_screen = TRUE, 
                card_header("Overlapping Aktivitas Satwa"), 
                card_body(
                  selectInput("spec_a", "Pilih Spesies A:", choices = NULL),
                  selectInput("spec_b", "Pilih Spesies B:", choices = NULL),
                  plotOutput("plot_overlap")
                )
      )
    )
  ),
  
  # ==========================================
  # TAB 4: ADVANCED ANALYSIS (IUCN & SAC) - PERBAIKAN TOTAL
  # ==========================================
  nav_panel(
    title = "Advanced Analysis",
    grid_container(
      layout = c("sac_zone iucn_zone"),
      row_sizes = c("1fr"), col_sizes = c("1fr", "1fr"), gap_size = "10px",
      
      # Card Kiri: Kurva Akumulasi Spesies
      grid_card(
        area = "sac_zone",
        full_screen = TRUE,
        card_header("Kurva Akumulasi Spesies (SAC)"),
        card_body(
          plotOutput("plot_sac", height = "100%")
        )
      ),
      
      # Card Kanan: Integrasi API Status Konservasi
      grid_card(
        area = "iucn_zone",
        full_screen = TRUE,
        card_header("Status Konservasi Spesies (IUCN & CITES)"),
        card_body(
          checkboxInput("myCheckboxInput", "Hubungkan ke API Global (IUCN & CITES)", value = FALSE),
          actionButton("faunacalculate", "Kalkulasi & Ambil Data Status", class = "btn-primary w-100"),
          br(), br(),
          DTOutput("table_iucn_cites")
        )
      )
    )
  )
)