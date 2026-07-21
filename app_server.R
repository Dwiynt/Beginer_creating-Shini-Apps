library(shiny)
library(camtrapR)
library(tidyverse)
library(lubridate)
library(DT)
library(sf)
library(mapview)
library(leaflet)
library(shinybusy)
library(dplyr)
library(tidyr)
library(tibble)
library(vegan)

app_server <- function(input, output, session) {
  
  # 1. Membaca data Effort yang diunggah pengguna
  data_effort <- reactive({
    req(input$file_effort)
    read.csv(input$file_effort$datapath, stringsAsFactors = FALSE)
  })
  
  output$table_effort <- renderDT({
    req(data_effort())
    datatable(data_effort(), options = list(pageLength = 5))
  })
  
  # 2. Proses reaktif Ekstraksi dan Pembersihan Kamera Trap
  processed_data <- eventReactive(input$btn_process, {
    req(data_effort(), input$path_raw1)
    
    show_modal_spinner(spin = "fading-circle", color = "#2c3e50", text = "Sedang mengekstrak Exif Metadata Foto/Video...")
    
    # Validasi folder 1 wajib ada secara fisik
    if (!dir.exists(input$path_raw1)) {
      remove_modal_spinner()
      showModal(modalDialog(
        title = "Folder Tidak Ditemukan",
        paste("Folder Kamera Trap 1 tidak ditemukan pada path:", input$path_raw1),
        easyClose = TRUE, footer = modalButton("Tutup")
      ))
      req(FALSE)
    }
    
    # Ambil tabel rekaman 1
    rec1 <- recordTable(inDir = input$path_raw1, IDfrom = "directory", timeZone = "Asia/Jakarta",
                        video = list(file_formats = c("jpg", "mp4"), dateTimeTag = "QuickTime:CreateDate"))
    
    # Cek fisik folder 2 jika diisi
    if (input$path_raw2 != "" && dir.exists(input$path_raw2)) {
      rec2 <- recordTable(inDir = input$path_raw2, IDfrom = "directory", timeZone = "Asia/Jakarta",
                          video = list(file_formats = c("jpg", "mp4"), dateTimeTag = "QuickTime:CreateDate"))
      if(nrow(rec2) > 0) {
        rec2$DateTimeOriginal <- rec2$DateTimeOriginal %m+% years(1)
      }
      detection_combined <- rbind(rec1, rec2)
      message("Folder 2 berhasil digabungkan.")
    } else {
      detection_combined <- rec1
      message("Proses hanya menggunakan data dari Folder 1.")
    }
    
    # Hitung matriks operasi kamera & Hari Trap Efektif
    camop <- cameraOperation(CTtable = data_effort(), stationCol = "Station", setupCol = "Start_date",
                             retrievalCol = "End_date", writecsv = FALSE, hasProblems = TRUE, dateFormat = "%d-%b-%y")
    camdays_effective <- sum(rowSums(camop, na.rm = TRUE))
    
    # Hitung Independent Event (30 Menit) - FIXED operator %in%
    detection_30m <- detection_combined %>%
      camtrapR:::assessTemporalIndependence(deltaTimeComparedTo = "lastIndepentRecord", columnOfInterest = "Species",
                                            stationCol = "Station", minDeltaTime = 30, camerasIndependent = FALSE) %>%
      drop_na(Species) %>%
      select(Station, Species, DateTimeOriginal, FileName, n_images) %>% 
      mutate(IE = 1) %>%
      filter(!Species %in% c('StartDate', 'EndDate', 'Aves', 'Elang', 'Bat', 'Blank', 'Unidentified', 'Rat'))
    
    # Update pilihan dropdown spesies untuk grafik overlap aktivitas
    unique_species <- unique(detection_30m$Species)
    updateSelectInput(session, "spec_a", choices = unique_species, selected = unique_species[1])
    updateSelectInput(session, "spec_b", choices = unique_species, selected = unique_species[pmin(2, length(unique_species))])
    
    remove_modal_spinner()
    
    list(detection = detection_combined, detection_30m = detection_30m, camdays_effective = camdays_effective, camop = camop)
  })
  
  # Tampilkan pratinjau data hasil deteksi mentah
  output$table_detection_head <- renderDT({
    req(processed_data())
    datatable(head(processed_data()$detection, 5), options = list(dom = 't'))
  })
  
  # Download Handler untuk file deteksi csv
  output$download_detection <- downloadHandler(
    filename = function() { "detection.csv" },
    content = function(file) {
      req(processed_data())
      processed_data()$detection %>%
        select(Station, Species, Date, Time, FileName) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  # 3. Pengolahan Ringkasan RAI & Psi Naive
  output$table_sum_rai <- renderDT({
    req(processed_data(), data_effort())
    res <- processed_data()
    
    sum_table1 <- res$detection_30m %>% 
      group_by(Species) %>% 
      summarise(n_images = sum(n_images), IE = sum(IE), .groups = 'drop') %>%
      mutate(RAI = sprintf("%.3f", IE / (res$camdays_effective / 100)))
    
    sum_table2 <- res$detection_30m %>%
      group_by(Species) %>%
      summarise(n_locs = n_distinct(Station), .groups = 'drop') %>%
      mutate(Psi_Naive = n_locs / n_distinct(data_effort()$Station))
    
    final_sum <- left_join(sum_table1, sum_table2, by = "Species")
    
    base_dir <- "Camtrap_base"
    if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
    write.csv(final_sum, file.path(base_dir, "RAI_and_psi_.csv"), row.names = FALSE)
    
    datatable(final_sum, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')))
  })
  # =========================================================================
  # ===== TEMPATKEN KODE BARU DI SINI (TEPAT DI BAWAH TABLE_SUM_RAI) =====
  # =========================================================================
  
  # 4. Output Teks Hari Trap Efektif untuk Jendela Baru
  output$text_camdays_effective <- renderText({
    req(processed_data())
    res <- processed_data()
    paste("Total Keseluruhan Hari Trap Efektif:", round(res$camdays_effective, 0), "Hari-Kamera")
  })
  
  # 5. Output Tabel Matriks Operasi Kamera dengan Fitur Scroll Horizontal
  output$table_camop <- renderDT({
    req(processed_data())
    res <- processed_data()
    
    # Mengubah matriks menjadi data frame & membuat kolom Station dari rownames
    camop_df <- as.data.frame(res$camop)
    camop_df <- cbind(Station = rownames(camop_df), camop_df)
    
    datatable(
      camop_df,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,  # Supaya kalender tanggal bisa digeser ke kanan-kiri dengan aman
        pageLength = 5,  # Membatasi tampilan stasiun per halaman agar layout rapi
        dom = 'Bfrtip'
      )
    )
  })
  
  # 6. Pemetaan Spasial
  output$map_spatial <- renderLeaflet({
    req(processed_data(), data_effort())
    res <- processed_data()
    
    Sp_occs <- detectionMaps(CTtable = data_effort(), recordTable = res$detection_30m, Xcol = "X", Ycol = "Y",
                             stationCol = "Station", speciesCol = "Species", richnessPlot = FALSE, speciesPlots = FALSE,
                             writeShapefile = FALSE)
    
    colnames(Sp_occs) <- tolower(colnames(Sp_occs))
    clean_cols <- !duplicated(colnames(Sp_occs)) & colnames(Sp_occs) != ""
    Sp_occs_clean <- Sp_occs[, clean_cols, drop = FALSE]
    colnames(Sp_occs_clean) <- make.names(colnames(Sp_occs_clean))
    
    x_col <- if ("x" %in% colnames(Sp_occs_clean)) "x" else grep("^x", colnames(Sp_occs_clean), value = TRUE)[1]
    y_col <- if ("y" %in% colnames(Sp_occs_clean)) "y" else grep("^y", colnames(Sp_occs_clean), value = TRUE)[1]
    
    sample_x <- na.omit(Sp_occs_clean[[x_col]])[1]
    
    if (!is.na(sample_x) && sample_x > 180) {
      detections_sf_utm <- st_as_sf(Sp_occs_clean, coords = c(x_col, y_col), crs = 32648)
      detections_sf <- st_transform(detections_sf_utm, crs = 4326)
      message("Sistem mendeteksi format koordinat: UTM (Zone 48N) -> Berhasil dikonversi ke WGS 84.")
    } else {
      detections_sf <- st_as_sf(Sp_occs_clean, coords = c(x_col, y_col), crs = 4326)
      message("Sistem mendeteksi format koordinat: Geografis WGS 1984 -> Langsung di-plot.")
    }
    
    color_col <- if ("n_species" %in% colnames(detections_sf)) {
      "n_species"
    } else if ("n_specs" %in% colnames(detections_sf)) {
      "n_specs"
    } else {
      grep("n_spec", colnames(detections_sf), value = TRUE)[1]
    }
    
    m <- mapview(detections_sf, zcol = color_col, legend = TRUE)
    m@map
  })
  
  # 7. Overlap Aktivitas
  output$plot_overlap <- renderPlot({
    req(processed_data(), input$spec_a, input$spec_b)
    res <- processed_data()
    
    plot_dir <- "Camtrap_base/plot_output"
    if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
    
    try(activityDensity(recordTable = res$detection_30m, allSpecies = TRUE, writePNG = TRUE, 
                        plotDirectory = plot_dir, plotR = FALSE, add.rug = TRUE), silent = TRUE)
    
    if(input$spec_a == input$spec_b) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "Pilih dua spesies yang berbeda untuk melihat overlap aktivitas.", cex=1.2)
    } else {
      activityOverlap(recordTable = res$detection_30m, speciesA = input$spec_a, speciesB = input$spec_b,
                      writePNG = TRUE, plotDirectory = plot_dir, plotR = TRUE, pngMaxPix = 1000,
                      linecol = c("black", "blue"), linewidth = c(5,3), linetype = c(1, 2),
                      olapcol = "darkgrey", add.rug = TRUE, extend = "lightgrey", ylim = c(0, 0.3),
                      main = paste("Activity overlap:", input$spec_a, "-", input$spec_b))
    }
  })
 
  # 8. EVENT TRIGGER KLIK CALCULATE (VERSI UPDATE DATA CAMERA TRAP)
  species_list_reactive <- eventReactive(input$faunacalculate, {
    # ⚠️ Sesuaikan reactive object input data Anda di bawah ini, misalnya: processed_data() atau dataset()
    req(processed_data()) 
    res_data <- processed_data()
    
    print("=== [LOG SHINY] Tombol Calculate diklik ===")
    
    show_modal_spinner(
      spin = "orbit",
      color = "#112446",
      text = "Menghubungi server API IUCN & CITES... Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~"
    )
    
    # Ambil data dari list camera trap detection_30m
    det_data <- res_data$detection_30m
    
    if (input$myCheckboxInput) {
      print("[LOG SHINY] Checkbox dicentang: Mulai proses API")
      
      res <- tryCatch({
        # 1. Siapkan data frame input langsung dari kolom Species data camera trap
        print("[LOG SHINY] Langkah 1: Mempersiapkan data frame spesies...")
        Species_df <- det_data %>%
          distinct(Species) %>%
          filter(!is.na(Species), Species != "", !Species %in% c("Aves", "aves", "Unknown", "unknown", "StartDate",
                                                                 "EndDate", "Elang", "Bat", "Blank", "Unidentified", "Rat")) %>%
          mutate(
            genus = sub(" .*", "", Species),
            species = sub("^\\S+\\s+", "", Species)
          )
        print(paste("[LOG SHINY] Jumlah spesies unik ditemukan:", nrow(Species_df)))
        
        # 2. Load package secara eksplisit untuk mencegah silent error
        library(iucnredlist)
        library(rcites)
        
        # 3. Set Token
        print("[LOG SHINY] Langkah 2: Menginisialisasi Token API...")
        set_token("xxxxxxxxxx") #change with your CITES tokencode
        api <- init_api("xxxxxxxxxxxxxxxxxxxxxxxxx") ##change with your IUCN token code
        
        # 4. Ambil data IUCN
        print("[LOG SHINY] Langkah 3: Menghubungi API IUCN (get_iucn_species_data)...")
        sp1 <- retrieve_IUCN_data(api, Species_df)
        print(paste("[LOG SHINY] Sukses mengambil data IUCN. Jumlah baris:", nrow(sp1)))
        
        # 5. Ambil data CITES
        print("[LOG SHINY] Langkah 4: Menghubungi API CITES (retrieve_CITES_data)...")
        sp2 <- retrieve_CITES_data(Species_df$Species) %>%
          distinct(Species, .keep_all = TRUE)
        print(paste("[LOG SHINY] Sukses mengambil data CITES. Jumlah baris:", nrow(sp2)))
        
        # 6. Gabungkan seluruh data dengan database internal (db)
        print("[LOG SHINY] Langkah 5: Menggabungkan data IUCN + CITES + Lokal DB...")
        final_res <- left_join(sp1, sp2, by = 'Species')
        
        if (exists("db")) {
          final_res <- left_join(final_res, db, by = "Species")
        }
        
        # 7. Rapikan struktur tabel
        final_res <- final_res %>%
          select(any_of(c("Class", "Order", "Family", "Species", "Common name", "Status", "CITES_Appendix", "Appendix", "Protected", "Endemic", "Migratory"))) %>%
          mutate(across(any_of(c("Class", "Order", "Family")), function(x) if(is.character(x)) tolower(x) else x)) %>%
          mutate(across(any_of(c("Class", "Order", "Family")), function(x) if(is.character(x)) tools::toTitleCase(x) else x))
        
        if ("CITES_Appendix" %in% colnames(final_res)) {
          final_res <- final_res %>% rename(Appendix = CITES_Appendix)
        }
        
        final_res <- final_res %>% arrange(across(any_of(c("Order", "Family", "Species"))))
        
        print("[LOG SHINY] Langkah 6: Penggabungan selesai. Mengirimkan hasil ke tabel...")
        final_res
        
      }, error = function(e) {
        print(paste("!!! [LOG ERROR] Terjadi kesalahan dalam tryCatch:", e$message))
        remove_modal_spinner()
        showNotification(paste("Proses API Gagal:", e$message), type = "error", duration = 10)
        
        # Fallback list agar tabel tidak kosong total jika error
        det_data %>%
          distinct(Species) %>%
          filter(!is.na(Species), Species != "") %>%
          arrange(Species)
      })
      
    } else {
      print("[LOG SHINY] Checkbox tidak dicentang: Menampilkan data lokal saja")
      res <- det_data %>%
        distinct(Species) %>%
        filter(!is.na(Species), Species != "") %>%
        arrange(Species)
    }
    
    remove_modal_spinner()
    return(res)
  })
  
  # Render hasil tabel IUCN & CITES ke UI (BAGIAN YANG HILANG SEBELUMNYA)
  output$table_iucn_cites <- renderDT({
    req(species_list_reactive())
    datatable(species_list_reactive(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 9. Analisis Kurva Akumulasi Spesies (SAC) - Berbasis Hari & Estimator (PERBAIKAN DATE)
  output$plot_sac <- renderPlot({
    req(processed_data())
    res <- processed_data()
    
    det_data <- res$detection_30m
    
    # 1. Konversi Tanggal dan Buat Matriks Komunitas Berbasis Hari (Date x Species)
    library(dplyr)
    library(tidyr)
    library(tibble)
    library(vegan)
    
    # Bersihkan nama sampah (sebagai pengaman tambahan)
    clean_det <- det_data %>%
      filter(!is.na(Species), Species != "")
    
    # Konversi kolom DateTimeOriginal menjadi objek Date murni di R
    # Fungsi as.Date secara otomatis akan memotong jam dan mengambil tanggalnya saja
    clean_det <- clean_det %>%
      mutate(
        Date = as.Date(DateTimeOriginal)
      ) %>% 
      filter(!is.na(Date))
    
    # Kelompokkan kehadiran spesies harian untuk matriks komunitas
    comm_matrix_days <- clean_det %>%
      count(Date, Species) %>%
      tidyr::pivot_wider(names_from = Species, values_from = n, values_fill = 0) %>%
      arrange(Date) %>%
      tibble::column_to_rownames("Date")
    
    # Proteksi jika hari pengamatan terlalu sedikit
    if (nrow(comm_matrix_days) < 2) {
      plot.new()
      text(0.5, 0.5, "Dibutuhkan minimal 2 hari pengamatan untuk membuat kurva.", cex = 1.2, col = "red")
      return()
    }
    
    # 2. Hitung Akumulasi Spesies (Metode Random & Raw)
    sac_random <- vegan::specaccum(comm_matrix_days, method = "random", permutations = 100)
    sac_raw <- vegan::specaccum(comm_matrix_days, method = "collector")
    
    # 3. Hitung Estimator Kekayaan Spesies (Jackknife 1) per akumulasi hari
    pool_calc <- vegan::poolaccum(comm_matrix_days, permutations = 100)
    
    # SOLUSI PASTI: Ambil langsung dari matriks mentah jack1 dan hitung rata-rata tiap baris (hari)
    # Ini 100% aman dari struktur list summary() yang menipu
    jack1_values <- rowMeans(pool_calc$jack1)
    
    # 4. MULAI PLOTTING MANUAL (SAFE BOUNDS)
    max_x <- length(sac_random$richness)
    
    # PROTEKSI EKSTRA: Pastikan panjang data Jackknife 1 pas dengan jumlah hari (max_x)
    if (length(jack1_values) > max_x) {
      jack1_values <- jack1_values[1:max_x]
    } else if (length(jack1_values) < max_x) {
      padding <- rep(tail(jack1_values, 1), max_x - length(jack1_values))
      jack1_values <- c(jack1_values, padding)
    }
    
    # Tentukan batas maksimal sumbu Y dengan aman
    max_y <- max(c(sac_random$richness + (2 * sac_random$sd), jack1_values), na.rm = TRUE) + 2
    
    plot(1:max_x, sac_random$richness, type = "n", 
         xlim = c(0, max_x + 5), ylim = c(0, max_y),
         xlab = "Day", ylab = "Number of species",
         main = "Species Accumulation Curve & Estimator",
         panel.first = grid(nx = NULL, ny = NULL, lty = 1, col = "gray90"))
    
    # A. Gambar Garis Vertikal Confidence Interval (Error Bars)
    for(i in 1:max_x) {
      low_ci <- sac_random$richness[i] - (1.96 * sac_random$sd[i])
      up_ci <- sac_random$richness[i] + (1.96 * sac_random$sd[i])
      low_ci <- max(0, low_ci) 
      
      lines(c(i, i), c(low_ci, up_ci), col = "black", lwd = 1) 
      lines(c(i-0.2, i+0.2), c(low_ci, low_ci), col = "black") 
      lines(c(i-0.2, i+0.2), c(up_ci, up_ci), col = "black")   
    }
    
    # B. Plot Garis Sobs Utama, Upper CI, dan Lower CI
    lines(1:max_x, sac_random$richness, col = "black", lwd = 2)
    lines(1:max_x, pmax(0, sac_random$richness - (1.96 * sac_random$sd)), col = "darkgreen", lwd = 1.5, lty = 1)
    lines(1:max_x, sac_random$richness + (1.96 * sac_random$sd), col = "darkred", lwd = 1.5, lty = 1)
    
    # C. Plot Garis Raw Accumulation
    lines(1:max_x, sac_raw$richness, col = "purple", lwd = 1.5)
    
    # D. Plot Garis Estimator Jackknife Order 1
    lines(1:max_x, jack1_values, col = "magenta", lwd = 2)
    
    # E. Tambahkan Legend
    legend("topright", 
           legend = c("Sobs", "Sobs Lower CI", "Sobs Upper CI", "Jackknife Order 1", "Raw Accumulation"),
           col = c("black", "darkgreen", "darkred", "magenta", "purple"), 
           lty = 1, lwd = 2, bty = "n", cex = 0.9)
  })
  
}