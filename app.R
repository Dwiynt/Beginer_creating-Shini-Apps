library(shiny)
library(shinydisconnect)
library(bslib)
library(gridlayout)
library(DT)
library(mapview)
library(camtrapR)  
library(exiftoolr)

# 1. Definisikan path absolut ke folder win_exe Anda
exif_folder <- "your_exif_folder_location"
exif_file <- file.path(exif_folder, "exiftool.exe")

# 2. Paksa R mendeteksi Exiftool lokal lewat Environment System
if (file.exists(exif_file)) {
  # Mengatur path internal exiftoolr
  options(exiftoolr.path = exif_folder)
  
  # Menyuntikkan langsung folder Exiftool ke dalam PATH sistem operasi R
  # Ini yang membuat camtrapR otomatis tahu di mana exiftool berada saat memproses gambar!
  Sys.setenv(PATH = paste(exif_folder, Sys.getenv("PATH"), sep = ";"))
  
  # Konfigurasi paksa untuk package exiftoolr
  try(exiftoolr::configure_exiftool(command = exif_file, quiet = TRUE), silent = TRUE)
  message("Berhasil mengunci dan mengonfigurasi Exiftool dari OneDrive lewat Environment!")
} else {
  message("Peringatan: File exiftool.exe tidak ditemukan di folder OneDrive Anda.")
}

# 3. Pengecekan versi akhir (Hanya pelengkap log, tidak memblokir aplikasi)
tryCatch({
  ver <- exiftoolr::exiftool_version()
  message(paste("Status: Exiftool Siap Digunakan! Versi:", ver))
}, error = function(e) {
  message("Catatan: Pengecekan versi dilewati, aplikasi tetap berjalan menggunakan konfigurasi lokal.")
})

# 4. Load Arsitektur Aplikasi Modular
source("app_ui.R")
source("app_server.R")
source("iucn_code.R")

# 5. Jalankan Aplikasi
shinyApp(ui = app_ui, server = app_server)