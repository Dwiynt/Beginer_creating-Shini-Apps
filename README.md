# Fauna Rapid Analysis - Camera Trap Edition 🐾

An interactive R Shiny application for processing camera trap metadata, calculating Relative Abundance Index (RAI), visualizing spatial biodiversity patterns, and modeling Species Accumulation Curves (SAC).

---

## 🛠️ Key Features
- Automated extraction of photo & video Exif metadata (`camtrapR`).
- Independent Event filtering (30-minute interval).
- Camera Operation Matrix & Effective Trap Days calculation.
- Species Accumulation Curve (SAC) visualization with *Jackknife Order 1* estimator.
- Interactive spatial maps of species richness (`leaflet`).
- Integrated species conservation status assessment using **GetTaxonCS**.

---

## ⚙️ External Prerequisites & System Requirements

Before running the application or processing camera trap metadata via `camtrapR`, you **must** have ExifTool installed on your system:

* **ExifTool**: Download and install the standalone executable or library from official source [ExifTool by Phil Harvey](https://exiftool.org/).
* Ensure the executable path is added to your system's `PATH` variable or properly set within R via `camtrapR::addExifToolPath()`.

---

## 🔍 GetTaxonCS Module & API Setup

The **GetTaxonCS** module is designed to retrieve taxonomic and conservation status information from three primary sources:
1. **IUCN Red List**
2. **CITES Appendices**
3. **Regulation of the Minister of Environment and Forestry of Indonesia** (*P.106/MENLHK/SETJEN/KUM.1/12/2018*)

### How to Use the API

To enable full functionality for species conservation checks:
1. Register and request an API token from the [IUCN Red List API](https://api.iucnredlist.org/).
2. Register and create an API token from the [Species+/CITES API](https://api.speciesplus.net/documentation).

> **Note:** The API keys provided in the example code are placeholders and will not work out of the box. You must register for your own API keys and replace the sample keys in the application settings/script.

---

## 👨‍💻 Authors & Developers

* **Lead Developer**: Dwiyanto
* **Affiliation / Organization**: Fauna & Flora Indonesia Programme
* **Email / Contact**: dwiyanto@fauna-flora.org
* **Co-author**: Ryan Avriandy (ryan.avriandy@fauna-flora.org)
* **GitHub Profiles**: [@Dwiynt](https://github.com/Dwiynt) | [@ryanavri](https://github.com/ryanavri)
* **ORCID**: [0000-0001-7545-6473](https://orcid.org/0000-0001-7545-6473)

---

## 📬 Feedback & Collaboration
If you have any questions or feedback about this project, please contact me. We welcome any collaboration or contribution to this project.

---

## 📄 License & Copyright
Copyright © 2026 Dwiyanto / Fauna & Flora Indonesia Programme. All rights reserved.