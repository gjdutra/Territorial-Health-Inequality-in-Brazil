# Municipality-year mortality indicators

Run either script from the project root:

```text
Programs/R-4.6.1/bin/Rscript.exe "Mortality Index Project/Code/01_mortality_indicators.R"
Programs/python.exe "Mortality Index Project/Code/01_mortality_indicators.py"
```

The scripts aggregate every municipality found in the municipal variables.
Outputs are written to `Output/`.

The default analysis years are 2000, 2005, 2010, and 2015. Override them with
the comma-separated environment variable `ANALYSIS_YEARS`.

## Variables checked in the available codebooks

- SINASC: `CODMUNRES` (mother's residence), `CODMUNNASC` (occurrence),
  `ano_nasc` (year), plus `res_codigo_adotado` and `nasc_codigo_adotado`
  for harmonized municipality codes. Each row is one live birth.
- SIM: `CODMUNRES` (deceased person's residence), `CODMUNOCOR` (occurrence),
  `ano_obito` (year), `TIPOBITO` (2 = non-fetal), `IDADE` (encoded age),
  `SEXO`, `CAUSABAS` (underlying ICD-10 cause), `res_codigo_adotado`, and
  `ocor_codigo_adotado`.
- Municipality attributes such as `res_CAPITAL`, `res_AMAZONIA` and
  `res_FRONTEIRA` exist, but no single “municipality type” variable is
  documented. `measure_type` in the outputs distinguishes residence from
  occurrence.

## Obtaining the data

Individual-level SIM and SINASC records are not distributed in this repository.
The analysis will run after the required files are available locally. Users can
either supply previously downloaded DATASUS files or download them directly in
R with the [`microdatasus`](https://github.com/rfsaldanha/microdatasus) package.

The example below downloads the four default analysis years for all Brazilian
states, preprocesses the records, and saves them using filenames recognized by
the analysis scripts:

```r
install.packages(c("microdatasus", "data.table"))

library(microdatasus)
library(data.table)

years <- c(2000, 2005, 2010, 2015)
dir.create("Raw Data/ETLSINASC", recursive = TRUE, showWarnings = FALSE)
dir.create("Raw Data/ETLSIM", recursive = TRUE, showWarnings = FALSE)

for (year in years) {
  sinasc <- fetch_datasus(
    year_start = year,
    year_end = year,
    uf = "all",
    information_system = "SINASC"
  )
  sinasc <- process_sinasc(sinasc)
  fwrite(
    sinasc,
    sprintf("Raw Data/ETLSINASC/ETLSINASC_BR_%d_t.csv", year)
  )

  sim <- fetch_datasus(
    year_start = year,
    year_end = year,
    uf = "all",
    information_system = "SIM-DO"
  )
  sim <- process_sim(sim)
  fwrite(
    sim,
    sprintf("Raw Data/ETLSIM/ETLSIM_BR_%d_t.csv", year)
  )
}
```

Downloading national microdata can take considerable time and memory. A stable
internet connection is required, and DATASUS may restrict FTP downloads from
some countries. For a smaller test, replace `uf = "all"` with one or more state
codes, such as `uf = "ES"`.

After the files have been saved, run the R or Python indicator script from the
workspace root. The scripts detect the selected years through `ANALYSIS_YEARS`;
if it is not set, they use 2000, 2005, 2010, and 2015.

## Definitions and assumptions

- Neonatal: completed age 0–27 days. Minutes and hours are included.
- Infant: age below one year, including SIM code 400 (below one year with exact
  unit unknown).
- Under five: age below five years.
- Maternal: female death with underlying ICD-10 cause O00–O95 or O98–O99.
  O96/O97 are excluded as late maternal death/sequela. The SIM dictionary
  documents the fields but does not itself define the aggregate indicator.
- Municipality codes are harmonized to the first six IBGE digits because the
  raw health files use six digits while common shapefiles use seven. The
  documented adopted-code fields are preferred to handle municipal changes;
  state-level unknown placeholders ending in `0000` are excluded.
- Residence is the primary interpretation; occurrence is retained separately.

## Spatial data

The R script uses `geobr::read_municipality()` to obtain the simplified 2020
municipal boundaries and saves a reusable shapefile under
`Data/Shapefiles/municipios_2020/`. The Python script reads this local shapefile.
Both implementations harmonize codes, report unmatched municipalities, and
save residence-based maps in `Output/Maps`.

The project currently contains the downloaded 2020 layer with 5,570 municipal
polygons.

## Health regions

The municipality-to-health-region relationship comes from
`Raw Data/Health Regions BR/tb_ibge.xlsx`. The supplied workbooks contain
CSV-formatted lines in their first Excel column. In `tb_regiao_saude.xlsx`,
long hexadecimal WKB geometries are split over several Excel cells; the R
script reconstructs the 450 records, converts the WKB to `sf`, and saves a
reusable health-region shapefile under `Data/Shapefiles/regioes_saude/`.

Health-region counts are sums of municipality counts. Rates are recalculated
from the summed deaths and live births rather than averaging municipal rates.

Each implementation writes its own validation log and language-suffixed CSVs so
running one script does not overwrite the other.
