DATA INSPECTION AND QUALITY NOTES

Documentation used
- Raw Data/ETLSINASC/dict_SINASC.csv
- Raw Data/ETLSIM/dict_SIM.csv
- No folder named Codebooks was present.

Raw data found
- SINASC: 648 annual state CSV files, 1996-2019 (about 34.46 GB).
- SIM: 648 annual state CSV files, 1996-2019 (about 20.67 GB).
- The files are individual-level records partitioned by residence UF/year.
- No shapefile (.shp) was initially supplied. The R pipeline downloads the
  simplified 2020 municipality layer with geobr and saves it in the project.

Verified variables
- SINASC residence municipality: CODMUNRES, harmonized with res_codigo_adotado.
- SINASC birth/occurrence municipality: CODMUNNASC, harmonized with
  nasc_codigo_adotado.
- SINASC year: ano_nasc; one row represents one live birth.
- SIM residence municipality: CODMUNRES, harmonized with res_codigo_adotado.
- SIM occurrence municipality: CODMUNOCOR, harmonized with ocor_codigo_adotado.
- SIM year: ano_obito.
- SIM non-fetal death: TIPOBITO = 2.
- SIM age: IDADE, encoded as unit plus quantity per dict_SIM.csv.
- SIM maternal-death inputs: SEXO and CAUSABAS.
- No single documented "municipality type" field exists. Output measure_type
  records whether each summary is residence or occurrence based.

Assumptions
- Neonatal means 0-27 completed days; minutes and hours are included.
- Infant means below one year, including IDADE = 400.
- Under five means below five years.
- Maternal means female with underlying ICD-10 O00-O95 or O98-O99.
  O96/O97 are excluded from the standard maternal mortality ratio.
- Municipality codes are harmonized to six IBGE digits.
- State-level unknown placeholders ending in 0000 are not municipalities and
  are excluded.

Municipality coverage
- The supplied raw data are national. Every municipality found in CODMUNRES,
  CODMUNNASC, and CODMUNOCOR is aggregated; no external target list is used.

Execution status
- Python script passed byte-code compilation.
- R script passed parsing.
- The R pipeline was run for 2000, 2005, 2010, and 2015.
- It loaded 108 SINASC files and 108 SIM files.
- The final panel contains all 5,570 municipalities in every selected year and
  in both residence and occurrence versions.
- The health-region panel contains all 450 regions in both versions.
- Municipality and health-region spatial joins left zero unmatched codes.
- The supplied health-region names contain encoding corruption in the workbook
  (for example accented letters rendered incorrectly). Codes and geometries
  are unaffected and are therefore used as the authoritative join fields.
- The local Python and R installations currently lack the analysis packages.
  Both scripts install required core packages when run; internet access may be
  needed for that first installation.
