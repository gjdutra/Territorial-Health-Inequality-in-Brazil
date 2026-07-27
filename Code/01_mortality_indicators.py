"""
Indicadores municipais de mortalidade - SINASC e SIM

Executar na raiz do projeto:
    Programs/python.exe "Mortality Index Project/Code/01_mortality_indicators.py"

Todos os municipios encontrados nas variaveis municipais sao agregados.
"""

from pathlib import Path
import re
import subprocess
import sys
import warnings
import csv
import io

try:
    import numpy as np
    import pandas as pd
except ImportError:
    subprocess.check_call([sys.executable, "-m", "pip", "install", "numpy", "pandas"])
    import numpy as np
    import pandas as pd

ROOT = Path.cwd()
RAW = ROOT / "Raw Data"
PROJECT = ROOT / "Mortality Index Project"
DATA = PROJECT / "Data"
OUTPUT = PROJECT / "Output"
MAPS = OUTPUT / "Maps"
LOG = OUTPUT / "validation_log_python.txt"

for folder in [DATA, OUTPUT, MAPS]:
    folder.mkdir(parents=True, exist_ok=True)

MAP_REFERENCE_YEAR = 2005
MAP_CLASSES = 7
PURPLE_PALETTE = [
    "#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f"
]

# Os arquivos disponiveis cobrem 1996-2019. Alterar aqui para reduzir o periodo.
ANALYSIS_YEARS = [
    int(x) for x in __import__("os").environ.get(
        "ANALYSIS_YEARS", "2000,2005,2010,2015"
    ).split(",")
]


def log(text):
    print(text)
    with LOG.open("a", encoding="utf-8") as f:
        f.write(str(text) + "\n")


def code6(values):
    """Padroniza codigo municipal: seis primeiros digitos do codigo IBGE."""
    x = values.astype("string").str.replace(r"\.0$", "", regex=True).str.strip()
    x = x.where(x.str.fullmatch(r"\d{6,7}", na=False))
    x = x.str[:6]
    return x.where(~x.str.endswith("0000", na=False))


def map_breaks(reference_values, n_classes=MAP_CLASSES):
    """Calcula a escala fixa dos mapas usando os valores de 2005."""
    ref = pd.to_numeric(reference_values, errors="coerce").dropna()
    if ref.empty:
        return None, None
    if ref.nunique() == 1:
        value = ref.iloc[0]
        return [-np.inf, value, np.inf], [f"<= {value:.2f}", f"> {value:.2f}"]
    cuts = np.unique(
        np.quantile(ref, np.linspace(1 / n_classes, (n_classes - 1) / n_classes,
                                    n_classes - 1))
    )
    if len(cuts) < (n_classes - 1):
        cuts = np.linspace(ref.min(), ref.max(), n_classes + 1)[1:n_classes]
    cuts = np.unique(cuts)
    breaks = np.r_[-np.inf, cuts, np.inf]
    labels = []
    for lower, upper in zip(breaks[:-1], breaks[1:]):
        if np.isneginf(lower):
            labels.append(f"<= {upper:.2f}")
        elif np.isposinf(upper):
            labels.append(f"> {lower:.2f}")
        else:
            labels.append(f"{lower:.2f} - {upper:.2f}")
    return breaks, labels


def map_categories(values, scale):
    """Aplica a escala fixa calculada com os valores de 2005."""
    breaks, labels = scale
    if breaks is None:
        return pd.Series(pd.NA, index=values.index, dtype="string")
    x = pd.to_numeric(values, errors="coerce")
    return pd.cut(x, bins=breaks, labels=labels, include_lowest=True, ordered=True)


def year_from_name(path):
    found = re.search(r"_(\d{4})_t", path.name)
    return int(found.group(1)) if found else None


def find_files(folder, prefix):
    """Localiza formatos tabulares documentados no pedido."""
    extensions = {".csv", ".txt", ".dbf", ".dbc", ".parquet", ".rds"}
    files = [
        p for p in folder.rglob("*")
        if p.is_file() and p.suffix.lower() in extensions and p.name.startswith(prefix)
    ]
    return sorted(
        [p for p in files if year_from_name(p) in ANALYSIS_YEARS]
    )


def read_table(path, columns=None):
    """Le CSV/TXT/DBF/Parquet/RDS. DBC requer conversao com read.dbc no R."""
    ext = path.suffix.lower()
    if ext in {".csv", ".txt"}:
        try:
            return pd.read_csv(
                path, usecols=columns, dtype="string", encoding="utf-8",
                low_memory=False
            )
        except UnicodeDecodeError:
            return pd.read_csv(
                path, usecols=columns, dtype="string", encoding="latin1",
                low_memory=False
            )
    if ext == ".parquet":
        data = pd.read_parquet(path, columns=columns)
        return data.astype("string")
    if ext == ".dbf":
        try:
            import geopandas as gpd
            data = gpd.read_file(path, ignore_geometry=True)
            return data[columns].astype("string") if columns else data.astype("string")
        except ImportError as exc:
            raise ImportError("Instalar geopandas para ler DBF.") from exc
    if ext == ".rds":
        try:
            import pyreadr
            data = next(iter(pyreadr.read_r(str(path)).values()))
            return data[columns].astype("string") if columns else data.astype("string")
        except ImportError as exc:
            raise ImportError("Instalar pyreadr para ler RDS.") from exc
    if ext == ".dbc":
        raise RuntimeError(
            f"Python nao le DBC diretamente: converter {path} com o script R/read.dbc."
        )
    raise ValueError(f"Formato nao reconhecido: {path}")


def summarize_births(files):
    needed = [
        "CODMUNRES", "CODMUNNASC", "ano_nasc",
        "res_codigo_adotado", "nasc_codigo_adotado",
        "res_MUNNOME", "nasc_MUNNOME"
    ]
    pieces = []
    for path in files:
        header = read_table(path, columns=None).columns.tolist() if path.suffix.lower() not in {".csv", ".txt"} else pd.read_csv(path, nrows=0).columns.tolist()
        use = [c for c in needed if c in header]
        required = {"CODMUNRES", "CODMUNNASC"}
        if not required.issubset(use):
            raise ValueError(f"Colunas SINASC ausentes em {path.name}: {required - set(use)}")
        data = read_table(path, use)
        data["year"] = pd.to_numeric(data.get("ano_nasc", year_from_name(path)), errors="coerce")
        if "ano_nasc" not in data:
            data["year"] = year_from_name(path)
        log(f"SINASC {path.name}: {len(data):,} observacoes")
        for kind, code_col, adopted_col, name_col in [
            ("residence", "CODMUNRES", "res_codigo_adotado", "res_MUNNOME"),
            ("occurrence", "CODMUNNASC", "nasc_codigo_adotado", "nasc_MUNNOME")
        ]:
            municipal_code = data[adopted_col] if adopted_col in data else data[code_col]
            municipal_code = municipal_code.fillna(data[code_col])
            tmp = pd.DataFrame({
                "municipality_code": code6(municipal_code),
                "year": data["year"],
                "municipality_name": data[name_col] if name_col in data else pd.NA,
                "measure_type": kind
            })
            pieces.append(tmp.dropna(subset=["municipality_code", "year"]))
    births = pd.concat(pieces, ignore_index=True)
    births = (
        births.groupby(["municipality_code", "year", "measure_type"], as_index=False)
        .agg(live_births=("municipality_code", "size"),
             municipality_name=("municipality_name", "first"))
    )
    births["year"] = births["year"].astype(int)
    return births


def age_flags(age):
    """Interpreta IDADE conforme dict_SIM.csv; 400 significa <1 ano sem unidade."""
    value = pd.to_numeric(age, errors="coerce")
    unit = np.floor(value / 100)
    amount = value % 100
    known_minutes = (unit == 0) & (value != 0)
    neonatal = known_minutes | (unit == 1) | ((unit == 2) & (amount <= 27))
    infant = known_minutes | unit.isin([1, 2, 3]) | (value == 400)
    under5 = infant | ((unit == 4) & (amount < 5))
    return neonatal.fillna(False), infant.fillna(False), under5.fillna(False)


def maternal_flag(cause, sex):
    """
    Assuncao: morte materna = sexo feminino e causa basica CID-10 O00-O95
    ou O98-O99. O96/O97 (morte materna tardia/sequela) ficam fora da razao
    padrao. O codebook documenta CAUSABAS, mas nao define o indicador.
    """
    cid = cause.astype("string").str.upper().str.replace(r"[^A-Z0-9]", "", regex=True)
    number = pd.to_numeric(cid.str.extract(r"^O(\d{2})", expand=False), errors="coerce")
    female = pd.to_numeric(sex, errors="coerce").eq(2)
    return female & ((number.between(0, 95)) | (number.between(98, 99)))


def summarize_deaths(files):
    needed = [
        "TIPOBITO", "CODMUNRES", "CODMUNOCOR", "ano_obito", "IDADE",
        "SEXO", "CAUSABAS", "res_codigo_adotado", "ocor_codigo_adotado",
        "res_MUNNOME", "ocor_MUNNOME"
    ]
    pieces = []
    for path in files:
        header = pd.read_csv(path, nrows=0).columns.tolist() if path.suffix.lower() in {".csv", ".txt"} else read_table(path).columns.tolist()
        use = [c for c in needed if c in header]
        required = {"TIPOBITO", "CODMUNRES", "CODMUNOCOR", "IDADE", "SEXO", "CAUSABAS"}
        if not required.issubset(use):
            raise ValueError(f"Colunas SIM ausentes em {path.name}: {required - set(use)}")
        data = read_table(path, use)
        data["year"] = pd.to_numeric(data.get("ano_obito", year_from_name(path)), errors="coerce")
        if "ano_obito" not in data:
            data["year"] = year_from_name(path)
        data = data[pd.to_numeric(data["TIPOBITO"], errors="coerce").eq(2)].copy()
        data["neonatal_deaths"], data["infant_deaths"], data["under5_deaths"] = age_flags(data["IDADE"])
        data["maternal_deaths"] = maternal_flag(data["CAUSABAS"], data["SEXO"])
        log(f"SIM {path.name}: {len(data):,} obitos nao fetais")
        for kind, code_col, adopted_col, name_col in [
            ("residence", "CODMUNRES", "res_codigo_adotado", "res_MUNNOME"),
            ("occurrence", "CODMUNOCOR", "ocor_codigo_adotado", "ocor_MUNNOME")
        ]:
            tmp = data[["year", "neonatal_deaths", "infant_deaths",
                        "under5_deaths", "maternal_deaths"]].copy()
            municipal_code = data[adopted_col] if adopted_col in data else data[code_col]
            municipal_code = municipal_code.fillna(data[code_col])
            tmp["municipality_code"] = code6(municipal_code)
            tmp["municipality_name"] = data[name_col] if name_col in data else pd.NA
            tmp["measure_type"] = kind
            pieces.append(tmp.dropna(subset=["municipality_code", "year"]))
    deaths = pd.concat(pieces, ignore_index=True)
    counts = ["neonatal_deaths", "infant_deaths", "under5_deaths", "maternal_deaths"]
    deaths = (
        deaths.groupby(["municipality_code", "year", "measure_type"], as_index=False)
        .agg(**{c: (c, "sum") for c in counts},
             municipality_name=("municipality_name", "first"))
    )
    deaths["year"] = deaths["year"].astype(int)
    return deaths


def make_maps(indicators):
    shape_files = sorted(RAW.rglob("*.shp")) + sorted(PROJECT.rglob("*.shp"))
    if not shape_files:
        log("AVISO MAPA: nenhum shapefile encontrado; mapas nao foram gerados.")
        return
    try:
        import geopandas as gpd
        import matplotlib.pyplot as plt
        from matplotlib.colors import ListedColormap
    except ImportError:
        log("AVISO MAPA: instalar geopandas e matplotlib.")
        return
    shape = gpd.read_file(shape_files[0])
    candidates = [c for c in shape.columns if c.lower() in {
        "code_muni", "code_mn", "codmun", "cd_mun", "cd_mun7", "geocodigo", "ibge"
    }]
    if not candidates:
        log(f"AVISO MAPA: coluna de codigo nao identificada em {shape_files[0]}.")
        return
    shape["municipality_code"] = code6(shape[candidates[0]])
    main = indicators[indicators["measure_type"].eq("residence")]
    unmatched = sorted(set(main["municipality_code"]) - set(shape["municipality_code"]))
    log(f"Municipios sem correspondencia no shapefile: {len(unmatched)}")
    variables = [
        "neonatal_per_1000", "infant_per_1000",
        "under5_per_1000", "maternal_per_100000"
    ]
    scales = {
        variable: map_breaks(
            main.loc[main["year"].eq(MAP_REFERENCE_YEAR), variable]
        )
        for variable in variables
    }
    for year in sorted(main["year"].unique()):
        geo = shape.merge(main[main["year"].eq(year)], on="municipality_code", how="inner")
        for variable in variables:
            if geo.empty:
                continue
            geo["classe_mapa"] = map_categories(geo[variable], scales[variable])
            ax = geo.plot(column="classe_mapa", legend=True,
                          cmap=ListedColormap(PURPLE_PALETTE),
                          categorical=True,
                          edgecolor="white", linewidth=0.2,
                          legend_kwds={
                              "title": f"{variable}\n7 categorias\nescala 2005"
                          },
                          missing_kwds={"color": "lightgrey"})
            ax.set_axis_off()
            ax.set_title(f"{variable} - residencia - {year}")
            plt.tight_layout()
            plt.savefig(MAPS / f"{variable}_residence_{year}.png", dpi=300)
            plt.close()


def embedded_csv_xlsx(path):
    """Le XLSX fornecido, que contem uma linha CSV em cada celula da coluna A."""
    try:
        cells = pd.read_excel(path, header=None).iloc[:, 0].dropna().astype(str)
    except ImportError:
        subprocess.check_call([sys.executable, "-m", "pip", "install", "openpyxl"])
        cells = pd.read_excel(path, header=None).iloc[:, 0].dropna().astype(str)
    rows = list(csv.reader(io.StringIO("\n".join(cells))))
    return pd.DataFrame(rows[1:], columns=rows[0])


def complete_municipality_panel(indicators):
    """Mantem os 5.570 municipios em todos os anos e tipos de medida."""
    crosswalk_file = RAW / "Health Regions BR" / "tb_ibge.xlsx"
    crosswalk = embedded_csv_xlsx(crosswalk_file)
    names = pd.DataFrame({
        "municipality_code": code6(crosswalk["ibge"]),
        "municipality_name_crosswalk": crosswalk["no_cidade"]
    }).dropna().drop_duplicates("municipality_code")
    grid = pd.MultiIndex.from_product(
        [names["municipality_code"], ANALYSIS_YEARS, ["residence", "occurrence"]],
        names=["municipality_code", "year", "measure_type"]
    ).to_frame(index=False)
    result = grid.merge(indicators, on=["municipality_code", "year", "measure_type"], how="left")
    result = result.merge(names, on="municipality_code", how="left")
    result["municipality_name"] = result["municipality_name"].fillna(
        result["municipality_name_crosswalk"]
    )
    result = result.drop(columns=["municipality_name_crosswalk"])
    counts = ["live_births", "neonatal_deaths", "infant_deaths",
              "under5_deaths", "maternal_deaths"]
    result[counts] = result[counts].fillna(0).astype(int)
    denominator = result["live_births"].replace(0, np.nan)
    result["neonatal_per_1000"] = 1000 * result["neonatal_deaths"] / denominator
    result["infant_per_1000"] = 1000 * result["infant_deaths"] / denominator
    result["under5_per_1000"] = 1000 * result["under5_deaths"] / denominator
    result["maternal_per_100000"] = 100000 * result["maternal_deaths"] / denominator
    return result.sort_values(["municipality_code", "year", "measure_type"])


def health_region_outputs(indicators):
    crosswalk_file = RAW / "Health Regions BR" / "tb_ibge.xlsx"
    shape_file = DATA / "Shapefiles" / "regioes_saude" / "regioes_saude.shp"
    if not crosswalk_file.exists():
        log("AVISO REGIAO: crosswalk tb_ibge.xlsx nao encontrado.")
        return
    crosswalk = embedded_csv_xlsx(crosswalk_file)
    crosswalk = pd.DataFrame({
        "municipality_code": code6(crosswalk["ibge"]),
        "health_region_code": crosswalk["co_regiao_saude"].str.zfill(5)
    }).dropna().drop_duplicates()
    joined = indicators.merge(crosswalk, on="municipality_code", how="left")
    log(f"Municipios sem regiao de saude: "
        f"{joined.loc[joined.health_region_code.isna(), 'municipality_code'].nunique()}")
    counts = ["live_births", "neonatal_deaths", "infant_deaths",
              "under5_deaths", "maternal_deaths"]
    health = (
        joined.dropna(subset=["health_region_code"])
        .groupby(["health_region_code", "year", "measure_type"], as_index=False)[counts]
        .sum()
    )
    denominator = health["live_births"].replace(0, np.nan)
    health["neonatal_per_1000"] = 1000 * health["neonatal_deaths"] / denominator
    health["infant_per_1000"] = 1000 * health["infant_deaths"] / denominator
    health["under5_per_1000"] = 1000 * health["under5_deaths"] / denominator
    health["maternal_per_100000"] = 100000 * health["maternal_deaths"] / denominator
    health.to_csv(
        OUTPUT / "mortality_indicators_health_region_year_python.csv", index=False
    )
    if not shape_file.exists():
        log("AVISO REGIAO: execute primeiro o script R para reconstruir a geometria WKB.")
        return
    try:
        import geopandas as gpd
        import matplotlib.pyplot as plt
        from matplotlib.colors import ListedColormap
    except ImportError:
        log("AVISO REGIAO: instalar geopandas e matplotlib para os mapas.")
        return
    shape = gpd.read_file(shape_file)
    code_candidates = [
        c for c in shape.columns
        if c.lower() in {
            "health_region_code", "hlth_r_", "hlth_rg", "hlth_rgn_c", "co_regsaud"
        }
        or "code" in c.lower()
    ]
    if not code_candidates:
        log("AVISO REGIAO: codigo regional nao identificado no shapefile.")
        return
    shape["health_region_code"] = (
        shape[code_candidates[0]].astype("string").str.replace(r"\.0$", "", regex=True).str.zfill(5)
    )
    main = health[health["measure_type"].eq("residence")]
    unmatched = set(main["health_region_code"]) - set(shape["health_region_code"])
    log(f"Regioes sem correspondencia na geometria: {len(unmatched)}")
    variables = ["neonatal_per_1000", "infant_per_1000",
                 "under5_per_1000", "maternal_per_100000"]
    scales = {
        variable: map_breaks(
            main.loc[main["year"].eq(MAP_REFERENCE_YEAR), variable]
        )
        for variable in variables
    }
    for year in sorted(main["year"].unique()):
        geo = shape.merge(main[main["year"].eq(year)], on="health_region_code", how="left")
        for variable in variables:
            geo["classe_mapa"] = map_categories(geo[variable], scales[variable])
            ax = geo.plot(column="classe_mapa", legend=True,
                          cmap=ListedColormap(PURPLE_PALETTE),
                          categorical=True,
                          edgecolor="white", linewidth=0.2,
                          legend_kwds={
                              "title": f"{variable}\n7 categorias\nescala 2005"
                          },
                          missing_kwds={"color": "lightgrey"})
            ax.set_axis_off()
            ax.set_title(f"{variable} - regiao de saude - residencia - {year}")
            plt.tight_layout()
            plt.savefig(MAPS / f"{variable}_health_region_residence_{year}.png", dpi=300)
            plt.close()


def main():
    LOG.write_text("VALIDACAO - PYTHON\n", encoding="utf-8")
    sinasc_files = find_files(RAW / "ETLSINASC", "ETLSINASC")
    sim_files = find_files(RAW / "ETLSIM", "ETLSIM")
    if not sinasc_files or not sim_files:
        raise FileNotFoundError("Arquivos SINASC/SIM nao encontrados.")
    log(f"Arquivos SINASC: {len(sinasc_files)}; arquivos SIM: {len(sim_files)}")

    births = summarize_births(sinasc_files)
    deaths = summarize_deaths(sim_files)
    keys = ["municipality_code", "year", "measure_type"]
    final = births.merge(deaths, on=keys, how="outer", suffixes=("_birth", "_death"))
    final["municipality_name"] = final["municipality_name_birth"].fillna(final["municipality_name_death"])
    final = final.drop(columns=["municipality_name_birth", "municipality_name_death"])
    count_cols = ["live_births", "neonatal_deaths", "infant_deaths",
                  "under5_deaths", "maternal_deaths"]
    final[count_cols] = final[count_cols].fillna(0).astype(int)
    denominator = final["live_births"].replace(0, np.nan)
    final["neonatal_per_1000"] = 1000 * final["neonatal_deaths"] / denominator
    final["infant_per_1000"] = 1000 * final["infant_deaths"] / denominator
    final["under5_per_1000"] = 1000 * final["under5_deaths"] / denominator
    final["maternal_per_100000"] = 100000 * final["maternal_deaths"] / denominator
    final = final[["municipality_code", "municipality_name", "year", "measure_type"] +
                  count_cols + ["neonatal_per_1000", "infant_per_1000",
                                "under5_per_1000", "maternal_per_100000"]]
    final = complete_municipality_panel(final)

    births.to_csv(OUTPUT / "sinasc_births_municipality_year_python.csv", index=False)
    deaths.to_csv(OUTPUT / "sim_deaths_municipality_year_python.csv", index=False)
    final.to_csv(OUTPUT / "mortality_indicators_municipality_year_python.csv", index=False)

    log(f"Anos SINASC: {births.year.min()}-{births.year.max()}")
    log(f"Anos SIM: {deaths.year.min()}-{deaths.year.max()}")
    log(f"Municipios na base final: {final.municipality_code.nunique()}")
    log(f"Municipio/ano/tipo com obitos infantis > nascidos vivos: "
        f"{(final.infant_deaths > final.live_births).sum()}")
    log(f"Valores ausentes - codigo: {final.municipality_code.isna().sum()}; "
        f"ano: {final.year.isna().sum()}")
    make_maps(final)
    health_region_outputs(final)


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        log(f"ERRO: {error}")
        raise
