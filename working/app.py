import pandas as pd
from shiny import App, render, ui

# === Load Excel file ===
file_path = "export_result_oct21.xlsx"

months = [
    "Dec23", "Jan24", "Feb24", "Mar24", "Apr24", "May24",
    "Jun24", "Jul24", "Aug24", "Sep24", "Oct24", "Nov24"
]

# === Function to extract all tables for a month ===
def extract_tables(sheet):
    df = pd.read_excel(file_path, sheet_name=sheet)

    # --- Detect where Table 2 starts ---
    food_start = df.index[df.iloc[:, 0].astype(str).str.lower().eq("food_items")]
    end_t1 = food_start[0] if len(food_start) > 0 else len(df)
    table1 = df.iloc[:end_t1, 0:5].dropna(how="all")

    # --- Table 2 ---
    if len(food_start) > 0:
        start_t2 = food_start[0]
        after = df.iloc[start_t2+1:, 0].isna()
        end_t2 = after.idxmax() if after.any() else len(df)
        table2 = df.iloc[start_t2:end_t2, 0:4].dropna(how="all")
        table2.drop(table2.index[0], inplace=True)
    else:
        table2 = pd.DataFrame()

    if table2.shape[1] == 4:
        table2.columns = ['food_items', 'quantity', 'sum_g', 'sum_kg']

    # --- Animal & Summary ---
    animal = df.iloc[0:17, 7:12].dropna(how="all")
    animal.columns = ['food_items', 'quantity', 'sum_g', 'gwp_1kg', 'gwp_x_kg']
    summary = df.iloc[18:23, 7:12].dropna(how="all")
    summary.columns = ['variables', 'mass_kg', 'percent_kg', 'gwp_total', 'percent_gwp']

    # --- Animal_S & Summary_S ---
    animal_s = df.iloc[0:17, 14:19].dropna(how="all")
    animal_s.columns = animal.columns
    summary_s = df.iloc[18:23, 14:19].dropna(how="all")
    summary_s.columns = summary.columns

    return {
        "Table1": table1,
        "Table2": table2,
        "Animal": animal,
        "Summary": summary,
        "Animal_S": animal_s,
        "Summary_S": summary_s
    }

# === Load all months into memory ===
all_data = {m: extract_tables(m) for m in months}

# === Shiny UI ===
app_ui = ui.page_fluid(
    ui.h2("Hospital Food Emissions Dashboard"),
    ui.input_select("month", "Select Month:", {m: m for m in months}),
    ui.input_select("table", "Select Table:", 
        {"Table1": "Table 1", "Table2": "Table 2", 
         "Animal": "Animal", "Summary": "Summary", 
         "Animal_S": "Animal_S", "Summary_S": "Summary_S"}
    ),
    ui.output_data_frame("table_out")
)

# === Server logic ===
def server(input, output, session):
    @output
    @render.data_frame
    def table_out():
        selected_month = input.month()
        selected_table = input.table()
        df = all_data[selected_month][selected_table]
        return df

app = App(app_ui, server)
