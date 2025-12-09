import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from shiny import App, render, ui

# === Configuration ===
file_path = "export_result_oct21.xlsx"
month_order = ["Dec23","Jan24","Feb24","Mar24","Apr24","May24",
               "Jun24","Jul24","Aug24","Sep24","Oct24","Nov24"]

all_foods = [
    "A_beef_consumed_ON","A_milk_consumed_ON",
    "Supp_EnsureChocolate_Water_WheyProtein_Sugar",
    "PuddingCup_Proxy_Milk_Eggs_Sugar_Consumed",
    "S_icecream_consumed_ON",
    "Supp_EnsureVanillaStrawberry_Water_SoyProtein_Milk",
    "Supp_BoostDrink_Water_Sugar_WheyProtein_Poore&Nemecek",
    "A_cheese_consumed_ON","A_egg_consumed_ON","A_chicken_consumed_ON",
    "Supp_Isosource+HN_MilkProtein_SoyProtein_Water_Sugar",
    "V_tomato_unheated_greenhouse_consumed_ON","A_yogurt_consumed_ON",
    "MagicCup_Proxy_Icecream ","F_banana_consumed_ON",
    "Supp_Glucerna_Water_Milk_SoyProtein","Muffin_Proxy_Bread_Sugar",
    "Turkey_Proxy_Chicken_Consumed","G_bread_consumed_ON",
    "Haddock_Bianchietal2022","Sorbet_Water_Sugar_Orange",
    "F_mandarin_consumed_ON","Apple_Oat_Sugar","G_pasta_consumed_ON",
    "Supp_Peptamen_TF","B_coffee_brewed_consumed_ON","Vegetables_MontegoBlend",
    "F_orange_juice_consumed_ON","Peptamen_Water_Sugar_WheyProtein",
    "G_oat_consumed_ON","A_butter_consumed_ON","Tuna_Bianchietal2022",
    "A_mixed_meat_consumed_ON","V_potato_consumed_ON",
    "Supp_Resource_Water_Milk_SoyProtein_Sugar","FrenchToast_Proxy_Eggs_Bread",
    "F_apple_consumed_ON","Cookies_Proxy_Bread+Sugar","Pizza_Flatbread_Cheese_Flatbred_Tomato",
    "V_lettuce_consumed_ON","Supp_NovaSource_Water_Milk_SoyProtein",
    "B_carbonated_drink_consumed_ON","G_rice_consumed_ON","V_pepper_consumed_ON",
    "PromoteVanilla_Water_SoyProtein_Sugar","S_candied_fruit_consumed_ON",
    "O_margarine_consumed_ON","V_carrot_consumed_ON","A_pork_consumed_ON",
    "PeachJuice_Proxy_OrangeJuice"
]

# === Helper functions ===
def extract_table1(sheet):
    df = pd.read_excel(file_path, sheet_name=sheet)
    food_start = df.index[df.iloc[:, 0].astype(str).str.lower().eq("food_items")]
    end_t1 = food_start[0] if len(food_start) > 0 else len(df)
    t = df.iloc[:end_t1, 0:5].dropna(how="all")
    t.columns = [str(c).strip().lower() for c in t.columns]
    if "food_items" not in t.columns:
        t.columns = t.iloc[0].astype(str).str.strip().str.lower()
        t = t.iloc[1:]
    return t

def clean_table1(df):
    if df is None or df.empty:
        return pd.DataFrame(columns=["food_items", "gwp_x_kg"])
    df = df.copy()
    df.columns = [str(c).strip().lower() for c in df.columns]
    if "food_items" not in df.columns or "gwp_x_kg" not in df.columns:
        return pd.DataFrame(columns=["food_items", "gwp_x_kg"])
    df["food_items"] = df["food_items"].astype(str).str.strip()
    df["gwp_x_kg"] = pd.to_numeric(df["gwp_x_kg"], errors="coerce")
    return df.groupby("food_items", as_index=False)["gwp_x_kg"].sum()

# === Preload data ===
all_tables = {}
for m in month_order:
    try:
        all_tables[m[:3].lower()] = extract_table1(m)
    except Exception as e:
        print(f"⚠️ Could not load {m}: {e}")

# === Modern and clean UI ===
app_ui = ui.page_fluid(
    ui.tags.style("""
        body { background-color: #f8fafc; font-family: 'Inter', sans-serif; }
        h2 { font-weight: 600; color: #1e293b; }
        .card { background: white; border-radius: 16px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); }
        .selectize-input { border-radius: 8px !important; }
    """),

    ui.h2("Hospital Food Emissions – GWP Plotter"),
    ui.layout_columns(
        ui.card(
            ui.input_selectize(
                "foods",
                "Select food items to display:",
                choices=all_foods,
                multiple=True,
                selected=[
                    "A_milk_consumed_ON",
                    "Supp_EnsureChocolate_Water_WheyProtein_Sugar",
                    "S_icecream_consumed_ON"
                ]
            ),
            ui.markdown(
                "<small>Select one or more items to compare their monthly GWP (kg CO₂e).</small>"
            ),
            class_="p-3"
        ),
    ),
    ui.layout_columns(
        ui.card(
            ui.output_plot("plot_gwp", width="100%", height="600px"),
            class_="p-3"
        ),
    )
)

# === Server logic ===
def server(input, output, session):

    @output
    @render.plot
    def plot_gwp():
        selected = input.foods()
        if not selected:
            fig, ax = plt.subplots()
            ax.text(0.5, 0.5, "Select at least one item", ha="center", va="center", fontsize=12)
            ax.axis("off")
            return fig

        records = []
        for m in month_order:
            df = clean_table1(all_tables.get(m[:3].lower()))
            for f in selected:
                val = df.loc[df["food_items"] == f.strip(), "gwp_x_kg"]
                gwp = float(val.iloc[0]) if len(val) else 0.0
                records.append({"food_items": f, "month": m[:3], "GWP": gwp})

        tidy = pd.DataFrame(records)
        if tidy.empty:
            fig, ax = plt.subplots()
            ax.text(0.5, 0.5, "No data found.", ha="center", va="center")
            ax.axis("off")
            return fig

        dfp = tidy.pivot(index="food_items", columns="month", values="GWP").reindex(index=selected)
        dfp = dfp.reindex(columns=[m[:3] for m in month_order]).fillna(0.0)

        x = np.arange(len(selected))
        bar_w = 0.06
        colors = plt.cm.tab20(np.linspace(0, 1, len(month_order)))

        fig, ax = plt.subplots(figsize=(14, 6))
        for i, m in enumerate([m[:3] for m in month_order]):
            ax.bar(x + i * bar_w, dfp[m].values, width=bar_w, color=colors[i], label=m)

        ax.set_xticks(x + bar_w * (len(month_order) / 2 - 0.5))
        ax.set_xticklabels(
            [f.replace("_consumed_ON", "").replace("Supp_", "") for f in selected],
            rotation=45, ha="right", fontsize=9
        )
        ax.set_ylabel("kg CO₂e", fontsize=12, weight="bold")
        ax.grid(axis="y", linestyle="--", alpha=0.7)
        ax.legend(title="Month", bbox_to_anchor=(1.02, 1), loc="upper left", frameon=False, fontsize=9)
        fig.tight_layout()
        return fig

app = App(app_ui, server)
