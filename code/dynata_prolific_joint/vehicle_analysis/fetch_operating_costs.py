"""Pull EPA fuel economy for the combined_table vehicles from fueleconomy.gov and
convert it to cents per mile. Output: data/vehicle_operating_costs.csv"""
import json, urllib.request, urllib.parse, csv, statistics, pathlib

BASE = "https://www.fueleconomy.gov/ws/rest/"
def get(path, **params):
    url = BASE + path + ("?" + urllib.parse.urlencode(params) if params else "")
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    return json.load(urllib.request.urlopen(req))

def items(x):
    if not x: return []
    x = x["menuItem"]
    return x if isinstance(x, list) else [x]

prices = {k: float(v) for k, v in get("fuelprices").items()}

# (pair_id, make, model, powertrain, year, [fueleconomy.gov model names])
SPEC = [
 (1,"KIA","SOUL","CV",2022,["Soul"]),
 (1,"KIA","SOUL","BEV",2020,["Soul Electric"]),          # no 2021+ Soul EV in US
 (2,"HYUNDAI","KONA","CV",2022,["Kona FWD","Kona AWD"]),
 (2,"HYUNDAI","KONA EV","BEV",2022,["Kona Electric"]),
 (3,"MINI","COOPER","CV",2022,["Cooper Hardtop 2 door","Cooper S Hardtop 2 door"]),
 (3,"MINI","HARDTOP 2 DOOR","BEV",2022,["Cooper SE Hardtop 2 door"]),
 (4,"TOYOTA","CAMRY","CV",2022,["Camry LE/SE","Camry XLE/XSE","Camry AWD LE/SE","Camry AWD XLE/XSE","Camry XSE"]),
 (4,"TOYOTA","CAMRY","HEV",2022,["Camry Hybrid LE","Camry Hybrid SE/XLE/XSE"]),
 (5,"TOYOTA","COROLLA","CV",2022,["Corolla","Corolla XLE","Corolla XSE"]),
 (5,"TOYOTA","COROLLA","HEV",2022,["Corolla Hybrid"]),
 (6,"TOYOTA","RAV4","CV",2022,["RAV4","RAV4 AWD","RAV4 AWD LE"]),
 (6,"TOYOTA","RAV4","HEV",2022,["RAV4 Hybrid AWD"]),
 (7,"FORD","ESCAPE","CV",2022,["Escape FWD","Escape AWD"]),
 (7,"FORD","ESCAPE","HEV",2022,["Escape FWD HEV","Escape AWD HEV"]),
 (8,"NISSAN","VERSA NOTE","CV",2022,["Versa"]),           # Versa Note hatch ended 2019
 (8,"NISSAN","LEAF","BEV",2022,["Leaf (40 kW-hr battery pack)","Leaf (62 kW-hr battery pack)","Leaf SV/SL (62 kW-hr battery pack)"]),
 (9,"BMW","4 SERIES","CV",2022,["430i Coupe","430i Gran Coupe","430i xDrive Coupe"]),
 (9,"BMW","I4","BEV",2022,["i4 eDrive40 Gran Coupe (18 inch wheels)","i4 eDrive40 Gran Coupe (19 inch wheels)"]),
]

def cents_per_mile(v):
    t = v["fuelType1"]
    if t == "Electricity":
        return float(v["combE"]) / 100 * prices["electric"] * 100        # kWh/100mi -> cents/mi
    p = prices["premium"] if "Premium" in t else prices["midgrade"] if "Midgrade" in t else prices["regular"]
    return 100 * p / float(v["comb08"])                                   # $/gal / mpg -> cents/mi

rows = []
for pid, make, model, pt, yr, names in SPEC:
    recs = []
    for n in names:
        for o in items(get("vehicle/menu/options", year=yr, make=make.title() if make != "BMW" else "BMW", model=n)):
            v = get("vehicle/" + o["value"])
            recs.append(v)
    assert recs, (make, model, yr)
    cpm = [cents_per_mile(v) for v in recs]
    rows.append(dict(pair_id=pid, make=make, model=model, powertrain=pt, epa_year=yr, n_configs=len(recs),
        epa_models="; ".join(names), fuel=recs[0]["fuelType1"],
        comb_mpg_or_mpge=round(statistics.mean(float(v["comb08"]) for v in recs),1),
        kwh_per_100mi=round(statistics.mean(float(v["combE"]) for v in recs),1) if pt=="BEV" else "",
        epa_annual_fuel_cost=round(statistics.mean(float(v["fuelCost08"]) for v in recs)),
        cents_per_mile=round(statistics.mean(cpm),2),
        cents_per_mile_min=round(min(cpm),2), cents_per_mile_max=round(max(cpm),2)))

out = pathlib.Path(__file__).resolve().parents[3] / "data" / "vehicle_operating_costs.csv"
with open(out, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
print(prices)
for r in rows: print(r["pair_id"], r["make"], r["model"], r["powertrain"], r["epa_year"], r["n_configs"], r["fuel"], r["comb_mpg_or_mpge"], r["kwh_per_100mi"], r["epa_annual_fuel_cost"], r["cents_per_mile"], r["cents_per_mile_min"], r["cents_per_mile_max"])
