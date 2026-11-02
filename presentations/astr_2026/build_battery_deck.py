#!/usr/bin/env python3
"""Build the battery-paper slide deck from the vehicle-paper template.
Reuses the template's masters/layouts/theme (Arial, blue accent) for a consistent look,
clears the template slides, and rebuilds with battery-paper content and figures.
"""
from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR, MSO_AUTO_SIZE
from pptx.enum.shapes import MSO_SHAPE
from pptx.oxml.ns import qn
from PIL import Image
import os, copy

TEMPLATE = "/Users/xnw17/Documents/GitHub/Conjoint_Survey_25/presentations/astr_2026/Used Vehicle Preference Slides.pptx"
IMG = "/Users/xnw17/Documents/GitHub/Conjoint_Survey_25/paper_writing/battery paper/battery_paper_full_origin/images"
OUT = "/Users/xnw17/Documents/GitHub/Conjoint_Survey_25/presentations/astr_2026/Battery Health Preference Slides.pptx"

ACCENT = RGBColor(0x42, 0x85, 0xF4)   # template blue
DARK   = RGBColor(0x21, 0x21, 0x21)
GREY   = RGBColor(0x55, 0x55, 0x55)
TAKE_FILL = RGBColor(0xED, 0xF2, 0xFB)  # pale blue
WHITE  = RGBColor(0xFF, 0xFF, 0xFF)

SW, SH = Inches(10), Inches(5.625)
prs = Presentation(TEMPLATE)

# ---- layouts by name ----
LAY = {l.name: l for l in prs.slide_layouts}
TITLE   = LAY["TITLE"]
BODY    = LAY["TITLE_AND_BODY"]
TONLY   = LAY["TITLE_ONLY"]
SECTION = LAY["SECTION_HEADER"]

# ---- keep the original vehicle title slide (slide 1), delete the rest ----
sldIdLst = prs.slides._sldIdLst
for sldId in list(sldIdLst)[1:]:
    rId = sldId.get(qn("r:id"))
    prs.part.drop_rel(rId)
    sldIdLst.remove(sldId)

# content slides start numbering at 2 (title slide keeps its blank number, as in the vehicle deck)
PAGE = [1]
def set_pagenum(slide):
    PAGE[0] += 1
    for ph in slide.placeholders:
        if ph.placeholder_format.idx == 12:
            ph.text_frame.text = str(PAGE[0])
            return

def get_ph(slide, idx):
    for ph in slide.placeholders:
        if ph.placeholder_format.idx == idx:
            return ph
    return None

def style_title(ph):
    for p in ph.text_frame.paragraphs:
        for r in p.runs:
            r.font.bold = True

def add_bullets(ph, items, size=18, space=8):
    """items: str, (text, level), or a list of run-segments for one bullet.
    A run-segment is (text, opts) where opts may set 'bold' and 'link'."""
    tf = ph.text_frame
    tf.word_wrap = True
    tf.clear()
    for i, it in enumerate(items):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.space_after = Pt(space)
        if isinstance(it, list):          # mixed runs in a single level-0 bullet
            p.level = 0
            for seg_text, opts in it:
                r = p.add_run(); r.text = seg_text
                r.font.size = Pt(size)
                r.font.color.rgb = DARK
                if opts.get("bold"):
                    r.font.bold = True
                if opts.get("cite"):      # [n] citation marker
                    r.font.color.rgb = ACCENT
                    r.font.bold = True
                    r.font.size = Pt(size - 3)
            continue
        text, lvl = (it if isinstance(it, tuple) else (it, 0))
        p.level = lvl
        r = p.add_run(); r.text = text
        r.font.size = Pt(size - 3*lvl if lvl else size)
        r.font.color.rgb = DARK if lvl == 0 else GREY
    return ph

def takeaway(slide, text, top=None):
    """Write the one-sentence takeaway to the slide's speaker notes."""
    notes = slide.notes_slide.notes_text_frame
    notes.text = "presentation_notes: " + text

def notes(slide, lines):
    """Write detailed, multi-line speaker notes. `lines` is a list of strings;
    each becomes its own paragraph. A leading '•' / '–' indicates a bullet."""
    tf = slide.notes_slide.notes_text_frame
    tf.text = lines[0]
    for ln in lines[1:]:
        p = tf.add_paragraph()
        p.text = ln

def add_refs(slide, refs):
    """Numbered reference list ([1], [2], ...) in the bottom-left corner, 9pt grey."""
    tb = slide.shapes.add_textbox(Inches(0.34), Inches(5.02), Inches(8.6), Inches(0.55))
    tf = tb.text_frame; tf.word_wrap = True
    tf.margin_left = 0; tf.margin_top = 0; tf.margin_bottom = 0
    tf.vertical_anchor = MSO_ANCHOR.BOTTOM
    for i, ref in enumerate(refs):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.space_before = Pt(0); p.space_after = Pt(0)
        r = p.add_run(); r.text = f"[{i+1}] {ref}"
        r.font.size = Pt(9); r.font.color.rgb = GREY
    return tb

def fit_picture(slide, path, top, max_h, left=None, max_w=Inches(9.32)):
    w, h = Image.open(path).size
    ar = w / h
    width = max_w; height = int(width / ar)
    if height > max_h:
        height = max_h; width = int(height * ar)
    if left is None:
        left = int((SW - width) / 2)
    return slide.shapes.add_picture(path, left, top, width=width, height=height)

def label(slide, text, left, top, w, h, size=13, bold=True, color=None, align=PP_ALIGN.CENTER):
    tb = slide.shapes.add_textbox(left, top, w, h)
    tf = tb.text_frame; tf.word_wrap = True
    p = tf.paragraphs[0]; p.alignment = align
    r = p.add_run(); r.text = text
    r.font.size = Pt(size); r.font.bold = bold
    r.font.color.rgb = color or DARK
    return tb

# =========================================================================
# SLIDE 1 — Title (retained vehicle title slide; edit text only, keep its exact format)
# =========================================================================
s = prs.slides[0]
# --- title: replace the single run's text, keep its 47pt centered styling ---
title_sh = get_ph(s, 0)
title_sh.text_frame.paragraphs[0].runs[0].text = (
    "Consumer Valuation of Battery Health Attributes in Used BEV Markets")
# --- author block: rebuild paragraphs from the existing run templates ---
authors = [  # (name, email, affiliation-superscript)
    ("Xiatian Iogansen, Ph.D. ", "xiatian.iogansen@gwu.edu", "1,2"),
    ("Zain Hoda ",        "zain.hoda@gwu.edu",         "1"),
    ("Christina Gore, Ph.D.  ",   "christina.gore@nist.gov",   "2"),
    ("Joshua D. Kneifel, Ph.D.  ","joshua.kneifel@nist.gov",   "2"),
    ("John P. Helveston, Ph.D.  ","jph@gwu.edu",               "1"),
]
legend = ["1: George Washington University",
          "2: National Institute of Standards and Technology"]
auth_sh = get_ph(s, 1)
txBody = auth_sh.text_frame._txBody
paras = txBody.findall(qn("a:p"))
author_tmpl = copy.deepcopy(paras[0])   # name (email) superscript
blank_tmpl  = copy.deepcopy(paras[5])   # empty spacer line
legend_tmpl = copy.deepcopy(paras[6])   # single-run legend line
for p in paras:
    txBody.remove(p)
def set_run_text(elem, idx, text):
    r = elem.findall(qn("a:r"))[idx]
    r.find(qn("a:t")).text = text
for name, email, aff in authors:
    p = copy.deepcopy(author_tmpl)
    set_run_text(p, 0, name)    # name
    set_run_text(p, 2, email)   # email (keeps '(' and ')' runs 1 & 3)
    set_run_text(p, 4, aff)     # affiliation superscript
    txBody.append(p)
txBody.append(copy.deepcopy(blank_tmpl))
for line in legend:
    p = copy.deepcopy(legend_tmpl)
    set_run_text(p, 0, line)
    txBody.append(p)
# author lines and affiliation legend at 17pt
for para in auth_sh.text_frame.paragraphs:
    for run in para.runs:
        run.font.size = Pt(17)

notes(s, [
    "TRANSITION FROM THE VEHICLE PAPER:",
    "• That first study looked at the used-vehicle market broadly — how buyers trade off powertrain, price, age, and mileage across gas, hybrid, and electric.",
    "• One result there kept standing out: when a BEV is the option, something invisible is driving hesitation. That 'something' is the battery.",
    "• This second paper zooms all the way in on that one problem — the used BEV, and the one attribute buyers cannot see or verify: battery health.",
    "• Same survey instrument, same national sample of real used-vehicle intenders — but here every option in the choice set is a BEV, and we vary the battery attributes directly so we can put a dollar value on them.",
    "",
    "HOUSEKEEPING:",
    "• Co-authored with colleagues at George Washington University and NIST; the work was funded by NIST.",
    "• Plan for the next ~15 minutes: (1) why battery health is an information problem, (2) how we measured preferences with a choice experiment, and (3) four findings — the average buyer, six segments, whether extra info helps, and why people walk away.",
])

# =========================================================================
# SLIDE 2 — Motivation
# =========================================================================
s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Battery condition is the key uncertainty in used BEVs"
style_title(get_ph(s, 0))
add_bullets(get_ph(s, 1), [
    [("52% of US consumers are unlikely to consider purchasing a used EV, and nearly ", {}),
     ("two-thirds of them", {"bold": True}),
     (" cited battery health and range anxiety as the leading barrier", {}),
     (" [1]", {"cite": True})],
    [("The battery can be up to 30% of a used BEV's value", {}),
     (" [2]", {"cite": True}),
     (", yet its condition is hidden at the point of sale.", {})],
    "",
    "Result: an information and market-transparency problem. ",
    [(" Webb et al. (2025) finds that both current BEV owners and potential buyers view battery information, including expected lifetime and range, as useful for purchasing decisions.", {}),
     (" [3]", {"cite": True})],
], size=17, space=10)
add_refs(s, [
    "Fernandes, J. (2023). US: Understanding consumer opinion about used electric cars. YouGov.",
    "Boudway, I. (2020). Batteries for electric cars speed toward a tipping point. Bloomberg.",
    "Webb, D., Gore, C., & Kneifel, J. (2025). Consumer perspectives on battery electric vehicles. NIST TN 2328.",
])
notes(s, [
    "SET THE SCENE — the market is real and growing:",
    "• Used cars are about 70% of all US passenger-vehicle sales. Used BEVs are the fast-growing slice — sales up 34% year-over-year through early 2025, and supply is tightening.",
    "• So this isn't hypothetical — a used BEV market exists and it's where middle- and lower-income households get their first EV.",
    "",
    "THE DEMAND-SIDE PROBLEM:",
    "• But hesitation is strong: 52% of US adults are unlikely to consider a used BEV; only 28% are open to it.",
    "• The most-cited barriers are uncertainty about vehicle and battery condition (41%) and reliability versus a new EV (23%).",
    "• Here's the striking part — the fear is bigger than the reality. Among people who actually bought a used BEV, over half worried about battery life beforehand, but only about 5% ever saw major performance loss. That gap between perceived and actual risk is the signature of an information problem.",
    "",
    "WHY THE BATTERY SPECIFICALLY (the point to land):",
    "• On a gas car, odometer and age are decent proxies for wear. On a BEV they're not — degradation depends on charging history, depth of discharge, heat exposure, driving patterns.",
    "• Two BEVs, same age and mileage, can have very different remaining capacity. Most of that history is invisible to buyers — and sometimes even to the seller.",
    "• That's a classic lemons problem: sellers can't credibly prove a good battery, so healthy batteries get undervalued and weak ones overpriced. And the battery is up to 30% of the car's value — this is not a rounding error.",
    "• It even spills over to NEW cars: 'resale anxiety' about whether a BEV will hold value can suppress new BEV adoption too.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 3 — Research questions
# =========================================================================
s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Research questions"
style_title(get_ph(s, 0))
add_bullets(get_ph(s, 1), [
    "1.  What are consumers willing to pay (WTP) for battery-health attributes in used BEVs (range, mileage, degradation rate, and refurbishment history)?",
    "2.  How do preferences and willingness to pay for these attributes vary across consumers? Are there distinct market segments?",
    "3.  How does extra information about battery replacement costs and warranty coverage affect consumers’ choices of used BEVs?",
    "4.  What factors discourage consumers from considering a used BEV?"
], size=19, space=16)
notes(s, [
    "Four questions, each tied to a decision someone actually faces:",
    "• Q1 is the core measurement question — what is battery health worth in dollars? Not 'do people care,' but how many dollars of purchase price a buyer will trade for more range, lower mileage, slower degradation, or an un-refurbished pack.",
    "• Q2 asks whether there's really one 'consumer' or many. If preferences split into distinct segments, a single pricing or disclosure strategy won't fit the market.",
    "• Q3 is a direct policy lever — if we simply hand buyers more information about replacement cost and warranty, does their behavior actually change?",
    "• Q4 is about the people who walk away entirely — what's keeping them out of the market?",
    "",
    "THE POSITIONING (say this):",
    "• Prior work — Webb and colleagues, 2025 — shows consumers SAY battery information is useful. But stated usefulness is not willingness to pay. People say a lot of things are 'useful.'",
    "• Our contribution: the first dollar-valued WTP estimates for battery-health attributes in the US used BEV market — and critically, conditional on buyers actually being able to see those attributes. We make the invisible visible, then watch what they pay.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 5 — Discrete choice experiment & Attributes table 
# =========================================================================
s = prs.slides.add_slide(TONLY)
get_ph(s, 0).text_frame.text = "Attributes in the Discrete choice experiment"
style_title(get_ph(s, 0))
rows = [
    ("Attribute", "Levels shown to respondents"),
    ("Purchase price", "$10,000 – $45,000 (varies by vehicle type & budget)"),
    ("Mileage", "15,000 – 60,000 miles"),
    ("Electric range (at delivery)", "50 – 350 miles on a full charge"),
    ("Battery degradation rate", "1% – 8% annual loss of range / state of health"),
    ("Battery refurbishment history", "Original  |  Some cells replaced  |  Entire pack replaced"),
]
tbl_w, tbl_h = Inches(9.0), Inches(3.1)
gt = s.shapes.add_table(len(rows), 2, Inches(0.5), Inches(1.35), tbl_w, tbl_h).table
gt.columns[0].width = Inches(3.2); gt.columns[1].width = Inches(5.8)
for ri, (a, b) in enumerate(rows):
    for ci, txt in enumerate((a, b)):
        cell = gt.cell(ri, ci)
        cell.margin_top = Inches(0.03); cell.margin_bottom = Inches(0.03)
        cell.vertical_anchor = MSO_ANCHOR.MIDDLE
        p = cell.text_frame.paragraphs[0]
        r = p.add_run(); r.text = txt
        r.font.size = Pt(15)
        if ri == 0:
            r.font.bold = True; r.font.color.rgb = WHITE
            cell.fill.solid(); cell.fill.fore_color.rgb = ACCENT
        else:
            r.font.color.rgb = DARK
            if ci == 0: r.font.bold = True
            cell.fill.solid()
            cell.fill.fore_color.rgb = RGBColor(0xF2,0xF5,0xFB) if ri % 2 else WHITE
notes(s, [
    "HOW A CHOICE EXPERIMENT WORKS (one line):",
    "• We never ask 'how much do you value range?' — people are terrible at answering that directly. Instead we force trade-offs between realistic vehicles and infer the dollar value from the choices they make.",
    "",
    "WALK THE ATTRIBUTES:",
    "• Price, mileage, and electric range are the familiar levers. The ranges here aren't arbitrary — they mirror the actual used-BEV fleet on the road today.",
    "• Battery degradation rate — how fast range is lost per year — is the attribute that's usually invisible. We put it on the table explicitly.",
    "• Refurbishment history is the novel one: original pack, some cells replaced, or the whole pack replaced. Buyers almost never see this described in a listing, and as you'll see, it carries a lot of signal.",
    "• By shuffling these five attributes across options and across six tasks, each person's choices trace out their personal trade-off curve.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 6 — Information treatment
# =========================================================================
_SURV_IMG = "/Users/xnw17/Documents/GitHub/Conjoint_Survey_25/survey_prolific/images"
_GREEN    = RGBColor(0x1B, 0x87, 0x54)

s = prs.slides.add_slide(TONLY)
get_ph(s, 0).text_frame.text = "Information treatment: randomly assigned"
style_title(get_ph(s, 0))

_PNL_TOP = Inches(1.05)
_HDR_H   = Inches(0.38)
_ICO_SZ  = Inches(0.44)
_ROW_H   = Inches(1.22)
_CTOP    = _PNL_TOP + _HDR_H + Inches(0.12)

def _panel_hdr(slide, left, top, width, text, color):
    tb = slide.shapes.add_textbox(left, top, width, _HDR_H)
    tf = tb.text_frame; tf.vertical_anchor = MSO_ANCHOR.MIDDLE
    p = tf.paragraphs[0]
    r = p.add_run(); r.text = text
    r.font.size = Pt(13); r.font.bold = True; r.font.color.rgb = color

def _info_row(slide, left, top, width, ico, title, bullets, tsz=13, bsz=11):
    slide.shapes.add_picture(ico, left + Inches(0.06), top + Inches(0.02), _ICO_SZ, _ICO_SZ)
    tb = slide.shapes.add_textbox(left + Inches(0.58), top, width - Inches(0.64), _ROW_H)
    tf = tb.text_frame; tf.word_wrap = True
    p = tf.paragraphs[0]
    r = p.add_run(); r.text = title
    r.font.size = Pt(tsz); r.font.bold = True; r.font.color.rgb = DARK
    for b in bullets:
        p2 = tf.add_paragraph()
        r2 = p2.add_run(); r2.text = b
        r2.font.size = Pt(bsz); r2.font.color.rgb = GREY
        p2.space_before = Pt(2); p2.space_after = Pt(0)

# vertical divider
_div = s.shapes.add_shape(MSO_SHAPE.ROUNDED_RECTANGLE, Inches(4.78), _PNL_TOP, Inches(0.02), Inches(4.1))
_div.fill.solid(); _div.fill.fore_color.rgb = RGBColor(0xCC, 0xCC, 0xCC)
_div.line.fill.background(); _div.shadow.inherit = False

# --- Short panel (left) ---
_panel_hdr(s, Inches(0.3), _PNL_TOP, Inches(4.3),
           "Short treatment  (50% of respondents)", ACCENT)
_info_row(s, Inches(0.3), _CTOP, Inches(4.3),
    os.path.join(_SURV_IMG, "battery_survey_battery.png"),
    "Vehicle Driving Range & Battery Health",
    ["EV batteries gradually lose capacity over time (battery degradation), "
     "reducing driving range and increasing charging time.",
     "Battery condition is measured as State of Health (SOH, %)."],
    tsz=13, bsz=12)

# --- Long panel (right) ---
_panel_hdr(s, Inches(4.95), _PNL_TOP, Inches(4.7),
           "Long treatment  (50% of respondents)", _GREEN)
_long_rows = [
    (os.path.join(_SURV_IMG, "battery_survey_battery.png"),
     "Vehicle Driving Range & Battery Health",
     ["Batteries degrade over time, reducing range.",
      "Condition measured as State of Health (SOH, %)."]),
    (os.path.join(_SURV_IMG, "battery_survey_veh_maintenance.jpg"),
     "Battery Maintenance",
     ["Replacements cost $5,000–$20,000 (rare).",
      "Most batteries last the vehicle's lifetime under normal use."]),
    (os.path.join(_SURV_IMG, "battery_survey_veh_warranty.png"),
     "Battery Warranty",
     ["Warranted for 8 years / 100,000 miles.",
      "Free replacement if SOH drops below 70–75% within warranty."]),
]
_row_y = _CTOP
for _ico, _ttl, _bts in _long_rows:
    _info_row(s, Inches(4.95), _row_y, Inches(4.7), _ico, _ttl, _bts, tsz=12, bsz=10)
    _row_y += _ROW_H

notes(s, [
    "THIS IS AN EXPERIMENT INSIDE THE EXPERIMENT:",
    "• Before the battery choices, everyone read a short education page. But we randomly split the sample.",
    "• The SHORT version (left) covered only the basics: batteries degrade over time, and condition is summarized as State of Health, a percentage.",
    "• The LONG version (right) added two reassuring facts: replacement is expensive ($5,000–$20,000) but rare, and BEVs carry a warranty of at least 8 years / 100,000 miles, with free replacement if health drops below ~70–75%.",
    "",
    "WHY WE DID THIS (the hypothesis to tee up):",
    "• The intuition is that more reassurance should calm buyers — fewer walk-aways, maybe more tolerance of degradation or refurbishment.",
    "• It's also a cheap, scalable policy lever: if a paragraph of warranty text moves behavior, that's an easy win for dealers and regulators.",
    "• Hold that thought — Finding 3 tells us whether it actually worked. (Spoiler: the answer is instructive.)",
])
set_pagenum(s)

# =========================================================================
# SLIDE 7 — Example choice question (Figure 1)
# =========================================================================

s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Example choice question"
style_title(get_ph(s, 0))
fit_picture(s, os.path.join(IMG, "battery-1.png"), Inches(1.2), Inches(3.6))
notes(s, [
    "This is exactly what a respondent saw on screen:",
    "• Three used BEVs side by side, each with its own price, mileage, range, degradation, and refurbishment status — plus a fourth option: 'none of these.'",
    "• Everyone did this six times, with different combinations drawn from the experimental design, so no two people see the same sequence.",
    "",
    "WHY THE 'NONE' OPTION MATTERS (don't skip this):",
    "• It's not just realism. The opt-out is how we measure disengagement. Forcing a choice would hide the people the market is losing.",
    "• Who opts out, how often, and why becomes a finding in its own right — that's Finding 4 at the end.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 7 — Sample & data (map)
# =========================================================================
s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Sample: 3,072 used-vehicle buyers, nationwide"
style_title(get_ph(s, 0))
# shrink body placeholder to left, map on right
bp = get_ph(s, 1)
bp.left = Inches(0.34); bp.top = Inches(1.3); bp.width = Inches(3.5); bp.height = Inches(3.4)
add_bullets(bp, [
    "National U.S. sample of adults intending to buy a used car or SUV within two years.",
    "Collected online via two panels (Dynata and Prolific).",
    "Broadly tracks the U.S. population on age, race, and income.",
    "6 choice tasks per respondent.",
], size=15, space=8)
fit_picture(s, os.path.join(IMG, "map_all.png"), Inches(1.35), Inches(3.3),
            left=Inches(4.0), max_w=Inches(5.9))
notes(s, [
    "Who's in the sample — and why it's the right sample:",
    "• 3,072 US adults, but not the general public. Every respondent told us they intend to buy a used car or SUV within the next two years.",
    "• That screening matters: these are the people a used-BEV market would actually draw from — the addressable market, not passers-by. It makes the WTP numbers more decision-relevant.",
    "• Recruited through two online panels, Dynata and Prolific, and the sample broadly tracks the US population on age, race, and income.",
    "• The map shows the geographic spread — coverage across the country, not one region.",
    "• Each person completed six battery choice tasks, so we're working with roughly 18,000 choice observations.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 8 — Modeling in plain language (Figure 3)
# =========================================================================
s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Method: Logit Model and Latent Class Choice Model"
style_title(get_ph(s, 0))
bp = get_ph(s, 1)
bp.left = Inches(0.34); bp.top = Inches(1.25); bp.width = Inches(4.0); bp.height = Inches(3.5)
add_bullets(bp, [
    ("Mixed logit model — the average buyer", 0),
    ("Estimate the market-wide mean WTP for each attribute and the extent to which preferences vary across consumers.**.", 1),
    ("Latent class choice model— hidden segments", 0),
    ("Sorts consumers into groups with similar tastes, then explains each group using attitudes, charging access, and demographics.", 1),
], size=15, space=7)
fit_picture(s, os.path.join(IMG, "model_framework-1.png"), Inches(1.9), Inches(2.9),
            left=Inches(4.45), max_w=Inches(5.4))
notes(s, [
    "Two models, and I'll keep the stats light:",
    "",
    "MIXED LOGIT — the average buyer:",
    "• Gives us one dollar figure per attribute for the whole market, PLUS a measure of how much people differ around that average.",
    "• The dollar value is just the attribute's weight divided by the price weight — that ratio converts abstract 'utility' into money.",
    "",
    "WHY WE DON'T STOP THERE (the key intuition):",
    "• An average can be a lie. If half the market loves range and half ignores it, the average looks 'moderate' — describing nobody. The mixed logit told us the spread was huge, which is a red flag that averages are hiding structure.",
    "",
    "LATENT CLASS MODEL — the hidden segments:",
    "• It sorts consumers into groups that choose alike, then explains who's in each group using their attitudes, charging access, and demographics.",
    "• So instead of one number, we get a map of distinct buyer types. That map is Findings 2.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 9 — Key finding 1: average WTP (number callouts)
# =========================================================================
s = prs.slides.add_slide(TONLY)
get_ph(s, 0).text_frame.text = "Key finding 1 — what the average buyer pays for battery health"
style_title(get_ph(s, 0))
cards = [
    ("+$11,070", "per +100 miles\nof electric range", "SD ±$13,950", ACCENT),
    ("−$2,780", "per +10,000\nmiles driven", "SD ±$4,020", RGBColor(0xC6,0x28,0x28)),
    ("−$980", "per +1 pt / yr\nbattery degradation", "SD ±$770", RGBColor(0xC6,0x28,0x28)),
    ("−$4,060", "pack-level\nrefurbishment", "SD ±$7,300", RGBColor(0xC6,0x28,0x28)),
    ("−$4,480", "cell-level\nrefurbishment", "SD ±$6,720", RGBColor(0xC6,0x28,0x28)),
]
n = len(cards); cw = Inches(1.72); gap = Inches(0.1)
total = n*cw + (n-1)*gap
x0 = int((SW - total)/2); y0 = Inches(1.65); chh = Inches(2.05)
for i,(big,small,sd,col) in enumerate(cards):
    x = int(x0 + i*(cw+gap))
    card = s.shapes.add_shape(MSO_SHAPE.ROUNDED_RECTANGLE, x, y0, cw, chh)
    card.fill.solid(); card.fill.fore_color.rgb = RGBColor(0xF5,0xF7,0xFB)
    card.line.color.rgb = col; card.line.width = Pt(1.5); card.shadow.inherit=False
    tf = card.text_frame; tf.word_wrap=True; tf.vertical_anchor=MSO_ANCHOR.MIDDLE
    p=tf.paragraphs[0]; p.alignment=PP_ALIGN.CENTER
    r=p.add_run(); r.text=big; r.font.size=Pt(21); r.font.bold=True; r.font.color.rgb=col
    p2=tf.add_paragraph(); p2.alignment=PP_ALIGN.CENTER
    r2=p2.add_run(); r2.text=small; r2.font.size=Pt(12); r2.font.color.rgb=DARK
    p3=tf.add_paragraph(); p3.alignment=PP_ALIGN.CENTER; p3.space_before=Pt(3)
    r3=p3.add_run(); r3.text=sd; r3.font.size=Pt(10); r3.font.italic=True; r3.font.color.rgb=GREY
label(s, "Mixed logit WTP estimates are statistically significant, with substantial variation across consumers, indicating considerable disagreement in how buyers value each attribute.",
      Inches(0.5), Inches(3.9), Inches(9.0), Inches(0.7), size=14, bold=False, color=GREY)
notes(s, [
    "FINDING 1 — the average buyer, in four numbers:",
    "• Range is the biggest lever: +$11,070 for every extra 100 miles. People pay a lot for range in a short-range used fleet.",
    "• Mileage: about -$2,780 per extra 10,000 miles — the familiar used-car discount, nothing surprising.",
    "• Degradation: -$980 for each percentage point of annual range loss. So buyers DO penalize a fading battery — the invisible attribute has a price once we make it visible.",
    "• Refurbishment reads as a red flag, not a fresh start: about -$4,060 for a replaced pack and -$4,480 for replaced cells. A new battery makes buyers suspicious, not reassured.",
    "",
    "THE REAL POINT OF THIS SLIDE (the setup for Finding 2):",
    "• These averages are statistically solid, but the spread around them is enormous. The standard deviation on range WTP is about $13,950 — larger than the mean itself.",
    "• And for refurbishment, roughly a quarter of buyers actually view it as neutral or even a positive quality signal — a newer battery.",
    "• So 'the average buyer' is a composite that may not exist. That disagreement is the door into the segment analysis.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 10 — Six-segment pie chart
# =========================================================================
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np

_SCRATCH  = "/private/tmp/claude-81040/-Users-xnw17-Documents-GitHub-Conjoint-Survey-25/f8cb252c-eca1-45bf-bb7c-23bb6533a23e/scratchpad"
_pie_path = os.path.join(_SCRATCH, "segment_pie.png")

_sizes  = [22.7, 20.8, 16.6, 12.8, 8.7, 18.5]
_names  = [
    "Battery Health Attentives",
    "Multi-Attribute Maximizers",
    "Range-Focused, EV-Knowledgeable",
    "Budget-Constrained, Low-WTP",
    "Opt-Out Dominant, BEV-Skeptical",
    "Low-Engagement, Attr. Non-Attendance",
]
_pcts = ["22.7%", "20.8%", "16.6%", "12.8%", "8.7%", "18.5%"]
_ns   = [661, 606, 485, 373, 252, 539]
# Blues = sensitive (1–3, left); Oranges = non-sensitive (4–6, right)
_colors = ["#1565C0", "#1976D2", "#90CAF9",
           "#BF360C", "#E64A19", "#FFAB91"]

# startangle=72 places sensitive classes (60.1%) centred on the LEFT half
_START = 72
_SUM_S = sum(_sizes[:3]) / 100 * 360   # 216.36° — sensitive span (left)
_SUM_N = sum(_sizes[3:]) / 100 * 360   # 143.64° — non-sensitive span (right)
_SENS_END = _START + _SUM_S            # 288.36°

fig, ax = plt.subplots(figsize=(11, 5.6), facecolor="white")
# narrow pie column, wide label columns on each side
ax.set_position([0.30, 0.05, 0.40, 0.90])

wedges, _ = ax.pie(
    _sizes, colors=_colors, startangle=_START,
    wedgeprops=dict(linewidth=2.0, edgecolor="white"),
    radius=1.0,
)
ax.set_aspect("equal")

# ── grouping arcs just outside the pie ──────────────────────────────────
_R_ARC = 1.10
# sensitive arc (left hemisphere)
_arc_s = mpatches.Arc((0, 0), 2*_R_ARC, 2*_R_ARC, angle=0,
                       theta1=_START, theta2=_SENS_END,
                       color="#1565C0", lw=4.5, zorder=5)
ax.add_patch(_arc_s)
# non-sensitive arc (right hemisphere)
_arc_n = mpatches.Arc((0, 0), 2*_R_ARC, 2*_R_ARC, angle=0,
                       theta1=_SENS_END, theta2=_START + 360,
                       color="#BF360C", lw=4.5, zorder=5)
ax.add_patch(_arc_n)

# ── leader lines + labels (fixed, evenly-spaced slots so nothing overlaps) ─
_X_LEFT  = -1.98    # text anchor x for sensitive labels (left)
_X_RIGHT =  1.98    # text anchor x for non-sensitive labels (right)
_X_KNEE  = 1.36     # elbow x (mirrored by side)
_SLOTS   = [0.92, 0.0, -0.92]   # top, middle, bottom

_info = []
for i, (wedge, name, pct, n_) in enumerate(zip(wedges, _names, _pcts, _ns)):
    mid = np.radians((wedge.theta1 + wedge.theta2) / 2)
    _info.append(dict(name=name, pct=pct, n=n_,
                      x=np.cos(mid), y=np.sin(mid), sensitive=(i < 3)))

for sens in (True, False):
    members = sorted([d for d in _info if d["sensitive"] == sens],
                     key=lambda d: d["y"], reverse=True)   # top → bottom
    xt   = _X_LEFT if sens else _X_RIGHT
    xkn  = -_X_KNEE if sens else _X_KNEE
    ha   = "right"  if sens else "left"
    for slot, d in zip(_SLOTS, members):
        ax.plot([d["x"], xkn, xt * 0.98],
                [d["y"], slot, slot],
                color="#999999", lw=0.9, solid_capstyle="round", zorder=4)
        ax.text(xt, slot + 0.055, d["name"],
                ha=ha, va="bottom", fontsize=8.4, color="#212121", fontfamily="Arial")
        ax.text(xt, slot - 0.05, f'{d["pct"]}  (n={d["n"]:,})',
                ha=ha, va="top",    fontsize=7.6, color="#777777", fontfamily="Arial")

# ── group title labels ───────────────────────────────────────────────────
ax.text(_X_LEFT,  1.58, "Battery-sensitive\nSegments",
        ha="right", va="top", fontsize=9.5, fontweight="bold",
        color="#1565C0", fontfamily="Arial")
ax.text(_X_RIGHT, 1.58, "Non-sensitive\nSegments",
        ha="left",  va="top", fontsize=9.5, fontweight="bold",
        color="#BF360C", fontfamily="Arial")

ax.set_xlim(-2.85, 2.85)
ax.set_ylim(-1.45, 1.78)
plt.savefig(_pie_path, dpi=180, bbox_inches="tight", facecolor="white")
plt.close(fig)

s = prs.slides.add_slide(TONLY)
get_ph(s, 0).text_frame.text = "Six consumer segments identified"
style_title(get_ph(s, 0))
fit_picture(s, _pie_path, Inches(0.85), Inches(4.4), max_w=Inches(9.5))
notes(s, [
    "FINDING 2 — the disagreement has structure. Six segments.",
    "• The point to make: the variation from the last slide isn't random noise — it sorts cleanly into six distinct buyer types.",
    "",
    "THE BIG SPLIT (the headline of this slide):",
    "• Left, in blue — the battery-sensitive segments, about 60% of the market. These buyers' choices actually respond to battery health and range. They are the audience for any disclosure or certification strategy.",
    "• Right, in orange — the other 40%: price-constrained or disengaged buyers who largely ignore battery signals. Some will buy regardless of battery; others won't buy at all.",
    "",
    "WHY IT MATTERS:",
    "• One message won't reach everyone. A battery-health premium only works on the blue 60%. For the orange 40%, the levers are price and basic access, not certification.",
    "• I'll walk through each of the six on the next slides — one per class — showing what they pay for and who they are.",
])
set_pagenum(s)

# =========================================================================
# SLIDES 12–17 — Key finding 2: one slide per class (WTP left, characteristics right)
# =========================================================================
# WTP values ($000) and significance, keyed by internal class id (from
# 5_visualize_lc6_profiles_no_optout.R). Attribute order:
#   mileage, range_pw1-3, loss_pw1-3, pack, cell
_WTP = {
    "c1": [-3.22, 0.78, 12.84, 9.18, -1.62, -0.30, 0.24, -6.35, -7.32],
    "c2": [-2.93, 4.57, 4.61, 5.31, -2.31, -2.33, -2.14, -2.74, -3.76],
    "c3": [-2.09, 38.70, 20.82, 25.47, -0.87, -0.69, -0.47, -0.54, -0.67],
    "c4": [-9.95, 12.31, -10.06, 13.76, 0.26, -1.26, -1.38, -26.64, -32.62],
    "c5": [-1.82, 11.83, 9.45, 9.50, -0.78, -0.76, -0.39, -5.76, -6.07],
    "c6": [-0.57, 2.81, 1.76, 1.98, -0.21, -0.17, -0.17, 0.10, 0.27],
}
_SIG = {
    "c1": [".", "", "", "*", "*", "", "", "*", "*"],
    "c2": ["***", ".", ".", "**", "***", "***", "***", "*", "**"],
    "c3": ["***", "***", "***", "***", "***", "***", "***", "", ""],
    "c4": ["**", "", "", ".", "", ".", "*", "**", "**"],
    "c5": ["***", "***", "***", "***", "***", "***", "*", "***", "***"],
    "c6": ["***", "**", "*", "", "**", "***", "***", "", ""],
}
# simplified attribute labels, top → bottom
_ATTR_LABELS = [
    "Mileage (per 10k mi)",
    "Range: 40-130mi (per 100mi)",
    "Range: 130-200mi (per 100mi)",
    "Range: 200+mi (per 100mi)",
    "Range Loss Rate: 5-12% (per %)",
    "Range Loss Rate: 12-24% (per %)",
    "Range Loss Rate: 24+% (per %)",
    "Refurbishment: Pack Replace",
    "Refurbishment: Cell Replace",
]
# vertical positions (top→bottom); larger gap between attribute groups
_ATTR_Y = [0.0, -1.6, -2.6, -3.6, -5.2, -6.2, -7.2, -8.8, -9.8]

# class characteristics text (from the R script's class_chars)
_CHARS = {
    "c1": ["3/4 opts out of all 6 choice tasks", "Oldest average age; least risk-taking",
           "Highest range anxiety; lowest climate concern", "Most negative attitude toward EV batteries",
           "Predominantly ICEV-only; lowest BEV ownership", "Lowest electric outlet access",
           "Highest SUV preference"],
    "c2": ["Largest class", "High and almost consistent loss aversion",
           "High concern about EV battery functionality", "Steady marginal returns to additional range",
           "High SUV preference"],
    "c3": ["Highest primary vehicle range", "High share of current BEV owners",
           "Most positive toward EV batteries", "Highest electrical outlet access",
           "High EV exposure; most EV knowledge", "High used-BEV purchase intention",
           "Highest income; lowest SUV preference"],
    "c4": ["Strong opt-out aversion, yet attribute-disengaged", "Smallest price coefficient; WTPs ratio-inflated",
           "Highest share of information treatment", "Refurbishment-averse",
           "High concern about EV battery functionality", "Lowest ICEV-only; highest BEV households",
           "Highest vehicle budget; most risk-taking"],
    "c5": ["Only 5.2% never opt out", "High concern about EV battery functionality",
           "High range anxiety", "Strongly refurbishment-averse",
           "Higher premium on minimum viable range", "Comprehensive evaluation across all attributes",
           "Median household income and vehicle budget"],
    "c6": ["Most price-sensitive; small WTPs", "Lowest household income",
           "Accepting of low-range BEVs", "Range loss-averse",
           "Lowest vehicle ownership (mostly ICEVs)", "Lowest share of homeowners",
           "Highest share of apartment renters"],
}
# slide order = paper class numbering: new1..new6
_CLASS_ORDER = [
    ("c2", 1, 661, 23, "Battery Health Attentives",              True),
    ("c5", 2, 606, 21, "Multi-Attribute Maximizers",             True),
    ("c3", 3, 485, 17, "Range-Focused, EV-Knowledgeable",        True),
    ("c6", 4, 373, 13, "Budget-Constrained, Low WTP",            False),
    ("c1", 5, 252,  9, "Opt-Out Dominant, BEV-Skeptical",        False),
    ("c4", 6, 539, 18, "Low-Engagement, Attribute Non-Attendance", False),
]
_COL_POS = "#2A9D8F"   # teal — positive WTP
_COL_NEG = "#E07B54"   # coral — negative WTP

# per-class speaker notes (oral style), keyed by internal class id
_CLASS_NOTES = {
    "c2": [  # Class 1 — Battery Health Attentives
        "CLASS 1 — the largest group, nearly a quarter of the market.",
        "• Read the chart: the range bars are modest, but the degradation (range-loss) penalties are the strongest and most uniform of any class — about $2,300 per percentage point across the board.",
        "• The story: these people aren't buying range, they're buying reliability. They want a battery that won't fade. They also dislike both kinds of refurbishment.",
        "• Who they are: well-informed, high BEV knowledge, and they lean toward SUVs.",
        "• BUSINESS IMPLICATION: this is the prime audience for certified State-of-Health readings and degradation forecasts in the listing. Financing or warranty terms tied to a verified SOH would land here.",
    ],
    "c5": [  # Class 2 — Multi-Attribute Maximizers
        "CLASS 2 — the all-at-once evaluators.",
        "• Read the chart: everything is significant — range, degradation, AND refurbishment all carry weight. No single attribute dominates.",
        "• The behavior that defines them: they opt out selectively. When no car on the screen clears their bar on every dimension at once, they walk rather than settle for the least-bad option. Only about 5% never opt out.",
        "• They also show high range anxiety and are strongly refurbishment-averse.",
        "• BUSINESS IMPLICATION: clear, multi-dimensional disclosure — range, degradation, and refurbishment shown together — is what keeps them in the game. A car that's strong on one axis but silent on another loses them.",
    ],
    "c3": [  # Class 3 — Range-Focused, EV-Knowledgeable
        "CLASS 3 — the big bars. These are the range maximizers.",
        "• Read the chart: an exceptional premium — about $38,700 — for crossing the minimum viable range threshold at the low end, then it falls off. Marginal range beyond ~130 miles adds little. They're paying to escape 'too short,' not for endless range.",
        "• Who they are: the most EV-knowledgeable segment, highest income, most charging access, and the most POSITIVE toward refurbished batteries — refurbishment barely registers for them.",
        "• CAVEAT to say out loud: these stated range values are very high — above new-vehicle benchmarks — so read Class 3's range numbers with some caution. Direction is solid; the exact magnitude may be inflated by the stated-preference setting.",
        "• BUSINESS IMPLICATION: lead with range and performance for this group; battery history is not their concern.",
    ],
    "c6": [  # Class 4 — Budget-Constrained, Low WTP
        "CLASS 4 — the price-driven buyers.",
        "• Read the chart: bars are small because this is the most price-sensitive class by far — their price coefficient is more than twice any other class, which compresses every dollar value.",
        "• They place modest but real value on range and are basically indifferent to battery history.",
        "• Who they are: lowest income, most renters, fewest household vehicles — but here's the key: about 91% NEVER opt out. They are committed to buying; price just dominates everything else.",
        "• BUSINESS IMPLICATION: this segment sustains demand for affordable used BEVs. Battery certification won't move them much — affordability does. A refurbished, cheaper car is a feature, not a flaw, here.",
    ],
    "c1": [  # Class 5 — Opt-Out Dominant, BEV-Skeptical
        "CLASS 5 — the skeptics who mostly walk away.",
        "• The defining feature isn't a bar — it's behavior. Three-quarters of this class opted out of all six tasks. They're largely outside the addressable used-BEV market today.",
        "• Who they are: low BEV knowledge, the highest range anxiety, most negative attitudes toward EV batteries, predominantly ICEV-only households, lowest charging access, oldest and least risk-taking.",
        "• Treat their WTP numbers lightly — they rarely chose a vehicle, so the estimates are imprecise.",
        "• BUSINESS IMPLICATION: information alone won't convert this group in the near term. They're a long-game audience — broader EV familiarity and charging access have to come first.",
    ],
    "c4": [  # Class 6 — Low-Engagement, Attribute Non-Attendance
        "CLASS 6 — a methodological caution, not a marketing target.",
        "• Be upfront: this class has a near-zero price sensitivity, which mechanically inflates every WTP ratio — dividing by a tiny price coefficient blows the dollar values up. The eye-popping refurbishment numbers here are artifacts, not real valuations.",
        "• In the choice-modeling literature this is 'attribute non-attendance' — people who didn't really trade off the attributes. The latent class model usefully absorbs them into their own class so they don't distort the others.",
        "• So we FLAG this class as unreliable and don't interpret it substantively. I'm showing it for completeness and transparency.",
        "• The honest takeaway: keeping this group separate is exactly what makes the other five classes trustworthy.",
    ],
}

# single shared x-scale across ALL class charts so bar length is directly
# comparable slide-to-slide (same dollar amount → same bar length everywhere)
_GLOBAL_VMAX = max(abs(v) for vs in _WTP.values() for v in vs)   # 38.70 ($38,700)
_XLIM = _GLOBAL_VMAX * 1.78   # headroom for value labels at the widest bars

def _make_wtp_chart(cid, path):
    vals = _WTP[cid]; sigs = _SIG[cid]
    fig, ax = plt.subplots(figsize=(6.7, 4.6), facecolor="white")
    ax.set_position([0.44, 0.04, 0.54, 0.92])
    colors = [_COL_POS if v >= 0 else _COL_NEG for v in vals]
    ax.barh(_ATTR_Y, vals, height=0.9, color=colors, zorder=3)
    ax.axvline(0, color="#777777", lw=1.0, zorder=4)

    ax.set_xlim(-_XLIM, _XLIM)                 # identical on every slide
    off = _GLOBAL_VMAX * 0.018
    for y, v, sg in zip(_ATTR_Y, vals, sigs):
        dollars = round(abs(v) * 1000)
        lbl = (f"${dollars:,}" if v >= 0 else f"-${dollars:,}") + sg
        if v >= 0:
            ax.text(v + off, y, lbl, ha="left", va="center", clip_on=False,
                    fontsize=9, color="#1A202C", fontfamily="Arial")
        else:
            ax.text(v - off, y, lbl, ha="right", va="center", clip_on=False,
                    fontsize=9, color="#1A202C", fontfamily="Arial")

    ax.set_yticks(_ATTR_Y)
    ax.set_yticklabels(_ATTR_LABELS, fontsize=9.5, color="#444444", fontfamily="Arial")
    ax.set_ylim(min(_ATTR_Y) - 0.9, max(_ATTR_Y) + 0.9)
    ax.set_xticks([])
    for sp in ax.spines.values():
        sp.set_visible(False)
    ax.tick_params(length=0)
    plt.savefig(path, dpi=180, facecolor="white")
    plt.close(fig)

for cid, num, n_, share, lbl, sensitive in _CLASS_ORDER:
    _chart_path = os.path.join(_SCRATCH, f"wtp_{cid}.png")
    _make_wtp_chart(cid, _chart_path)

    s = prs.slides.add_slide(TONLY)
    # clear the layout title placeholder, draw our own fixed-size title textbox
    get_ph(s, 0).text_frame.text = ""
    _tbx = s.shapes.add_textbox(Inches(0.3), Inches(0.3), Inches(9.4), Inches(0.7))
    _ttf = _tbx.text_frame
    _ttf.word_wrap = True; _ttf.auto_size = MSO_AUTO_SIZE.NONE
    _ttf.vertical_anchor = MSO_ANCHOR.MIDDLE
    _tp = _ttf.paragraphs[0]; _tp.alignment = PP_ALIGN.LEFT
    _tr = _tp.add_run(); _tr.text = f"Class {num}: {lbl}  (n={n_:,}, {share}%)"
    _tr.font.bold = True; _tr.font.size = Pt(21); _tr.font.color.rgb = DARK

    # WTP chart — left (below the title)
    fit_picture(s, _chart_path, Inches(1.15), Inches(3.95), left=Inches(0.15), max_w=Inches(6.4))

    # characteristics box — right
    _hdr_col = ACCENT if sensitive else RGBColor(0x54, 0x6E, 0x7A)
    box = s.shapes.add_shape(MSO_SHAPE.ROUNDED_RECTANGLE,
                             Inches(6.65), Inches(1.35), Inches(3.15), Inches(3.75))
    box.fill.solid(); box.fill.fore_color.rgb = RGBColor(0xF5, 0xF7, 0xFA)
    box.line.color.rgb = _hdr_col; box.line.width = Pt(1.0); box.shadow.inherit = False
    tf = box.text_frame; tf.word_wrap = True
    tf.margin_left = Inches(0.18); tf.margin_right = Inches(0.14)
    tf.margin_top = Inches(0.16); tf.margin_bottom = Inches(0.14)
    p = tf.paragraphs[0]; p.alignment = PP_ALIGN.CENTER
    r = p.add_run(); r.text = "CLASS CHARACTERISTICS"
    r.font.size = Pt(12); r.font.bold = True; r.font.color.rgb = _hdr_col
    for line in _CHARS[cid]:
        pb = tf.add_paragraph(); pb.alignment = PP_ALIGN.LEFT
        pb.space_before = Pt(6); pb.space_after = Pt(0)
        rb = pb.add_run(); rb.text = "•  " + line
        rb.font.size = Pt(11); rb.font.color.rgb = DARK

    notes(s, _CLASS_NOTES[cid])
    set_pagenum(s)

# =========================================================================
# SLIDE 11 — Information treatment result
# =========================================================================
s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Key finding 3 - Does extra battery information change behavior?"
style_title(get_ph(s, 0))
add_bullets(get_ph(s, 1), [
    "Half the respondents received extra detail on battery replacement cost and warranty coverage; the other half did not.",
    "The extra information changed neither market engagement (opt-out rate) nor the broad pattern of choices.",
    "Interpretation: once the battery attributes themselves are visible in the offer, additional warranty/cost text adds little.",
], size=18, space=12)
notes(s, [
    "FINDING 3 — did the extra information help? Short answer: no.",
    "• Recall the setup — half the sample got the longer page with replacement cost and warranty details. We can now compare the two groups directly.",
    "• The extra information moved neither the opt-out rate nor the broad pattern of choices. Statistically, no effect on market engagement.",
    "",
    "HOW TO INTERPRET THIS (the nuance that makes it interesting):",
    "• This is NOT 'information doesn't matter.' It's that once the battery attributes themselves — range, degradation, refurbishment — are visible IN the offer, a paragraph of generic warranty and cost text adds little on top.",
    "• The attribute disclosure is doing the work; the educational boilerplate isn't. That's an actionable distinction: standardize the attributes in listings, don't just add reassuring copy.",
    "• It also tempers the easy policy hope that a quick info intervention fixes demand. The lever is structured disclosure, not more words.",
])
set_pagenum(s)

# =========================================================================
# SLIDE 12 — Key finding 3: why buyers opt out (Figure 5)
# =========================================================================
s = prs.slides.add_slide(TONLY)
get_ph(s, 0).text_frame.text = "Key finding 3 — why some reject every option"
style_title(get_ph(s, 0))
fit_picture(s, os.path.join(IMG, "thematic_hierarchical_nobev_all_blocks.png"),
            Inches(1.15), Inches(3.6), max_w=Inches(9.4))
notes(s, [
    "FINDING 4 — the people who rejected everything. We let them speak.",
    "• 209 respondents opted out of all six tasks. We asked them, in their own words, why — and coded the answers into 10 themes.",
    "• The top three: general disinterest in BEVs, economic barriers, and charging inconvenience. Over a third named more than one barrier.",
    "",
    "THE INSIGHT (this is the one to land):",
    "• Notice what's NOT at the top of the list: battery health specifically. For the most disengaged consumers, the battery is just one item on a longer list of reasons.",
    "• So better battery information — the thing this whole paper is about — won't bring these buyers back if they can't charge at home or can't afford the car in the first place.",
    "• It's a useful reality check: disclosure is powerful for the engaged 60%, but re-engaging the disengaged is a different problem — infrastructure and affordability.",
])
set_pagenum(s)



# =========================================================================
# SLIDE 13 — Conclusions & implications
# =========================================================================
s = prs.slides.add_slide(BODY)
get_ph(s, 0).text_frame.text = "Conclusions & implications"
style_title(get_ph(s, 0))
add_bullets(get_ph(s, 1), [
    "Buyers pay for range and discount mileage, degradation, and refurbishment",
    "Preferences are segmented, so match cars to the buyers who value them",
    "Credible disclosure: healthy BEVs earn a premium, refurbished go to budget buyers",
    "OEM certified pre-owned is the most feasible near-term path",
    "Re-engaging disengaged buyers also needs affordability and charging access",
], size=15, space=7)
notes(s, [
    "PULL IT TOGETHER — what to remember:",
    "• First dollar-valued WTP estimates for battery health in the US used BEV market. On average: range ~$11,070/100mi, degradation ~-$980/point/yr, refurbishment ~-$4,000 — though a quarter of buyers shrug at refurbishment.",
    "• The real story is heterogeneity: six segments, 60% battery-sensitive, 40% price-driven or disengaged.",
    "",
    "THE CENTRAL IMPLICATION (the line to deliver):",
    "• Battery-health transparency has real economic value — but the gain comes from MATCHING each car to the right buyer, not from raising every price. Certified-healthy BEVs can earn a premium with the sensitive 60%; refurbished or older packs route efficiently to the price-focused segment.",
    "• Most feasible near-term path: OEM-backed certified pre-owned programs, because OEMs already hold both the battery data and the warranty. They're positioned to make health credible.",
    "",
    "HONEST CAVEATS (end on credibility, not a sales pitch):",
    "• These are stated preferences, so magnitudes may run high — our range number sits above new-vehicle benchmarks, and the opt-out penalty is large.",
    "• The sample is intenders only. And disclosure helps the engaged; the disengaged need charging access and affordability first.",
    "• Thank you — happy to take questions.",
])
set_pagenum(s)

prs.save(OUT)
print("Saved:", OUT)
print("Slides:", len(prs.slides._sldIdLst))
