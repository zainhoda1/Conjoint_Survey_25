# Consumer Preferences in the Used Vehicle Market: Willingness to Pay for Powertrain Type, Driving Range, Vehicle Condition, and Operating Cost

Zain Hoda¹, Xiatian Iogansen¹,², Christina Gore²*, Joshua D. Kneifel², Sindhu Ranganath¹,², John Helveston¹

¹ Department of Engineering Management and Systems Engineering, George Washington University, Washington D.C., USA
² Applied Economics Office, Engineering Laboratory, National Institute of Standards and Technology, USA
\* Corresponding author

**Keywords:** Battery electric vehicle; Used vehicle market; Discrete choice experiment; Willingness to pay; Mixed logit; Consumer preferences

---

## Background and Objective

Used vehicles account for roughly 70% of U.S. vehicle transactions and used alternate fuel vehicles (AFVs) are a growing part of the used market. However, most research on AFV preferences has been focused on new-vehicle buyers while additional insights about used vehicle buyers will help extend vehicle lifetimes and lower the cost of vehicle ownership for lower and middle-income households. Demand appears constrained by range anxiety, charging access, upfront price premiums, and uncertainty about prior vehicle condition and battery aging. This study estimates willingness to pay (WTP) for powertrain type and other vehicle attributes among prospective U.S. used-vehicle buyers. It addresses two questions:

1. How do WTP estimates for used-vehicle attributes compare across vehicle segment and budget tier?
2. How do existing used AFVs compete against their conventional counterparts in the real market?

## Data and Methods

A nationally distributed online survey (Dynata and Prolific, December 2025 – April 2026) was fielded for adults in US planning to buy a used car or sport utility vehicle (SUV) within two years. After screening for attention-check failures and inconsistent responses, the analytic sample comprises **2,254 respondents**.

Each respondent completed six discrete choice experiment (DCE) tasks. Each task offered three used-vehicle alternatives and a "none of the above" opt-out. The attributes were:

- powertrain (conventional, gas hybrid [HEV], or battery electric [BEV])
- purchase price
- operating cost (cents per mile)
- electric driving range (BEV only)
- model year (a proxy for vehicle age)
- mileage

Four separate designs were built, one for each combination of body type (car or SUV) and budget tier (low or high), so that the profiles shown were plausible for each respondent. Choice profiles were generated with the `cbcTools` R package.

Six mixed logit (MXL) models were estimated directly in WTP space with the `logitr` package. Two are segment models (car and SUV) and four are budget-tier subgroup models. Estimation used panel correction, 5,000 Sobol draws, and 10 random starting points. BEV WTP was computed for defined configurations (100, 200, and 300 miles of range). Uncertainty was propagated by drawing 10,000 samples from the estimated joint parameter distribution. To answer the second question, simulated net WTP was compared with observed real-market used-listing price premiums for matched vehicle pairs at about three years of age.

## Results

**Preference heterogeneity.** Mean BEV powertrain utility is negative and highly significant in all six models (−$10.4k to −$26.1k). It is more negative for SUV than car buyers. The BEV and HEV standard deviations are large and significant throughout. This pattern is consistent with genuine within-segment polarization rather than uniform aversion to BEVs. HEVs carry no significant premium among low-budget buyers but a significant premium among high-budget buyers (about $4,200 for cars and $2,300 for SUVs).

**WTP for vehicle attributes (Figure 1).** The BEV penalty narrows sharply as driving range increases. For a 100-mile BEV, WTP relative to a conventional vehicle ranges from about −$8,200 (low-budget car) to −$18,800 (high-budget SUV). At 300 miles of range, WTP rises to about −$2,600 (high-budget car), −$3,700 (low-budget car), −$8,200 (low-budget SUV), and −$11,700 (high-budget SUV). **The only combination whose BEV WTP is statistically indistinguishable from the conventional baseline is the high-budget car buyer considering a 300-mile-range vehicle.** Mileage, vehicle age, and operating cost are valued negatively in every subgroup. Mileage carries the largest penalty (about $1,000–$3,800 per 10,000 miles). Vehicle age costs about $300–$2,200 per year and operating cost about $400–$1,100 per cent per mile. Dollar penalties are larger for high-budget buyers and for SUV buyers.

![Figure 1. Willingness to pay for vehicle attributes, by vehicle segment and budget tier (95% confidence intervals shown; values in thousands of dollars). For the BEV range configurations, solid intervals mark WTP calculated within the range shown to that segment and dotted intervals mark WTP extrapolated beyond it.](images/vehicle_analysis/wtp_plot_vehicle_attributes.png)

**Model WTP versus real-market prices.** Observed used-BEV price premiums exceed the WTP-implied requirement for parity in every matched pair examined. For the high-budget car pair, the BEV is priced near parity with its conventional counterpart (−$68) against a simulated net WTP of −$6,150, a shortfall of about $6,100. The two lower-budget BEVs are priced at a premium over their conventional counterparts (+$8,585 for the car and +$5,962 for the SUV). Their simulated net WTP is −$7,152 and −$10,897, so the gaps are roughly $15,700 and $16,900. HEV results are mixed. The high-budget car HEV appears underpriced relative to consumer valuation (simulated net WTP of +$4,860 against an observed premium of +$1,945). The high-budget SUV HEV is priced above its WTP-implied premium (+$7,987 against +$2,809).

## Implications

- **Incentive size.** The (former) federal used clean vehicle credit (capped at $4,000, for vehicles priced at or below $25,000) matches only the residual gap for high-budget car buyers considering longer-range BEVs. It is well below what other segments would require. That is roughly $8,000–$13,000 for a 100-mile car BEV and $16,000–$19,000 for an SUV BEV.
- **Targeting.** The price cap excludes most used BEVs with 250–300 miles of range, the configuration nearest parity. This risks rewarding purchases that would have occurred anyway.
- **Non-price instruments.** Standardized battery health disclosure, third-party certification, and transferable warranties could address the uncertainty behind the age-related BEV penalty at lower fiscal cost than cash transfers.
- **Hybrids.** HEVs already carry a positive WTP premium among high-budget car and SUV buyers. They may serve as a lower-friction near-term step toward electrification of the used market.

Overall, the results support segment-aware policy rather than a single uniform instrument.

## Limitations

The analysis uses stated rather than revealed preferences, and an online-panel sample may overrepresent information-engaged consumers. The MXL assumes continuous normal mixing distributions. The real-market comparison relies on a small number of matched pairs (three BEV-versus-conventional and two HEV-versus-conventional) at a single vehicle age. Future work could add a PHEV level, estimate latent-class models, validate against revealed transaction data, and jointly estimate the vehicle and battery DCEs.
