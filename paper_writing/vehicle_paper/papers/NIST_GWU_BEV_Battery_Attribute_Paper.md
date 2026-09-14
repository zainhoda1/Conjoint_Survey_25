1

2

3

4

5

6

7

8
9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

Consumer Valuation of Battery Health Attributes in Used Battery
Electric Vehicle Markets: A Discrete Choice Experiment

Xiatian Iogansena,b,1, Zain Hodab,2, Christina Gorea,∗,3, Joshua D. Kneifela,4,
Sindhu Ranganatha,b,5 and John P. Helvestonb,6

aApplied Economics Office, Engineering Laboratory, National Institute of Standards and Technology, Maryland, USA
bDepartment of Engineering Management and Systems Engineering, George Washington University, Washington D.C., USA

A R T I C L E I N F O

Keywords:
Battery electric vehicle
Used vehicle market
Discrete choice experiment
Willingness-to-pay
Consumer preferences
Information treatment

A B S T R A C T

Buyers of used battery electric vehicles (BEVs) face substantial uncertainty about battery
condition at the point of sale, even though the battery accounts for much of the vehicle’s value.
This study delivers the first consumer willingness to pay (WTP) estimates for battery-health
attributes of used BEVs. A discrete choice experiment was administered to a national sample
of 3,072 used vehicle buyers in the U.S. The DCE jointly varies price, mileage, electric range,
battery state of health, and battery refurbishment history across six repeated choice tasks. A
mixed logit model shows that consumers value additional range at $11,070 per 100 miles, while
each additional 10,000 miles of vehicle mileage reduces WTP by $2,780 and and each additional
percentage point of annual battery range loss reduces WTP by $980. Consumers also assign
negative values to pack- and cell-level battery refurbishment, with WTP reductions of $4,060
and $4,480, respectively. Each attribute shows wide dispersion around its mean, suggesting
substantial heterogeneity in WTP across the sample. A latent class choice model further identifies
six consumer segments, which differ substantially in their sensitivity to battery-health attributes,
price, and the no-choice option. Attitudes, infrastructure access, and demographics explain much
of the heterogeneity. In addition, respondents who reject all vehicle offers most often cite general
BEV disinterest, economic barriers, and charging inconvenience. However, providing detailed
battery replacement cost and warranty information does not affect overall market engagement
and choice behavior. This study assesses the potential for battery-health disclosure to serve as a
market-transparency signal in used BEV transactions, which carries implications for used vehicle
dealers, OEMs, and certification service providers.

1. Introduction

The used vehicle market is central to U.S. automobile ownership. Used vehicle transactions account for roughly
70% of all passenger vehicle purchases and are projected to grow steadily through the coming decade (Market Research
Future, 2025). An increasing number of used battery electric vehicles (BEVs) are entering the secondary market and the
demand is also accelerating: sales rose 34.2% year-over-year through early 2025, while average days of supply fell by
21.5% (Cox Automotive Inc., 2025). The growth of the used BEV market extends access to middle- and lower-income
households (Hagman et al., 2016), supports a circular economy by extending vehicle and battery lifetimes (Guzek
et al., 2024), and underpins the residual value that make new BEV leases financially viable (Brückmann et al., 2021;
Lim et al., 2015).

Despite this momentum, consumer hesitation toward used BEVs remains pronounced. Fernandes (2023) reports
that 52% of U.S. adults are unlikely to consider purchasing a used BEV, compared to 28% who express openness to
the idea. In the same survey, consumers most often cite uncertainty about vehicle and battery condition (41%) and
concerns about reliability relative to new EVs (23%) as primary barriers. Among those who have already purchased
a used BEV, over half reported pre-purchase concerns about battery longevity, even though only 5.1% experienced
substantial performance loss in practice (Sheykhfard et al., 2025).

∗Corresponding author
1xiatian.iogansen@gwu.edu & xiatian.logansen@nist.gov, https://orcid.org/0000-0002-4851-1323
2zain.hoda@gwu.edu, https://orcid.org/0009-0003-0958-1868
3christina.gore@nist.gov, https://orcid.org/0000-0002-3586-6918
4joshua.kneifel@nist.gov, https://orcid.org/0000-0002-3114-5531
5sindhu.ranganath@nist.gov, https://orcid.org/0000-0001-5764-9773
6jph@gwu.edu, https://orcid.org/0000-0002-2657-9191

Page 1 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

This gap between perceived and actual risk points to an information problem: buyers lack information about battery
health at the point of sale. This is a considerable risk as the battery can account for up to 30% of a used BEV’s value
(Boudway, 2020). Unlike conventional vehicles, where odometer readings and vehicle age are reasonable proxies for
overall wear, battery degradation in BEVs depends on factors such as charging history, depth of discharge cycles,
thermal exposure, and driving patterns (Bashash et al., 2011; Neubauer et al., 2012). Two BEVs of identical age and
mileage can differ substantially in their remaining battery capacity and projected useful life. Little of this history is
visible to buyers, and some of it is unavailable even to sellers themselves. As a result, sellers cannot credibly signal
battery quality and struggle to recoup the true value of their vehicles at resale, while buyers (unable to tell the difference)
may undervalue well-maintained BEVs and overpay for poorly maintained ones. The consequences extend beyond
individual transactions: persistent market uncertainty may suppress new BEV adoption by generating “resale anxiety”
among prospective buyers who question whether their new vehicle will hold its value (Brückmann et al., 2021).

Webb et al. (2025) finds that both current BEV owners and potential buyers view battery information, including
expected lifetime and range, as very useful for purchasing decisions. Stated usefulness, however, does not measure
consumers’ willingness to pay for these attributes. This study measures vehicle buyers’ preferences by administering a
discrete choice experiment (DCE) to a national sample of 3,072 U.S. adults who reported an intention to purchase a used
car or SUV within the next two years. The DCE presents respondents with six repeated choices among hypothetical used
BEVs that vary in mileage, purchase price, electric range, battery degradation rate, and battery refurbishment history.
Respondents are randomly assigned to receive either basic range-and-health information or extended information that
additionally describes battery maintenance costs and warranty coverage. This allows us to test whether the information
treatment shapes preferences and opt-out behavior. Respondents who selected the “no-choice” option in all six choice
tasks were asked to describe, in their own words, the reasons for consistently opting out.

This study provides the first WTP estimates for battery-health attributes in the U.S. used BEV market, conditional
on attributes being visible to consumers. We estimated two complementary models: (1) a mixed logit (MXL) model
characterizes aggregate preferences and the degree of preference heterogeneity across the full sample, and (2) a
latent class choice model (LCCM) which partitions respondents into six distinctive market segments that differ in
their sensitivity to vehicle attributes and have different individual characteristics. The MXL results suggest that the
population, on average, values additional electric range ($11,070 per 100 miles at Year 3), penalizes higher mileage
(-$2,780 per 10,000 miles), and discounts the rate of battery degradation (-$980 per percentage point of annual range
loss). The battery refurbishment history carries significant disutility, though estimates of large standard deviations in
WTP indicate substantial variation across individuals. The LCCM results indicate that this heterogeneity has a discrete
structure. Approximately 17% of respondents belong to a BEV-knowledgeable segment that places a high premium on
driving range, while an additional 45% fall into two classes that are particularly attentive to battery health attributes.
A further 17% are either price-constrained buyers or deeply skeptical non-adopters unlikely to enter the used BEV
market in the near term. The presence of identifiable segments with strong preferences for range and battery health
supports the potential for premium pricing for used BEVs with certified battery health, whereas the price-sensitive
segment suggests continued demand for affordable used BEVs. We also conduct a thematic analysis that identifies 10
barriers to BEV market engagement, with the most frequently cited being general BEV disinterest, economic barriers,
and charging inconvenience. Finally, we find that the information treatment does not significantly affect the overall
rate of market engagement.

2. Literature Review

This section reviews four strands of literature that inform the survey design and analysis: used BEV adoption, BEV
battery degradation and state of health (SOH), replacement cost and warranty considerations, and consumer valuation
of battery-health attributes.

2.1. Used Battery Electric Vehicle Adoption

Prior reviews organize the drivers of and barriers to BEV adoption into several domains: perceptions and attitudes,
sociodemographic characteristics, technology reliability, infrastructure accessibility, economic constraints, peer and
community influences, and information and knowledge (Gore et al., 2025; Iogansen et al., 2026, 2023; Lohawala and
Rahman, 2026; Naseri et al., 2024; Sonar et al., 2023; Yuan et al., 2018; Zou et al., 2020). Most of these studies concern
the new BEV market, whereas fewer studies examine the used market even as it is becoming an increasingly important
channel into BEV ownership.

Page 2 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

While used BEV buyers share some attribute preferences with new BEV buyers, the two consumer segments can
differ in profile, decision context, and constraints. Demographically, studies find that used BEV adopters are more often
male, White, better educated, and more likely to live in detached homes or townhouses, yet report lower household
incomes than new BEV buyers. This suggests that used BEVs offer an affordable entry point into BEV ownership
(Mashrur and Mohamed, 2025; Sheykhfard et al., 2025; Tal et al., 2017). Structurally, used BEV adopters more often
lack on-site charging: in a stated-preference experiment, used car buyers responded most to the proximity of slow
charging near home (“garage orphans”), whereas new BEV buyers responded to charging availability at their home
parking space (Zou et al., 2020). Affordability and battery condition also constrain used BEV buyers in ways that
constrain new BEV buyers less (Canepa et al., 2019; Loh and Noland, 2024). In terms of daily travel patterns, driving
range (though a common concern) may be less binding in the used market: Sheykhfard et al. (2025) found that 59.4%
of used BEV owners drove less than 100 miles per day and another 29.4% drove 101–200 miles, distances well within
the range of most current used BEVs.

2.2. Battery Degradation and State-of-Health

The uncertainty specific to used BEVs is battery health, or the remaining useful life of the battery (Pedrosa and
Nobre, 2018). Buyers cannot observe battery degradation (the loss of capacity and driving range over time), which
depends on how frequently and deeply the battery is cycled, the state-of-charge windows at which it is stored, and
the ambient temperatures it is exposed to (Bashash et al., 2011; Neubauer et al., 2012). Yang et al. (2018) simulate
battery useful life across U.S. driving and climate conditions and find that expected lifetimes range from roughly five
years for hot-climate and high-mileage owners to thirteen years for moderate-climate light users. Batteries typically
degrade linearly or sublinearly but can acceleration later in life, making long-term health conditions difficult to predict
(Attia et al., 2022). The conventional retirement benchmark, an SOH of 70–80% range relative to when the pack was
new (Canals Casals et al., 2019), is a rule of thumb rather than a hard physical threshold. The actual retirement point
depends on the vehicle segment, warranty policy, and the available second-life pathway.

For used BEV buyers, the two quantities most relevant to valuation (how much life remains in the pack, and how
confidently that estimate can be trusted) are precisely the hardest to observe. Even when SOH is reported, the number
itself is not standardized. Neither industry nor academia has reached consensus on a vehicle-level SOH definition or
measurement procedure (Bilfinger et al., 2025), and manufacturer-reported SOH can be inconsistent and unreliable
across BEV platforms (Park et al., 2026). This problem is especially acute in secondary markets, where buyers lack
knowledge of prior use, charging behavior, and refurbishment history. Battery health disclosure is therefore a market
transparency problem as much as a technical one.

2.3. Battery Replacement, Cost, and Warranty Considerations

Battery warranty coverage and out-of-warranty replacement costs anchor the economics of the used BEV market
because they shape both expected ownership costs and perceived downside risk. Manufacturer battery warranties are
relatively standardized, typically eight years or 100,000 miles (whichever comes first), with replacement triggered at
some manufacturers when capacity falls below a stated threshold (commonly 70%) (Clarke, 2024). Empirically, average
degradation stays well within these bounds. Drawing on a fleet of roughly 5,000 BEVs, Dnistran (2024) reports a mean
annual capacity loss of only 1.8%, implying that under typical duty cycles the pack should outlast the vehicle’s expected
service life. Even so, out-of-warranty replacements, when they occur, commonly cost $5,000 to $16,000 and exceed
$20,000 for some models, depending on pack size, labor, and parts availability (Kothari, 2024; Witt, 2024). Total-cost-
of-ownership (TCO) studies treat battery lifetime, replacement cost, and resale value as joint determinants of BEV
ownership costs (Hagman et al., 2016; Letmathe and Suares, 2017), but because these attributes are hard to verify at
the point of sale, they stop short of estimating how used BEV buyers actually price battery-condition uncertainty.

As growing numbers of BEV batteries reach the end of their vehicle life, their disposal has become a real
commercial and policy challenge (Skeete et al., 2020; Tankou et al., 2023). The industry has largely converged on
two routes: repurposing low-capacity batteries for stationary energy storage (the “second-life” route), or restoring
batteries for continued use in vehicles through refurbishment. Refurbishment can recover residual value from batteries
after their first vehicle life and may make refurbished options more cost-competitive than new packs (Jiao and Evans,
2016; Shaikh et al., 2023). Studies of remanufactured products suggest that consumers often discount refurbished goods
because of perceived quality risks and uncertainty about the refurbishment process (Abbey et al., 2015). Whether these
concerns extend to refurbished BEV batteries remains largely unexplored. Pedrosa and Nobre (2018) is the only study
to our knowledge that examines consumer perceptions of used BEVs with replaced batteries. Their qualitative results

Page 3 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

suggest that pairing a replacement battery with an explicit dealer warranty improves sentiment and, for some buyers,
raises reported WTP above the baseline used BEV scenario. Because that study is qualitative, small in scope, and
conducted in Portugal, its findings may not generalize to the U.S. market. A further question concerns the depth of
refurbishment, since batteries can be disassembled and repaired at the pack, module, or cell level. Foster et al. (2014)
estimate that remanufactured batteries with replaced cells or modules cost about 40% less than newly manufactured
packs. However, these savings carry technical risk: pairing new modules with aged ones can unbalance the pack, and
opening a sealed pack makes the repairer responsible for restoring its sealing, thermal management, and electrical
isolation (Tire Review Staff, 2026). Whether consumers evaluating used BEVs differentiate between original batteries
and refurbished batteries at different levels (e.g., cell- or module-level replacement versus complete pack replacement)
remains an open empirical question.

2.4. Willingness to Pay for Vehicle and Battery-Health Attributes

Direct WTP evidence on vehicle and battery-health attributes of used BEVs remains limited. A substantial
literature, however, focuses on the attributes of new BEVs. Table 9 in the Appendix consolidates the monetized
estimates from these studies by attribute.

Driving range is one of the most studied attributes. Early studies that used choice experiments among potential new
BEV buyers in the U.S. estimate a WTP of $35–$75 (Hidrue et al., 2011) and $25–$118 (Helveston et al., 2015) per
additional mile of range. A meta-analysis of 33 stated-preference studies on BEVs and broader alternative fuel vehicles
places mean WTP at US$66–$75 per mile and finds that marginal WTP declines as baseline range grows, so additional
range is worth more for short-range BEVs compared to for longer-range BEVs (Dimitropoulos et al., 2013). Tanaka
et al. (2014) report a far lower value for BEV and PHEV choices ($2.15 / mile) from a design whose range levels
extended to 1,000 miles. The most recent U.S. benchmark (Forsythe et al., 2023) used a nationally representative DCE
of new-BEV buyers and found car and SUV buyers are willing to pay $51 and $70, respectively, per mile of additional
BEV range.

Consumers also price other usability and performance attributes. Estimated WTP includes $425–$3,250 per hour
reduction in charging time (Hidrue et al., 2011), and roughly $4,100 for fast-charging capability (Forsythe et al., 2023).
A $0.01-per-mile reduction in operating cost is worth about $1,600 in the U.S. (Helveston et al., 2015), or $1,960 for
cars and $1,490 for SUVs in Forsythe et al. (2023), and a one-second acceleration improvement is worth roughly
$1,000–$1,500 across U.S. studies (Forsythe et al., 2023; Greene et al., 2018; Helveston et al., 2015). Helveston et al.
(2015) further estimate that U.S. consumers value BEV technology $10,000–$20,000 below a comparable conventional
vehicle depending on range. Pooling 52 U.S. studies and 777 marginal WTP estimates across 142 attributes, Greene
et al. (2018) report mean values of $86 per mile of range, $2,195 per hour of recharging time saved, and $1,880 per
one-cent-per-mile fuel-cost reduction. Vehicle attributes, in short, are routinely capitalized into consumer WTP.

Evidence on battery condition, the attributes closest to our study, is far thinner. In a Canadian national latent class
study among potential BEV buyers, Ferguson et al. (2018) report a WTP of up to $3,153 for a one-level improvement
in battery warranty, alongside about $30 per kilometer of added range among PHEV-oriented classes, $1,971 per hour
of public charging time saved, and $1,122 for a step improvement in charging-station availability.

Two features of this literature matter for interpreting our results. First, all estimates are nominal in each study’s
survey year, so cross-study comparisons require inflation adjustment. Second, no existing study prices the attributes that
distinguish a used BEV: mileage, SOH and degradation rate, and refurbishment history. The present study addresses
this gap.

3. Survey Design, Data Collection, and Sample Profile
3.1. Nationwide Used BEV Survey

The survey elicits consumer preferences and WTP for vehicle and battery-health attributes in the used BEV market.
The target population is U.S. adults aged 18 or older who reported their intention to purchase a used car or SUV within
the next two years. For a full description of the survey instrument, see Gore et al. (2025).7

The centerpiece of the survey is a BEV DCE, designed to examine how battery-health attributes are valued. The
DCE includes six repeated choice tasks in which respondents choose among hypothetical used BEVs with varying
attributes. All vehicles are assumed to have been manufactured in 2022 and to be three years old at the time of purchase.

7The Data Collection Instrument was published prior to data collection. Certain aspects of the survey design were revised following piloting.

Where discrepancies exist between the instrument and this paper, the description in this paper reflects the final implemented version.

Page 4 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

The attributes varied are vehicle mileage, purchase price, electric range, battery degradation and SOH, and battery
refurbishment history. Before the choice tasks, respondents see descriptions of all attributes, including definitions for
battery-specific terms (see Table 1). The DCE design varied two underlying parameters: electric range at manufacturing
(Year 0) and annual degradation rate. To improve comprehension, respondents instead saw these transformed into
range and SOH, both at Year 3 (the point of purchase) and what the expected values would be at Year 8 (five years
after purchase).

The DCE incorporates several ways to increase contextual tangibility and behavioral realism (Haghani et al., 2021).
First, each respondent selected a vehicle image of their preferred body type (car versus SUV) they found appealing
before the choice tasks. This image was then used to depict every alternative across the six tasks, holding appearance
constant so that choices reflect the listed attributes rather than styling. Second, for choice task, respondents select among
three used BEVs or a “None of the above” option. The opt-out allows respondents to decline unattractive alternatives,
which makes the stated choices closer to actual purchase decisions. Third, respondents were assigned to one of four
DCE versions based on their stated budget (low, ≤$20,000 versus high, >$20,000) and body-type preference.

Choice profiles were generated as a random design using the cbcTools R package (Helveston, 2025): for each
respondent, attribute levels for each alternative were drawn at random from the level ranges of the assigned version.
Unlike efficiency-optimized designs, a random design requires no prior assumptions about preference parameters and
preserves the independent attribute variation needed to identify potential interaction and nonlinear effects (Walker
et al., 2018). Figure 1 shows an example of one choice question.

Respondents were also randomly assigned to one of two information treatment conditions. The basic information
group received descriptions of electric range and battery SOH only. The extended information group additionally
received information on expected battery useful lifetime, replacement cost, and warranty coverage. This design allows
us to test whether the content of battery information affects consumers’ choices and their propensity to opt out.

Table 1
BEV choice experiment attributes and levels.

Attribute

Definition

Levels

Mileage

Battery
history

refurbishment

The total number of miles a vehicle
has traveled since manufacture.

The repair or replacement work that
has been performed on the vehicle’s
main battery pack.

Electric range at deliv-
ery (Year 0)

Battery annual degra-
dation rate and state of
health (SOH)

Purchase price

The maximum distance the vehicle
can travel on a full battery charge
under typical driving conditions, as
rated at the time of manufacture.

The average annual percentage loss
in electric range and battery SOH.
The remaining range and SOH at
Year 3 (current) and Year 8 (five
years from purchase) are derived
from this rate.

The total cost of the vehicle in
dollars,
including down payment,
monthly payments, taxes, and fees.

Low-budget
(≤$20,000) Car

High-budget
(>$20,000) Car

Low-budget
(≤$20,000) SUV

High-budget
(>$20,000) SUV

15,000–60,000 miles, in increments of 500 miles.

(1) Original: The battery remains in its factory-original condition with no repairs or
replacements.
(2) Some battery cells replaced: A portion of the battery cells has been replaced to address
performance issues or extend battery life; the remaining cells are original.
(3) Entire battery pack replaced: The full battery pack has been replaced with a new,
refurbished, or used pack, typically after battery performance declined below a specified
threshold.

50–150 miles
(50-mile increments)

100–250 miles
(50-mile increments)

150–250 miles
(50-mile increments)

200–350 miles
(50-mile increments)

1%–8%, in increments of 1%.

$10,000–$20,000
($2,000 increments)

$20,000–$40,000
($5,000 increments)

$15,000–$25,000
($2,000 increments)

$25,000–$45,000
($5,000 increments)

Page 5 of 29

Figure 1: Example of a choice experiment question. In each unique question, the values of each attribute varied according
to the predefined design of experiment.

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

In addition to the DCE, the survey collects additional information. Before the choice tasks, respondents report
on their current household vehicle fleet (fuel type, range, acquisition method, and costs) and future purchasing plans
(anticipated budget, payment method, and the likelihood of buying a new or used plug-in hybrid or BEV). After the
choice tasks, the survey assesses respondents’ knowledge, attitudes, and perceptions of BEVs and battery technology.
This section includes factual questions on BEV charging capabilities, as well as psychological statements rated
on five-point Likert scales, covering social norms, beliefs, cost perceptions, battery concerns, and personal traits
such as risk-taking and price sensitivity. Two items about refurbished BEV batteries are of particular relevance to
this study: “Purchasing refurbished BEV batteries will minimize negative effects on natural ecosystems” (hereafter
environmentally positive) and “Refurbished BEV batteries do NOT perform and function as original BEV batteries”
(hereafter functionally negative). The final section collects demographic information including age, gender, race and
ethnicity, household size, income, education, employment status, and housing type.

3.2. Data Collection and Sample Profile

The survey was administered online through two research platforms (Prolific and Dynata) between December
2025 and April 2026. The median completion time was approximately 14 minutes. All participants received platform-
standard compensation.

The initial sample comprised 3,657 respondents from across the U.S. We identified and removed low-quality
responses by reviewing each case, including overall completion-time, failure of the embedded attention check
questions, internal inconsistencies across survey sections, and uninformative or incoherent open-ended text. A separate
timing screen flagged respondents who sped through the choice tasks specifically. The final sample consists of 3,072
respondents, yielding 18,432 choice observations across six tasks. Figure 2 shows the geographic distribution of
respondents by ZIP code, with point size and color intensity scaled to respondent count.

Page 6 of 29

Figure 2: Geographic distribution of survey respondents across the continental United States.

1

2

3

4

5

6

7

8

9

Because no official population distribution exists for consumers intending to purchase a used vehicle, we compare
the sample’s demographic profile with ACS 2020–2024 five-year estimates (Table 2). The sample aligns closely with
the national adult population on housing type and on lower- and middle-income shares. It skews younger (5.4% aged 65
and older versus 22.0% nationally), more female (60.7% versus 51.0%), and more educated (54.1% hold a bachelor’s
degree or higher versus 35.7% of adults 25 and older). Renters are over-represented (46.7% versus 34.8%), while
households earning $150,000 or more (13.4% versus 23.0%) and Hispanic adults (11.4% versus 17.4%) are under-
represented. Some of these deviations are consistent with findings from prior studies of used-vehicle buyers (e.g.,
skew younger and lower-income than the general population). Without the population benchmark, we decided not to
weight the data, and all analyses below are unweighted.

Page 7 of 29

Table 2
Sample profile compared with the U.S. population (ACS 2020–2024 5-year estimates).

Sample (n)

Sample (%) ACS (%)

Age

18 - 24
25 - 34
35 - 44
45 - 54
55 - 64
65+
Gender

Female
Male
Other

Race

White alone
Black or African American alone
Other or multiple races

Hispanic origin

Hispanic or Latino
Not Hispanic or Latino

Household income

Less than $25,000
$25,000 - $49,999
$50,000 - $74,999
$75,000 - $99,999
$100,000 - $149,999
$150,000 or more

Education

High school or less
Some college or associate
Bachelor’s degree
Graduate or professional degree

Housing tenure

Own
Rent

Housing type

Single-family detached
Single-family attached
Apartment (2+ units)
Mobile home or other

438
921
742
500
280
165

1,861
1,140
65

2,088
478
506

349
2,723

458
495
751
354
601
411

439
968
1,065
592

1,519
1,331

1,874
330
755
113

14.4
30.2
24.4
16.4
9.2
5.4

60.7
37.2
2.1

68.0
15.6
16.5

11.4
88.6

14.9
16.1
24.5
11.5
19.6
13.4

14.3
31.6
34.8
19.3

53.3
46.7

61.0
10.7
24.6
3.7

11.7
17.5
16.9
15.6
16.2
22.0

51.0
49.0
–

63.3
11.9
24.8

17.4
82.6

14.6
16.7
15.5
12.6
17.5
23.0

36.4
27.9
21.6
14.1

65.2
34.8

62.3
6.4
26.0
5.2

Notes: Sample size and shares are unweighted and exclude item nonresponse. ‘–’ indicates no comparable ACS
category. ACS universes: population 18+ (age, gender, race, Hispanic origin), population 25+ (education), and
households (income, tenure, housing type).

4. Method
4.1. Mixed Logit Model

We begin with estimating WTP for vehicle attributes across the full sample (3,072 respondents, 18,432 choice
observations). We estimate a mixed logit (MXL) model directly in WTP space using the logitr R package (Helveston,
2023). This provides an aggregate baseline and reveals the extent of preference heterogeneity. The MXL model relaxes
the Independence of Irrelevant Alternatives (IIA) assumption of the standard multinomial logit by allowing preference
parameters to vary continuously across individuals (McFadden and Train, 2000; Train, 2009). Estimating in WTP space
lets us specify the distribution of WTP for each non-price attribute directly, rather than recovering it post hoc from the

1

2

3

4

5

6

7

8

Page 8 of 29

1

2

3

4

5

6

7

8

9

10

ratio of attribute to price coefficients. We assume individual WTP values follow normal distributions, which captures
symmetric heterogeneity around population means. The utility parameterization follows the notation in Helveston et al.
(2018) and Helveston (2023).

The WTP-space utility for respondent 𝑛 choosing alternative 𝑖 in choice scenario 𝑡 is:

𝑢𝑛𝑡𝑖 = 𝜆𝑛

(𝝎⊤

𝑛 𝐱𝑛𝑡𝑖 − 𝑝𝑛𝑡𝑖

)

+ 𝜀𝑛𝑡𝑖,

𝜀𝑛𝑡𝑖 ∼ Gumbel(0, 1)

(1)

where 𝑝𝑛𝑡𝑖 is the purchase price presented in scenario 𝑡 to respondent 𝑛; 𝐱𝑛𝑡𝑖 is a vector of non-price BEV attributes
including vehicle mileage, battery refurbishment history, current electric range at the point of purchase (Year 3), and
the proportional rate of range loss over the subsequent five years (to Year 8);8 𝜆𝑛 > 0 is the individual-specific scale
parameter; 𝝎𝑛 is a vector of individual WTP values drawn from a mixing distribution 𝑓 (𝝎 ∣ 𝝁, 𝚺) with population
mean 𝝁 and covariance 𝚺; and 𝜀𝑛𝑡𝑖 is an IID Type I Extreme Value error. Because 𝜆𝑛 and 𝝎𝑛 are both unobserved, the
choice probability requires integrating the logit kernel over their joint mixing distribution:

𝑃𝑛𝑡𝑖(𝝁, 𝚺) = ∫ ∫

exp

(𝜆(𝝎⊤𝐱𝑛𝑡𝑖 − 𝑝𝑛𝑡𝑖
))
(𝜆(𝝎⊤𝐱𝑛𝑡𝑗 − 𝑝𝑛𝑡𝑗
exp

∑

𝑗

)) 𝑓 (𝝎 ∣ 𝝁, 𝚺) 𝑔(𝜆) 𝑑𝝎 𝑑𝜆

(2)

11

12

This integral is evaluated by simulation using Sobol draws (𝑅 = 1000). Accounting for the panel structure of six
repeated choice tasks per respondent, the simulated log-likelihood is:

(𝝁, 𝚺) =

𝑁
∑

𝑛=1

ln

1
𝑅

⎡
⎢
⎢
⎢
⎣

𝑅
∑

𝑇
∏

𝑟=1

𝑡=1

𝑗

exp
∑

(

(𝝎⊤
𝜆𝑟
(𝜆𝑟

𝑟 𝐱𝑛𝑡𝑖∗
(𝝎⊤

𝑛𝑡

exp

))

− 𝑝𝑛𝑡𝑖∗

𝑛𝑡

𝑟 𝐱𝑛𝑡𝑗 − 𝑝𝑛𝑡𝑗

))

⎤
⎥
⎥
⎥
⎦

(3)

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

where (𝜆𝑟, 𝝎𝑟) is the 𝑟-th draw from the joint mixing distribution and 𝑖∗
𝑛𝑡 is the alternative chosen by respondent 𝑛 in
scenario 𝑡. Because estimation is carried out directly in the WTP space, the population mean WTP for each non-price
attribute 𝑘 is obtained directly as ̂𝜇𝑘.

4.2. Latent Class Choice Model

Model Framework

The MXL results reveal substantial preference heterogeneity across respondents. To explore whether this hetero-
geneity reflects discrete consumer segments with systematically different preferences, we estimate a latent class choice
model (LCCM) using the Apollo R package (Hess and Palma, 2019). The LCCM assumes that individuals can be
assigned to a finite number of unobserved groups (classes) within which preferences are homogeneous, but across which
they differ (Greene and Hensher, 2003; Kamakura and Russell, 1989). This structure is well suited to the situations
where consumers may differ in their attitudes toward battery health risk, their familiarity with BEV technology, and
their price sensitivity. The model is estimated on a subsample obtained after excluding cases with missing data on
covariates (2,916 respondents, 17,496 choice observations).

As illustrated in Figure 3, the LCCM consists of two simultaneously estimated components. The class membership
model assigns each respondent probabilistically to a latent class as a function of observable individual characteristics.
The class-specific choice model then estimates the conditional probability of each vehicle choice within each class.
In addition, a set of inactive indicators excluded from estimation for collinearity or theoretical reasons is used to
characterize the classes descriptively after estimation. The formal model structure follows the notation of Chen et al.
(2023).

8We compared six attribute specifications that varied whether range was expressed at Year 0 or Year 3, and whether battery health change was
expressed as absolute range loss or as a percentage degradation rate. The specification combining range at Year 3 with percentage loss rate from
Year 3 to Year 8 yielded the best model fit and the most economically interpretable coefficients, which suggests consumers evaluate used BEVs by
current performance and relative future deterioration.

Page 9 of 29

Figure 3: LCCM model framework.

Class-specific Choice Model

For respondent 𝑛 belonging to latent class 𝑠 (𝑠 = 1, 2, … , 𝑆), the utility of choosing vehicle alternative 𝑖 in choice

scenario 𝑡 (𝑡 = 1, … , 6) is:

𝑈𝑛𝑡𝑖∣𝑠 = 𝜷⊤

𝑠 𝐗𝑛𝑡𝑖 + 𝜀𝑛𝑡𝑖∣𝑠

(4)

where 𝐗𝑛𝑡𝑖 is a vector of observed BEV attributes (vehicle mileage, battery refurbishment status, electric range at
Year 3, proportional range loss to Year 8, and purchase price); 𝜷𝑠 is a vector of class-specific utility parameters; and
𝜀𝑛𝑡𝑖∣𝑠 is an IID Type I Extreme Value error. Under this assumption, the conditional probability that respondent 𝑛 in
class 𝑠 chooses alternative 𝑖 in scenario 𝑡 follows the MNL form:

𝑃𝑛𝑡(𝑖 ∣ 𝑠) =

(𝜷⊤

exp

)

𝑠 𝐗𝑛𝑡𝑖
(𝜷⊤

𝑠 𝐗𝑛𝑡𝑗

∑

𝑗∈𝑛𝑡

exp

)

(5)

where 

𝑛𝑡 denotes the choice set available to respondent 𝑛 at scenario 𝑡.

To capture the non-linear relationship between range and choice probability, we adopt a piecewise linear
specification for the range and degradation rate attributes. Consumers likely have diminishing sensitivity to range
at higher levels and heightened sensitivity around near minimum viability thresholds (Dimitropoulos et al., 2013;
Hackbarth and Madlener, 2016). We compared a quadratic specification, which imposes a symmetric parabolic shape
and performs poorly at the tails, with a piecewise linear specification that estimates separate slopes for distinct
segments. The piecewise specification provides superior model fit and more interpretable coefficients. Electric range
at Year 3 is segmented into 40–130 miles, 130–200 miles, and above 200 miles; the range loss rate is segmented into
below 12%, 12–24%, and above 24%. These thresholds reflect both theoretical considerations about minimum driving
viability and empirical patterns in the data (the breakpoints fall near the 33rd and 66th percentiles of each attribute’s
distribution).

To assess whether the estimated marginal valuations differ significantly across segments, we apply the delta
method to test pairwise WTP differences within each class. For any two segments 𝑠 and 𝑠′, the WTP difference is
( ̂𝛽𝑠 − ̂𝛽𝑠′)∕(− ̂𝛽price) × $10,000, with standard errors approximated using the model’s robust covariance matrix. Note
that pairwise differences are meaningful only when the individual WTP estimates being compared are estimated with

Page 10 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

Inactive indicators(not included in the model, buthelp describe the classes)Class Membership ModelLatent ClassPerceptions & attitudestoward EV and EVBVehicle travel demand(proxy)EV infrastructureavailabilitySocio-demographicsClass-Speciﬁc Choice ModelUtilitiesBEV AttributesVehicleChoice1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

sufficient precision. When either the attribute or price coefficient is not statistically distinguishable from zero, the
resulting WTP estimate is imprecise, and the pairwise comparison inherits this uncertainty. Consequently, a non-
significant pairwise difference may indicate either genuinely similar marginal valuations or insufficient statistical power
to distinguish two imprecisely estimated WTPs. For this reason, we discuss pairwise differences only for classes in
which the underlying attribute-specific WTP estimates are themselves statistically significant.

Class Membership Model

The class membership model specifies the probability that respondent 𝑛 belongs to latent class 𝑠 as a function of

observed individual characteristics:

𝑀𝑛(𝑠) =

exp

(𝜸⊤

𝑠 𝐙𝑛

)

𝑆
∑

𝑠′=1

exp

(𝜸⊤

𝑠′ 𝐙𝑛

)

(6)

where 𝐙𝑛 is a vector of individual-level covariates organized into four groups: (1) perceptions and attitudes toward
BEVs and BEV batteries (e.g., range anxiety, environmental and functional assessments of refurbished batteries); (2)
vehicle travel demand proxies (e.g., range of the respondent’s primary vehicle); (3) EV infrastructure availability (e.g.,
access to electrical outlets for charging); and (4) socio-demographic characteristics (e.g., household income). The
parameters for the first class (𝑠 = 1), which serves as the reference class, are normalized to zero for identification.

The unconditional probability that respondent 𝑛 chooses alternative 𝑖 in scenario 𝑡, integrating over all latent classes,

is:

𝑃𝑛𝑡(𝑦𝑛 = 𝑖) =

𝑆
∑

𝑠=1

𝑃𝑛𝑡(𝑖 ∣ 𝑠) ⋅ 𝑀𝑛(𝑠)

(7)

Because each respondent completes all 𝑇 = 6 choice tasks as a member of a single latent class, the likelihood takes
the product over tasks within each class before summing across classes. The log-likelihood across all 𝑁 respondents
is:

(𝜷, 𝜸) =

𝑁
∑

𝑛=1

ln

[ 𝑆
∑

𝑠=1

𝑀𝑛(𝑠)

𝑇
∏

𝑡=1

]

𝑃𝑛𝑡(𝑖∗

𝑛𝑡 ∣ 𝑠)

(8)

where 𝑖∗
jointly via maximum likelihood.

𝑛𝑡 denotes the alternative chosen by respondent 𝑛 in scenario 𝑡. The model parameters 𝜷𝑠 and 𝜸𝑠 are estimated

Willingness to Pay

A key output of the LCCM is class-specific WTP for each BEV attribute. Because price enters linearly, the marginal

utility of income is constant within each class, and the WTP for attribute 𝑘 in class 𝑠 is:

WTP𝑠,𝑘 = −

̂𝛽𝑠,𝑘
̂𝛽𝑠,price

(9)

This represents the dollar amount a representative respondent in class 𝑠 would pay, or would need to be compensated,
for a one-unit change in attribute 𝑘, holding all other attributes constant.

4.3. Thematic Analysis of Reasons for Opt-out

Respondents (𝑛 = 209) who opted out systematically (i.e., choosing none of the presented used BEVs in all six
choice tasks) were asked to explain their reasons in an open-ended question at the end of the DCE ("We noticed you
did not choose any of the electric vehicle options in the last few questions. Could you tell us any reasons why?").

Page 11 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

These open-ended responses complement the quantitative analyses in two ways: they point to the specific attributes
respondents dislike most, and they can surface barriers that are absent from our attribute list or hard to quantify in
closed-ended questions.

We use thematic analysis, which applies a coding system to organize qualitative responses and identify recurring
themes (Braun and Clarke, 2006). Reviewing the response texts, we constructed a two-level codebook of 10 parent
themes and 25 sub-themes. Because respondents could cite multiple reasons for opting out, we also examine theme
co-occurrence to identify clusters of related barriers (Krishna, 2021). Finally, we test whether the battery information
treatment affects the opt-out rate.

5. Results and Discussion
5.1. Results of Mixed Logit Model

Table 3 presents the MXL model estimates. All mean WTP parameters are significant at the 0.1% level (***).
On average, respondents value an additional 100 miles of electric range at Year 3 at $11,070, penalize every 10,000
miles of additional odometer mileage by $2,780, and discount each additional percentage point of annual range-loss
rate by $980. Both refurbishment types generate meaningful disutility: pack replacement reduces WTP by $4,060 and
cell replacement by $4,480. The no-choice opt-out option carries a large negative WTP (-$51,300), reflecting a strong
baseline preference to purchase a vehicle rather than opt out.

The standard deviation estimates show substantial preference heterogeneity across respondents. Range at Year 3
exhibits the widest dispersion in absolute terms (SD = $13,950 relative to a mean of $11,070), with the distribution of
simulated WTP draws spanning from $1,660 at the first quartile to $20,470 at the third quartile. This indicates that while
most respondents positively value range, a non-trivial share places little weight on it. Heterogeneity in the refurbishment
parameters is also pronounced: the standard deviations for pack replacement ($7,300) and cell replacement ($6,720)
are both larger in magnitude than their respective means, and the upper quartiles of the simulated draws are positive
($850 and $70, respectively). This implies that roughly one quarter of consumers view battery refurbishment neutrally
or even as a positive quality signal, potentially because a replaced battery is perceived as newer or more cost-effective
than the original. Mileage dispersion is also substantial: approximately one quarter of the simulated draws are positive.
In contrast, range-loss rate is the most uniformly penalized attribute; its standard deviation is small relative to the
mean, and the distribution remains negative throughout nearly the entire sample. Together, these results suggest that
consumer valuations of battery-related attributes are heterogeneous in both sign and magnitude. This motivates the
segment-level analysis that follows.

Page 12 of 29

Table 3
Mixed Logit Model Estimates in WTP Space (price unit: $10,000).

Parameter

Mean parameters

Estimate

Std. Error

𝑝-value

𝜆 (scale)
Mileage
Range (Year 3)
Range loss Rate (Year 3 to Year 8, in percentage)
Battery Refurbishment: Pack Replace
Battery Refurbishment: Cell Replace
No choice (opt-out)

Standard deviation

SD: Mileage
SD: Range (Year 3)
SD: Range loss Rate (Year 3 to Year 8, in percentage)
SD: Battery Refurbishment: Pack Replace
SD: Battery Refurbishment: Cell Replace

0.987
-0.278
1.107
-0.098
-0.406
-0.448
-5.130

-0.402
1.395
-0.077
0.730
-0.672

0.025 <0.001
0.015 <0.001
0.045 <0.001
0.003 <0.001
0.037 <0.001
0.037 <0.001
0.110 <0.001

0.017 <0.001
0.053 <0.001
0.003 <0.001
0.060 <0.001
0.061 <0.001

***
***
***
***
***
***
***

***
***
***
***
***

Observations
Parameters
Log-Likelihood
Null Log-Likelihood
AIC
BIC
McFadden 𝑅2
Adj. McFadden 𝑅2

Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1.

5.2. Results of Latent Class Choice Model
5.2.1. Model Selection

Summary of 10k Draws

1st Qu. Median

3rd Qu.

-0.548
0.166
-0.150
-0.899
-0.899

-0.277
1.107
-0.098
-0.407
-0.447

-0.006
2.047
-0.046
0.085
0.007

3,072 × 6 = 18,432
12
-18,615.44
-25,552.18
37,254.89
37,348.75
0.2715
0.2710

To determine the optimal number of latent classes, we estimated models with one through eight classes (Table 4).
Model fit was evaluated using log-likelihood (𝐿𝐿), the 𝜌2 measure relative to the null model, Akaike Information
Criterion (𝐴𝐼𝐶), Bayesian Information Criterion (𝐵𝐼𝐶), adjusted 𝜌2, and entropy, where smaller 𝐴𝐼𝐶 and 𝐵𝐼𝐶
values and larger adjusted 𝜌2 and entropy indicate better fit and class separation (Nylund et al., 2007). We selected a
six-class solution based on model fit and interpretability. Class membership ranges from 8.7% to 22.7% of the sample.
The improvement in BIC diminishes substantially beyond six classes. Moreover, in the seven- and eight-class solutions,
one class consistently accounted for fewer than 6% of the sample, reducing robustness for segment-level analysis.

Table 4
Latent Class Model Fit Index Comparison.

No. of Classes Npar

LL

𝐿2

AIC

BIC Adj.𝜌2

Entropy Class proportions

1
2
3
4
5
6
7
8

11
33
55
77
99
121
143
165

-20,474
-17,917
-17,411
-17,071
-16,771
-16,538
-16,413
-16,308

0
5,114
6,126
6,806
7,406
7,872
8,122
8,332

40,970
35,901
34,931
34,296
33,740
33,318
33,113
32,947

41,056
36,157
35,358
34,895
34,510
34,258
34,224
34,229

0.148
0.254
0.274
0.287
0.299
0.308
0.312
0.316

—
0.911
0.759
0.758
0.723
0.730
0.736
0.728

100.0%
78.8% / 21.2%
56.0% / 23.4% / 20.6%
47.7% / 21.9% / 21.6% / 8.8%
25.8% / 22.9% / 21.5% / 21.1% / 8.7%
22.7% / 20.8% / 18.5% / 16.6% / 12.8% / 8.7%
22.2% / 18.3% / 16.5% / 15.9% / 12.6% / 8.8% / 5.6%
20.4% / 16.1% / 16.0% / 15.1% / 12.8% / 7.9% / 5.8% / 5.8%

1 𝐿2 = −2(LL1𝑐 − LLmodel).
2 Adj.𝜌2 = adjusted rho-squared vs. observed shares (constants model).
3 Entropy = 1 − 𝐻∕𝐻max, where 𝐻 = − ∑ 𝜋𝑖𝑘 log(𝜋𝑖𝑘). Values closer to 1 indicate cleaner class separation.

Page 13 of 29

1

2

3

4

5

6

7

8

9

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

52

5.2.2. WTP and Class Profiles

Estimated model coefficients are presented in Table 5, with Class 5 serving as the reference class. Figure 4 provides
a visual summary of WTP estimates and segment profiles across classes. Full WTP estimates and class profiles are
reported in Table 6 in the Appendix. Metadata on opt-out behavior and pairwise WTP differences are provided in
Tables 8 and 7, respectively. We describe each class below.

Class 1: Battery Health Attentives (𝑛=661, 22.7%).
Class 1, the largest class, is defined by battery state-of-health sensitivity. Range loss aversion is the highest compared
to other classes, with penalties $2,310*** per percentage point in the 5–12% degradation segment, $2,330*** in the
12–24% segment, and $2,140 in the 24%+ segment. Both refurbishment types also generate meaningful disutility
(-$2,740* for pack replacement; -$3,760** for cell replacement). Their range WTP is consistent across the three
piecewise segments ($4,570, $4,610, and $5,310**), indicating a steady marginal valuation of additional range without
a strong threshold effect, though only the highest tier reaches statistical significance.

Members of Class 1 have an average household income of $91,340 and are relatively young (36.5 years). They
demonstrate above-average BEV knowledge (76.2%), moderate electrical outlet access (46.0%), and a high rate of
EV-owning neighbors (43.5%). These well-informed consumers place great value on battery reliability at the point of
sale and expected battery performance over time.

Their valuation structure is well suited to transparent, attribute-based pricing. Financing products that tie interest
rates or warranty terms to battery SOH readings, or that offer extended coverage contingent on passing a minimum
degradation threshold, could be particularly attractive to this segment. Their strong aversion to range loss also suggests
that short-term ownership scenarios with rapid degradation are significantly discounted. Long-term degradation
forecasts in the listing description could address this concern directly.

Class 2: Multi-Attribute Maximizers (𝑛=606, 20.8%).
Class 2 exhibits statistically significant valuations across all vehicle attributes. This class places a higher premium on
crossing the minimum viable range threshold ($11,830***, $9,450***, $9,500***), but pairwise tests indicate that
these differences are only directional and are not statistically significant. Range loss aversion is significant across all
three segments (-$780***, -$760***, -$390* per percentage point). Refurbishment disutility is among the highest of
the attribute-sensitive classes (-$5,760*** for pack replacement; -$6,070*** for cell replacement).

What further distinguishes Class 2 is its opt-out pattern: only 5.2% of members never opt out, while the plurality
opt out two (29.0%) or three (27.2%) times out of six tasks. Class 2 members exhibit high range anxiety (83.4%) and are
particularly averse to battery refurbishment (only 18.8% disagree that refurbished batteries are functionally inferior).
They also report average household incomes ($86,920) and lower risk tolerance. These characteristics are consistent
with their cautious evaluations across all vehicle attributes at once. The uniform magnitude and significance of their
attribute benefits and penalties mean that no single attribute can easily offset large deficits elsewhere. Combined with
their high opt-out frequency, this suggests that when a choice set presents no option that reaches a minimal quality
threshold across all dimensions at once, respondents in this class choose to opt out rather than settle for the least-bad
alternative. Clear multi-dimensional disclosure presenting range, degradation rate, and refurbishment status together
would be highly valued by this customer segment in their vehicle choice decision making process.

Class 3: Range-Focused, EV-Knowledgeable Consumers (𝑛=485, 16.6%).
Class 3 is distinguished by an exceptionally high WTP for range: $38,700*** per 100 miles for vehicles with 40–130
miles of range, falling sharply to $20,820*** in the 130–200 miles segment. Pairwise tests (see Table 8) suggest that
this drop is statistically significant (𝑝 = 0.003). However, the WTP for the 200+ mile segment ($25,470***) is not
statistically distinguishable from the 130–200 mile segment. This pattern suggests that consumers in this class place
a substantial premium on vehicles that exceed the minimum viable range threshold, but marginal range beyond 130
miles provides no statistically significant incremental value. This aligns with evidence that the marginal value of range
is highest at low baseline range (Dimitropoulos et al., 2013; Hackbarth and Madlener, 2016). Their range loss aversion
is moderate and significant across all degradation segments (-$870***, -$690***, -$470***). Pairwise tests indicate
that the disutility associated with the 5-12% range loss rate is statistically higher than that of the 24%+ range loss rate.
Class 3 consumers are range-sensitive, thus each additional percentage point of range loss in the low-degradation tier
carries a larger penalty because it further reduces a vehicle attribute that these consumers explicitly value. In contrast,
vehicles in the high-degradation tier may already fall outside their acceptable choice set, making additional losses less
consequential. WTP for refurbishment is negligible and statistically insignificant, suggesting that this class focuses
more on current and future performance than on battery refurbishment history.

Page 14 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

Class 3 represents the most EV-knowledgeable segment in the sample. They hold the most favorable attitudes
toward refurbished batteries (65.5% agree they are environmentally beneficial and 35.5% disagree that they are
functionally inferior). Their households show the lowest rate of ICEV-only composition (74.1%), and they are the
most likely class to prefer a car over an SUV (57.2%). They have the highest electrical outlet access (57.5%), along
with the strongest BEV knowledge (81.1%) and subsidy awareness (31.9%). They have the highest household income
($97,120) and the highest employment rate (71.5%), and are the most risk-tolerant of all classes (39.0% agree with a
risk-taking characterization).

Their minimal disutility for refurbishment history, combined with high income, high BEV knowledge, and positive
attitudes toward refurbished batteries, suggests they are open to technically sound refurbishments provided the resulting
vehicle meets their range threshold. Performance-oriented marketing that leads with current range and degradation
trajectory, may resonate most with this segment.

Class 4: Budget-constrained, Low-WTP Consumers (𝑛=373, 12.8%).
Class 4 has the largest price coefficient in absolute value (−4.017***), more than twice the magnitude of the next-most
price-sensitive class. This suggests that members of Class 4 are budget-constrained buyers. Range WTPs are modest
but statistically significant at lower range segments ($2,810** for 40–130 miles; $1.8k* for 130–200 miles), and range
loss aversion is marginal (-$170** and -$170*** in the two upper degradation segments). WTP for refurbishment
is negligible and statistically insignificant. Battery service history is therefore largely immaterial in this group’s
purchasing calculus.

Class 4 members have the lowest average household income ($70,000) and the lowest next-vehicle budget
($19,660). They have the lowest homeownership rate (43.0%) and the highest share of renters (48.8%), the fewest
household vehicles (1.9 on average), and the highest share of ICEV-only households (87.1%). Despite their strong
price sensitivity, 91.2% of Class 4 members never select the no-choice option, which indicates a potential reliance on
having a personal vehicle. They are interested in used vehicles, but price dominates battery SOH in their choices. For
this class, improving vehicle information transparency would enable consumers to evaluate used BEVs with greater
confidence and make purchase decisions that best align with their budget constraints and preferences.

Class 5: Opt-Out Dominant, BEV-Skeptical Consumers (𝑛=252, 8.7%).
Class 5 is defined primarily by systematic opt-out behavior: 75.4% of members selected the no-choice option in all six
tasks, and an additional 19.4% did so in five of six. As a result, most WTP estimates for vehicle attributes are statistically
indistinguishable from zero, with the exception of a moderate disutility for range loss in the 5–12% segment (-$1,620*
per percentage point) and significant penalties for both pack replacement (-$6,350*) and cell replacement (-$7,320*).
These patterns suggest that respondents who systematically avoid the BEV market retain particular concerns about
battery quality.

Class 5 represents consumers who are not yet viable participants in the used BEV market. The majority (70.4%)
strongly disagree that they are likely to purchase a used BEV. They are the oldest class on average (47.1 years old), the
least risk-tolerant (24.0% agree that they are prepared to take risks), and the least environmentally concerned (21.7%
express concern about global climate change). Their household fleets are almost exclusively internal combustion engine
vehicles (ICEVs), with 89.3% of households reporting no BEVs and 93.3% driving an ICEV as their primary vehicle.
A majority (68.2%) intend their next vehicle to be an SUV. EV-related knowledge is low (68.5% answer the BEV
knowledge question correctly compared with 81.1% in the most knowledgeable group, and only 10.6% are aware of
federal BEV subsidies), while range anxiety is the highest of any class (87.2%). Attitudes toward refurbished batteries
are notably negative: only 30.5% agree that they are environmentally beneficial, and just 9.1% disagree that they are
functionally inferior. Infrastructure readiness is also limited: only 32.8% have home electrical outlet access, and only
24.7% report a neighbor who owns or leases a BEV or PHEV. Overall, this class is largely outside adopters for used
BEVs in the near term.

Class 6: Low-Engagement, Attribute Non-Attendance Respondents (𝑛=539, 18.5%).
The most prominent feature of this class is that it has the price coefficient closest to zero (-0.144**), which mechanically
inflates all estimated WTP ratios. The class also shows the pattern that the choice-modeling literature associates with
attribute non-attendance: one familiar attribute (mileage) carries a large, significant valuation (-$9,950** per 10,000
miles) while most others are imprecisely estimated, and completion times are the shortest of any class except Class 5
(81 seconds across six tasks). Ignoring attributes is known to distort WTP estimates (Hensher et al., 2005), and a
standard remedy is to let a latent class absorb non-attending behavior (Scarpa et al., 2009). We therefore retain Class 6

Page 15 of 29

1

2

in estimation, where it serves this function, but flag its WTP values as unreliable and exclude them from cross-class
comparisons.

Page 16 of 29

P
a
g
e

1
7

o
f

2
9

Figure 4: WTP and class profile summary.

Table 5
LCCM model results

Vehicle Attributes

No-Choice Option (opt-out)

Mileage (10,000 miles)

BEV Electric Range (Year 3): <130 miles

BEV Electric Range (Year 3): 130-200 miles

BEV Electric Range (Year 3): 200+ miles

Range Loss Rate: <12%

Range Loss Rate: 12%-24%

Range Loss Rate: 24%+

Battery Refurbishment: Pack Replace

Battery Refurbishment: Cell Replace

Purchase Price (10,000 USD)

Active Indicators

ASC

Perceived EV Range Anxiety: Agree

Risk Taking Propensity: Agree

Household Income (10,000 USD)

Electrical Outlet Access

EV Battery Knowledge: Yes

EV Battery Environmentally Positive: Agree

EV Battery Functionally Negative: Disagree

Current Primary Household Vehicle Fuel Type:
ICEV
Current Primary Vehicle Typical Range (miles)

SUV/Crossover as the Next Vehicle

Est. (SE)[Sig.]
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1.

Class 1

Class 2

Class 3

Class 4

Class 5

Class 6

-9.819
(0.813)***
-0.3
(0.041)***
0.468
(0.281).
0.472
(0.245).
0.545
(0.189)**
-0.236
(0.029)***
-0.239
(0.019)***
-0.22
(0.03)***
-0.281
(0.126)*
-0.385
(0.127)**
-1.025
(0.118)***

0.867
(0.5).
-0.021
(0.257)
0.692
(0.2)***
0.011
(0.016)
0.391
(0.191)*
0.146
(0.194)
1.209
(0.189)***
0.987
(0.278)***
-0.629
(0.268)*
-0.173
(0.091).
-0.203
(0.191)

-2.513
(0.387)***
-0.22
(0.031)***
1.426
(0.365)***
1.14
(0.22)***
1.145
(0.166)***
-0.093
(0.017)***
-0.091
(0.013)***
-0.047
(0.019)*
-0.695
(0.106)***
-0.732
(0.11)***
-1.206
(0.1)***

1.379
(0.539)*
-0.118
(0.269)
0.042
(0.207)
0 (0.018)

0.305
(0.195)
0.284
(0.197)
0.829
(0.191)***
0.745
(0.29)*
-0.596
(0.268)*
-0.113
(0.096)
-0.531
(0.191)**

-1.968
(0.75)**
-0.31
(0.058)***
5.742
(0.417)***
3.089
(0.471)***
3.78
(0.733)***
-0.129
(0.024)***
-0.102
(0.016)***
-0.069
(0.019)***
-0.08
(0.159)
-0.099
(0.145)
-1.484
(0.287)***

-0.272
(0.552)
-0.131
(0.272)
0.387
(0.214).
0.021
(0.018)
0.812
(0.205)***
0.307
(0.225)
1.295
(0.211)***
1.547
(0.284)***
-0.592
(0.279)*
0.076
(0.098)
-0.984
(0.273)***

-13.847
(1.661)***
-0.229
(0.055)***
1.128
(0.399)**
0.707
(0.357)*
0.794
(0.507)
-0.084
(0.03)**
-0.069
(0.02)***
-0.067
(0.02)***
0.041
(0.166)
0.109
(0.165)
-4.017
(0.363)***

1.28
(0.577)*
-0.513
(0.283).
0.001
(0.242)
-0.048
(0.022)*
0.423
(0.223).
0.009
(0.223)
1.186
(0.217)***
1.169
(0.303)***
-0.015
(0.332)
-0.186
(0.101).
-0.736
(0.222)***

-2.577
(1.344).
-0.472
(0.248).
0.114
(1.326)
1.884
(1.383)
1.347
(0.61)*
-0.237
(0.072)***
-0.044
(0.063)
0.035
(0.087)
-0.932
(0.431)*
-1.074
(0.417)*
-1.467
(0.652)*

0

0

0

0

0

0

0

0

0

0

0

-3.491
(0.457)***
-0.143
(0.03)***
0.177
(0.225)
-0.145
(0.176)
0.198
(0.116).
0.004
(0.015)
-0.018
(0.01).
-0.02
(0.01)*
-0.384
(0.105)***
-0.47
(0.102)***
-0.144
(0.054)**

2.641
(0.493)***
-0.734
(0.251)**
0.893
(0.204)***
0.003
(0.017)
0.525
(0.197)**
-0.183
(0.202)
0.808
(0.196)***
0.822
(0.288)**
-1.227
(0.261)***
-0.325
(0.094)***
-0.52
(0.198)**

1

2

3

4

5

6

7

8

9

10

11

12

13

5.3. Results of Opt-Out Analysis

The thematic network for the reasons for opt-out (𝑛 = 209) is plotted in Figure 5. The network visualization reveals
10 parent themes, which can be organized into four groups: general disinterest in BEVs, practical barriers (economic
barriers, charging inconvenience, range anxiety, battery concerns), psychological factors (gas vehicle enthusiasm, BEV
distrust, used vehicle distrust, environmental concerns), and limited knowledge on BEVs.

The most prevalent reason is general disinterest in BEVs (𝑛=54), expressed without reference to any concrete barrier
(e.g., “I just don’t want one”). Economic barriers (𝑛 = 52) group four sub-themes: the upfront purchase price, ongoing
maintenance and repair expenses (in which battery replacement looms large), operation costs (e.g., electricity cost),
and a generic “not worth it” value judgment. Charging inconvenience (𝑛 = 51) captures three sub-themes: insufficient
access to public chargers (especially in rural or remote areas), lack of home charging access (often tied to apartment
or rental living situations without a private outlet), and the time required to recharge relative to refueling a gasoline
vehicle. Range anxiety (𝑛 = 42) encompasses two sub-themes: insufficient range for daily driving needs, and the fear
of being stranded on long trips beyond a single charge. Battery concerns (𝑛 = 37) are anchored in three sub-themes:

Page 18 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

capacity degradation and lifespan loss, safety risks such as fire or crash damage to lithium-ion cells, and cold-weather
performance loss.

Gas vehicle enthusiasm (𝑛 = 37) reflects a positive preference for internal combustion engine vehicles, split
between an explicit love of gas engines (sound, range, refueling speed) and the absence of certain features in BEVs such
as manual transmissions. EV distrust (𝑛 = 28) captures general skepticism of the technology, comprising a sub-theme
of generic “no faith” statements and a related concern that the technology is still immature or not ready for mainstream
adoption. Used vehicle distrust (𝑛 = 13) refers specifically to skepticism toward refurbished or used BEV batteries.
Environmental concerns (𝑛 = 8) comprise a broader “green claim” skepticism (e.g., that BEVs are not actually better
for the environment than gasoline vehicles) and concerns about end-of-life battery disposal and recycling. Finally,
Limited knowledge on BEVs (𝑛 = 3) captures the smallest share who explicitly admitted insufficient familiarity to
make an informed choice.

Following Krishna (2021), we further examine the co-occurrences among barriers, that is, the extent to which the
same respondent simultaneously cites concerns from different parent themes. Among 209 responses, 62.7% (𝑛 = 131)
cited a single barrier (i.e., one sub-theme), whereas 37.3% (𝑛 = 78) cited two or more (18.2% cited two, 12.9% cited
three, and 6.2% cited four or more). Of the 78 multi-sub-theme responses, 74 cited sub-themes from different parent
themes (e.g., economic barriers and charging inconvenience) and 4 cited multiple sub-themes within the same parent
theme (e.g., lack of public chargers and charging time). To avoid over-reporting co-occurrences and over-crowding the
figure, only pairs cited by five or more respondents are drawn as dashed lines in Figure 5, with the count labeled near
each line; less frequent pairings are omitted. Theme counts report unique respondents, so sub-theme counts can sum
to slightly more than their theme count when a respondent cited multiple sub-themes within the same theme.

The most frequent co-occurrences form a practical-infeasibility cluster linking charging and range: respondents
who mention insufficient public chargers also mention long-trip range insufficiency (𝑛 = 11) or insufficient daily
range (𝑛 = 8), and charging time co-occurs with insufficient daily range (𝑛 = 7). Battery-related concerns bridge
into adjacent clusters: battery degradation co-occurs with used-vehicle distrust (𝑛 = 6), which suggests that distrust
of used BEVs is partly rooted in battery replacement concerns, and with insufficient daily range (𝑛 = 7). Gas engine
preference bridges to both practical and psychological barriers: love of gas engines co-occurs with insufficient public
chargers (𝑛 = 8), general BEV distrust (𝑛 = 5), and insufficient daily range (𝑛 = 5). Within a single theme, the only
pairing above this threshold is daily and long-trip range insufficiency (𝑛 = 7). These overlaps suggest that addressing
a single barrier (e.g., expanding public charging infrastructure) could, in principle, alleviate several adjacent concerns
at once, consistent with the cascading-effect pattern reported by Krishna (2021).

Regarding the impact of the information treatment on opt-out, the opt-out rate in the total sample is 6.81%
(=103/1,513) for the basic-information group and 6.80% (=106/1,559) for the extended-information group. A chi-
square test finds no difference in the overall opt-out rate between treatment groups (𝜒 2 ≈ 0, 𝑝 = 1.00).

Page 19 of 29

P
a
g
e

2
0

o
f

2
9

Figure 5: Thematic diagram and co-occurrences of opt-out reasons (𝑁 = 209).

Notes: 1. 𝑛 counts unique respondents and their share of the 209 opt-out respondents. A respondent citing multiple sub-themes within a theme counts once toward the
theme total, so sub-theme counts can sum to slightly more than their theme count; 2. Within each theme, sub-themes are shaded from darker to lighter in descending
order of frequency. 3. Dashed lines mark pairs of sub-themes cited together by at least five respondents, with each label reporting the number of co-citing respondents; less
frequent pairings are omitted.

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

5.4. Discussion

The DCE defined and displayed every battery-health attribute to every respondent; as a result, the WTP estimates
describe how consumers price these attributes when they can observe them, which is what we expect to happen in a
future, more mature used BEV market where battery-health information is standardized and visible at the point of
sale, rather than today’s market where this information is largely absent. The information treatment supports this
interpretation. Giving half the respondents additional detail on replacement costs and warranties changed neither
market engagement nor the broad pattern of choices. Once the attributes themselves are visible, additional warranties
did not change stated purchasing decisions.

Consumer demand, however, is not a sufficient condition for battery-health certification in the short term.
Certification at retail presupposes battery data at acquisition. When dealers source used BEVs through wholesale
channels that carry no SOH information, they cannot price battery quality when they buy a vehicle, let alone certify
it when they sell. Access to battery management system (BMS) data also remains largely gated by OEMs in the U.S.,
limiting the ability of independent operators to verify battery health and develop trusted certification mechanisms for
used BEV markets. Our estimates indicate that certification may be worth building, but they do not indicate who can or
should build it. In the U.S., OEM-backed certified pre-owned programs may be the most feasible near-term route, since
the OEM controls both the battery data and the warranty, whereas independent certifiers face a data-access constraint
that demand alone cannot lower.

In addition, the refurbishment WTP estimates warrant two caveats. First, the WTP for the cell-level and pack-
level refurbished batteries are not statistically significant in any of the pairwise tests (see Table 8). This may reflect
measurement error rather than preference. Used BEVs with refurbished batteries remain rare in retail inventory, so
these estimates describe a future market when this may be more common. Second, even though the two refurbishment
methods differ technically (Section 2), respondents encountered the distinction only through the attribute definitions,
and buyers with no market experience of battery refurbishment may simply not differentiate their implications for
battery longevity and performance. The heterogeneity results from the MXL model also soften what the penalties
mean in practice. Although the average buyer penalizes a refurbished battery, roughly a quarter of consumers do not
mind one, and some even prefer it. Credible battery-health information would therefore work in two ways: it would let
vehicles with documented healthy batteries earn higher prices from the buyers who care most about battery quality,
and it would steer refurbished vehicles toward the price-focused buyers willing to accept them. The gain comes from
matching each vehicle with the right buyer, not from raising every price.

6. Conclusion

The used BEV market is expanding, yet the persistent lack of information around battery health may continue to
dampen consumer confidence and impede efficient pricing in the secondary market. This paper provides the first WTP
estimates for battery-health attributes in the U.S. used BEV market, based on a national DCE with 3,072 respondents.
At the aggregate level, consumers value electric range, penalize mileage and battery degradation, and discount
refurbishment history. The MXL model estimates show that range is worth about $11,070 per 100 miles, degradation
costs $980 per percentage point per year, and any refurbishment history costs $4,060 on average while a quarter of
buyers disregard it. The preference heterogeneity is substantial. The LCCM results suggest that consumers fall into
six segments that differ markedly in their sensitivity to battery quality signals, their price responsiveness, and their
market engagement. Class 1 (8.7%) is defined by systematic opt-out behavior and represents consumers who are largely
outside the addressable used BEV market, with low BEV knowledge, high range anxiety, and predominantly ICEV
households. Class 2 (22.7%), the largest class, is characterized by the strongest and most uniform range loss aversion
and meaningful disutility for both refurbishment types. These well-informed consumers prioritize battery reliability
over range. Class 3 (16.6%) is the most EV-knowledgeable segment and places an exceptionally high premium on
crossing a minimum viable range threshold. Class 5 (20.8%) attends simultaneously to range adequacy, degradation
trajectory, and refurbishment condition, and opts out selectively when no presented vehicle meets its standards across
all dimensions at once. Class 6 (12.8%) is the most price-sensitive class and is composed of budget-constrained buyers
who are strongly committed to making a vehicle choice but largely indifferent to battery quality signals. Class 4 (18.5%)
is a low-engagement, attribute non-attendance class: its near-zero price sensitivity mechanically inflates WTP ratios,
so we flag its estimates as unreliable and withhold substantive interpretation.

Beyond the choice models, the thematic analysis among respondents who opted out of all six tasks shows that
market disengagement is not about battery health alone. The most common reasons are general disinterest in BEVs,

Page 21 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

economic barriers, and charging inconvenience, and over a third of these respondents cite more than one barrier. For
the most disengaged consumers, better battery-health information addresses only one item on a longer list: their return
to the market depends at least as much on charging access and affordability as on battery transparency.

This study has several limitations. The analysis relies on stated preferences from a DCE, which may not fully
replicate the decision-making dynamics of real vehicle purchases, particularly for consumers with limited prior
BEV experience. Stated-preference estimates may also overstate market valuations. Our aggregate range estimate
($11,070/100mi) sits above new-vehicle benchmarks (see Section 2). The gap is consistent with the high marginal value
of range in a short-range used fleet, and Class 3’s range valuations in particular should be read with this caution. The
large no-choice penalty (-$51,300) may imply a stronger compulsion to transact than real purchase behavior, a potential
stated-preference artifact. Moreover, the sample is restricted to individuals who expressed an intention to purchase
a used vehicle within two years, which excludes passive or uninterested populations. The LCCM assumes a finite
number of discrete classes, so unobserved heterogeneity within each class remains. Future research can examine how
these preferences evolve as used BEV supply increases and battery health reporting becomes more standardized, and
whether incentive programs aimed at improving information quality in the secondary market translate into measurable
shifts in demand.

7. Appendix

Table 6
WTP and Characteristics by Class

Class 1
(n=661,
22.7%)

Class 2
(n=606,
20.8%)

Class 3
(n=485,
16.6%)

Class 4
(n=373,
12.8%)

Class 5
(n=252,
8.7%)

Class 6
(n=539,
18.5%)

WTP (*1000 USD) for Vehicle Attributes

No-Choice Option (opt-out)
Mileage (10,000 miles)
BEV Range: 40-130 mi segment (per 100 miles)
BEV Range: 130-200 mi segment (per 100 miles)
BEV Range: 200+ mi segment (per 100 miles)
Range Loss Rate: 5-12% (per %)
Range Loss Rate: 12-24% (per %)
Range Loss Rate: 24%+ (per %)
Battery Refurbishment: Pack Replace
Battery Refurbishment: Cell Replace

Active Indicators

Risk-taking Propensity: Agree: no
Risk-taking Propensity: Agree: yes
Perceived EV Range Anxiety: Agree: no
Perceived EV Range Anxiety: Agree: yes
EV Battery Environmentally Positive: Agree: no
EV Battery Environmentally Positive: Agree: yes
EV Battery Functionally Negative: Disagree: no
EV Battery Functionally Negative: Disagree: yes
Household Income (1000 USD)
EV Knowledge: no
EV Knowledge: yes
Electrical Outlet Access: no
Electrical Outlet Access: not_sure
Electrical Outlet Access: yes
Primary Vehicle Typical Range (miles)
Household Vehicle Fuel Composition: has_bev
Household Vehicle Fuel Composition: has_phev_hev
Household Vehicle Fuel Composition: icev_only
Household Vehicle Fuel Composition: other
Next Vehicle Type: SUV: no
Next Vehicle Type: SUV: yes

-$95.8***
-$2.9***
$4.6.
$4.6.
$5.3**
-$2.3***
-$2.3***
-$2.1***
-$2.7*
-$3.8**

-$20.8***
-$1.8***
$11.8***
$9.5***
$9.5***
-$0.8***
-$0.8***
-$0.4*
-$5.8***
-$6.1***

-$13.3**
-$2.1***
$38.7***
$20.8***
$25.5***
-$0.9***
-$0.7***
-$0.5***
-$0.5
-$0.7

-$34.5***
-$0.6***
$2.8**
$1.8*
$2.0
-$0.2**
-$0.2***
-$0.2***
$0.1
$0.3

56.0%
44.0%
17.0%
83.0%
37.4%
62.6%
76.9%
23.1%
91.3
23.8%
76.2%
47.8%
6.3%
46.0%
292.2
7.5%
16.1%
76.2%
0.1%
38.9%
61.1%

71.3%
28.7%
16.6%
83.4%
47.7%
52.3%
81.2%
18.8%
86.9
23.5%
76.5%
51.8%
6.4%
41.8%
297.4
7.5%
13.3%
79.0%
0.2%
46.3%
53.7%

61.0%
39.0%
19.4%
80.6%
34.5%
65.5%
64.5%
35.5%
97.1
18.9%
81.1%
37.2%
5.3%
57.5%
321.3
9.2%
16.6%
74.1%
0.0%
57.2%
42.8%

72.4%
27.6%
22.0%
78.0%
39.6%
60.4%
72.1%
27.9%
70.0
30.7%
69.3%
52.3%
7.8%
39.8%
282.2
3.4%
9.5%
87.1%
0.0%
53.3%
46.7%

-$17.6.
-$3.2.
$0.8
$12.8
$9.2*
-$1.6*
-$0.3
$0.2
-$6.4*
-$7.3*

76.0%
24.0%
12.8%
87.2%
69.5%
30.5%
90.9%
9.1%
85.5
31.5%
68.5%
63.0%
4.1%
32.8%
306.8
1.7%
7.6%
89.3%
1.4%
31.8%
68.2%

-$242.4**
-$10.0**
$12.3
-$10.1
$13.8.
$0.3
-$1.3.
-$1.4*
-$26.6**
-$32.6**

48.1%
51.9%
33.0%
67.0%
46.4%
53.6%
78.5%
21.5%
89.3
28.4%
71.6%
42.3%
5.8%
51.9%
267.6
13.5%
25.3%
61.2%
0.0%
48.3%
51.7%

Page 22 of 29

Table 6
WTP and Characteristics by Class (continued)

Inactive Indicators

EV Subsidy Knowledge: no
EV Subsidy Knowledge: yes
Neighbor Owns/Leases a BEV/PHEV: no
Neighbor Owns/Leases a BEV/PHEV: not_sure
Neighbor Owns/Leases a BEV/PHEV: yes
Age
Gender: female
Gender: male
Gender: other
Gender: prefer_not_answer
Ethnicity: hispanic
Ethnicity: non-hispanic
Race: african_american_only
Race: other
Race: white_only
Education Level: bachelor
Education Level: graduate
Education Level: high_school
Education Level: prefer_not_answer
Education Level: some_college
Student Status: non-student
Student Status: prefer_not_answer
Student Status: student
Employment Status: full_time
Employment Status: not_employed
Employment Status: part_time
Employment Status: prefer_not_answer
Household Size
Household Tenure: other
Household Tenure: own
Household Tenure: prefer_not_answer
Household Tenure: rent
Household Type: apart
Household Type: other
Household Type: sf_attached
Household Type: sf_detached
Household Vehicle Count
Primary Vehicle Fuel Type: bev
Primary Vehicle Fuel Type: icev
Primary Vehicle Fuel Type: phev_hev
Next Vehicle Budget (1000 USD)
Likelihood of buying new BEV: neutral
Likelihood of buying new BEV: somewhat_agree
Likelihood of buying new BEV: somewhat_disagree
Likelihood of buying new BEV: strongly_agree
Likelihood of buying new BEV: strongly_disagree
Likelihood of buying used BEV: neutral
Likelihood of buying used BEV: somewhat_agree
Likelihood of buying used BEV: somewhat_disagree
Likelihood of buying used BEV: strongly_agree
Likelihood of buying used BEV: strongly_disagree
Climate Concern: no
Climate Concern: yes

Page 23 of 29

Class 1
(n=661,
22.7%)

Class 2
(n=606,
20.8%)

Class 3
(n=485,
16.6%)

Class 4
(n=373,
12.8%)

Class 5
(n=252,
8.7%)

Class 6
(n=539,
18.5%)

73.6%
26.4%
29.6%
26.9%
43.5%
36.5
65.1%
32.7%
1.9%
0.3%
11.6%
88.4%
17.9%
15.6%
66.5%
36.8%
20.2%
12.6%
0.2%
30.2%
84.1%
1.1%
14.8%
47.2%
26.0%
25.7%
1.1%
3.1
6.1%
49.6%
1.2%
43.0%
24.1%
3.2%
11.9%
60.9%
2.1
3.1%
85.3%
11.6%
27.8
10.7%
16.8%
20.4%
7.4%
44.6%
12.6%
26.2%
20.5%
10.5%
30.2%
56.5%
43.5%

78.0%
22.0%
31.8%
30.3%
37.9%
40.9
63.1%
34.4%
2.5%
0.0%
11.5%
88.5%
11.4%
18.1%
70.5%
36.1%
19.6%
11.4%
0.5%
32.4%
86.0%
1.8%
12.1%
43.1%
33.1%
21.9%
1.8%
2.9
4.9%
52.5%
1.6%
41.0%
21.9%
4.1%
10.6%
63.5%
2.0
4.3%
87.4%
8.3%
25.2
10.0%
13.1%
16.4%
4.6%
55.9%
12.3%
18.9%
19.7%
8.0%
41.2%
59.8%
40.2%

68.1%
31.9%
26.0%
26.8%
47.2%
37.5
52.6%
44.9%
2.5%
0.0%
9.8%
90.2%
14.4%
14.5%
71.2%
38.4%
20.4%
11.9%
0.0%
29.3%
84.6%
0.8%
14.6%
48.4%
27.7%
23.1%
0.8%
2.9
5.8%
54.4%
1.8%
38.1%
21.6%
3.1%
9.2%
66.0%
2.1
4.7%
84.9%
10.4%
26.9
11.3%
16.7%
22.2%
5.8%
44.0%
13.2%
26.2%
24.1%
10.1%
26.4%
57.6%
42.4%

79.1%
20.9%
33.9%
31.2%
34.9%
37.7
60.1%
36.0%
3.2%
0.7%
11.6%
88.4%
11.7%
15.9%
72.4%
31.7%
17.9%
16.0%
0.5%
33.9%
80.6%
1.9%
17.5%
38.6%
33.5%
26.1%
1.9%
2.8
7.2%
43.0%
1.0%
48.8%
27.6%
3.9%
10.3%
58.2%
1.9
1.4%
91.6%
6.9%
19.7
10.0%
12.5%
19.1%
3.3%
55.2%
13.6%
23.4%
22.0%
7.5%
33.5%
58.4%
41.6%

89.4%
10.6%
46.9%
28.3%
24.7%
47.1
67.3%
31.3%
1.4%
0.0%
5.3%
94.7%
7.1%
10.4%
82.5%
31.9%
17.0%
18.6%
0.0%
32.6%
92.4%
0.7%
6.9%
38.2%
36.5%
24.6%
0.7%
2.8
6.1%
58.3%
1.6%
34.1%
17.2%
4.7%
9.2%
68.9%
2.0
0.9%
93.3%
5.8%
24.3
5.2%
5.3%
10.6%
1.7%
77.2%
5.5%
6.3%
14.7%
3.2%
70.4%
78.3%
21.7%

70.9%
29.1%
28.5%
20.6%
50.9%
37.4
57.9%
41.3%
0.8%
0.1%
15.5%
84.5%
22.3%
18.5%
59.2%
32.9%
20.4%
16.8%
0.1%
29.8%
84.0%
0.9%
15.1%
46.9%
29.4%
22.8%
0.9%
3.3
4.7%
49.5%
1.8%
43.9%
26.8%
3.2%
12.4%
57.6%
2.1
6.1%
72.0%
21.9%
29.3
14.8%
22.3%
16.9%
11.1%
34.9%
16.7%
26.7%
18.0%
11.1%
27.6%
62.4%
37.6%

Table 7
Metadata Summary by Class

Metadata: Information Treatment
Battery Information Treatment: no
Battery Information Treatment: yes

Metadata: Opt-out Count

Opt-out: 0 times
Opt-out: 1 time
Opt-out: 2 times
Opt-out: 3 times
Opt-out: 4 times
Opt-out: 5 times
Opt-out: 6 times

Metadata: Survey Duration
Battery DCE Section: Q1
Battery DCE Section: Q2
Battery DCE Section: Q3
Battery DCE Section: Q4
Battery DCE Section: Q5
Battery DCE Section: Q6
Battery DCE Section: Total
Full Survey

Class 1
(n=661,
22.7%)

Class 2
(n=606,
20.8%)

Class 3
(n=485,
16.6%)

Class 4
(n=373,
12.8%)

Class 5
(n=252,
8.7%)

Class 6
(n=539,
18.5%)

49.3%
50.7%

86.4%
12.0%
1.6%
0.1%
0.0%
0.0%
0.0%

25.0
15.0
14.0
13.0
12.0
12.0
99.0
924.0

48.3%
51.7%

5.2%
18.6%
29.0%
27.2%
15.3%
4.1%
0.6%

27.0
16.0
14.0
13.0
12.0
11.0
102.0
894.0

50.7%
49.3%

85.7%
10.7%
2.9%
0.6%
0.0%
0.0%
0.0%

27.0
16.0
15.0
15.0
13.0
13.0
107.0
907.0

50.3%
49.7%

91.2%
7.4%
1.1%
0.3%
0.0%
0.0%
0.0%

25.0
15.0
13.0
13.0
12.0
11.0
97.0
842.0

51.6%
48.4%

0.0%
0.0%
0.0%
0.4%
4.8%
19.4%
75.4%

22.0
9.0
7.0
7.0
6.0
6.0
63.0
896.0

47.3%
52.7%

85.0%
12.2%
2.7%
0.1%
0.0%
0.0%
0.0%

22.0
14.0
12.0
12.0
11.0
11.0
93.0
952.0

Table 8
Pairwise WTP differences across attribute levels, by latent class

Class 1
(n=661,
22.7%)

Class 2
(n=606,
20.8%)

Class 3
(n=485,
16.6%)

Class 4
(n=373,
12.8%)

Class 5
(n=252,
8.7%)

Class 6
(n=539,
18.5%)

BEV Electric Range (Year 3, per 100 miles): Pairwise WTP Differences

40–130 mi minus 130–200 mi
40–130 mi minus 200+ mi
130–200 mi minus 200+ mi

-$40
-$740
-$700

$2,380
$2,330
-$50

$17,880**
$13,230.
-$4,650

$1,050
$830
-$220

-$12,060
-$8,400
$3,660

$22,370
-$1,450
-$23,820

Range Loss Rate (per percentage point): Pairwise WTP Differences

<12% minus 12–24%
<12% minus 24%+
12–24% minus 24%+

$20
-$170
-$190

-$20
-$390*
-$370

-$180
-$400*
-$220

-$40
-$40
$0

-$1,320
-$1,860
-$540

$1,520
$1,640
$120

Battery Refurbishment: Pairwise WTP Differences

Pack replace minus Cell replace

$1,020

$310

$130

-$170

$970

$5,980

Each cell reports the WTP difference in dollars between two attribute levels; a positive value means the first level carries higher WTP (or lower
disutility).
Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1.

1

2

Page 24 of 29

P
a
g
e

2
5

o
f

2
9

Table 9
Monetized WTP estimates for vehicle and battery attributes in prior studies.

Study

Market and sample

Driving range

Charging

Operating cost

Other monetized attributes

Hidrue et al. (2011)

U.S.; potential BEV buyers

$35–$75 per additional mile

$425–$3,250 per hour reduction in
charging time

About $2,706 for a
$1/gallon-equivalent reduction

Helveston et al. (2015)

U.S. and China; new HEV,
PHEV, and BEV alternatives

Not estimated separately; range
enters through BEV75/100/150
alternatives. Implied U.S. value
about $76 per mile (derived, not
reported)

Fast charging: about $3,331 (U.S.,
PHEV; insignificant for BEV); about
$7,567 (PHEV) and $6,428 (BEV) in
China

About $1,600 (U.S.) and
$3,000–$3,500 (China) per
$0.01/mile reduction

Forsythe et al. (2023)

U.S.; new car and SUV
buyers (gasoline, HEV,
PHEV, BEV)

$5,120 (cars) and $7,010 (SUVs) per
100 miles of BEV range

Fast-charging capability: $4,140
(cars) and $4,110 (SUVs)

$1,960 (cars) and $1,490 (SUVs)
per $0.01/mile saved

Dimitropoulos et al.
(2013)

Meta-analysis of 33 BEV and
AFV stated-preference
studies

Mean $66–$75 per additional mile;
marginal WTP declines as range
increases

Tanaka et al. (2014)

U.S. and Japan; BEV and
PHEV choice

About $21.5 per 10 miles

—

—

—

$49.8 (U.S.) and $36.7 (Japan)
per 1% fuel-cost reduction
relative to gasoline

Acceleration: $2,600–$7,300 by improvement
level. Pollution reduction: $1,900–$4,300+ by
reduction level.

BEV technology penalty: U.S. consumers
value BEVs $10,000–$20,000 below a
comparable conventional vehicle, depending
on range. Acceleration: about $1,200 (U.S.)
and $5,000 (China) per one-second
improvement.

Acceleration: $1,470 (cars) and $1,440
(SUVs) per one-second 0–60 mph reduction.
BEV powertrain penalty: $4,160 (cars) and
$8,700 (SUVs) relative to a comparable
gasoline vehicle.

A 100-mile-range vehicle may need to be
$13,000–$17,000 cheaper than a long-range
conventional counterpart to be equally
attractive.

Fuel-station availability: $49.8 (U.S.) and
$33.6 (Japan) per percentage point. Emissions
reduction: $29.0 (U.S.) and $26.2 (Japan) per
percentage point. Home plug-in installation:
−$21.3 (U.S.) and −$16.9 (Japan) per $10 of
installation cost.

Greene et al. (2018)

U.S.; pooled review of 52
vehicle-choice studies (2015
dollars; not BEV-specific)

Pooled mean $86 per mile across
range variables

Pooled mean $2,195 per hour
reduction in recharging time

Pooled mean $1,880 per
$0.01/mile reduction

Acceleration: pooled mean $954 per
one-second 0–60 mph reduction.

Ferguson et al. (2018)

Canada; latent class analysis
(ICE-, HEV-, PHEV-, and
BEV-oriented classes)

About $31/km (PHEV-oriented
class) and $30/km (BEV-oriented
class)

Public charging time saved:
$531–$1,971 per hour across classes.
Station availability: $317–$1,122 per
step improvement (highest for the
BEV-oriented class)

—

Battery warranty: $800–$3,153 per
warranty-level improvement across classes,
highest for HEV-oriented households, lowest
for BEV-oriented households, who appear
most comfortable with battery risk.

Notes: Values are nominal in each study’s survey-year currency; cross-study comparison requires inflation adjustment. — indicates the attribute was not reported or not a focus of the
study.

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

52

53

54

55

56

57

CRediT authorship contribution statement

Xiatian Iogansen: Conceptualization, Data curation, Methodology, Software, Formal analysis, Writing - Original
Draft, Writing - Review & Editing. Zain Hoda: Conceptualization, Data curation, Methodology, Writing - Review
& Editing. Christina Gore: Conceptualization, Data curation, Methodology, Investigation, Writing - Original draft
preparation, Writing - Review & Editing, Supervision, Project administration, Funding acquisition. Joshua D. Kneifel:
Conceptualization, Data curation, Methodology, Investigation, Writing - Review & Editing, Supervision, Project
administration, Funding acquisition. Sindhu Ranganath: Writing - Original Draft, Writing - Review & Editing. John
P. Helveston: Conceptualization, Methodology, Writing - Review & Editing, Supervision.

References
Abbey, J. D., M. G. Meloy, V. D. R. Guide Jr., and S. Atalay (2015). “Remanufactured Products in Closed-Loop Supply Chains for Consumer
Goods”. In: Production and Operations Management 24.3, pp. 488–503. ISSN: 1937-5956. DOI: 10 . 1111 / poms . 12238. URL: https :
//onlinelibrary.wiley.com/doi/abs/10.1111/poms.12238 (visited on 07/06/2026).

Attia, P. M., A. Bills, F. Brosa Planella, P. Dechent, G. dos Reis, M. Dubarry, P. Gasper, R. Gilchrist, S. Greenbank, D. Howey, O. Liu, E. Khoo,
Y. Preger, A. Soni, S. Sripad, A. G. Stefanopoulou, and V. Sulzer (June 2022). “Review—“Knees” in Lithium-Ion Battery Aging Trajectories”.
In: Journal of The Electrochemical Society 169.6, p. 060517. ISSN: 1945-7111. DOI: 10.1149/1945- 7111/ac6d13. URL: https://dx.
doi.org/10.1149/1945-7111/ac6d13 (visited on 07/03/2025).

Bashash, S., S. J. Moura, J. C. Forman, and H. K. Fathy (Jan. 1, 2011). “Plug-in Hybrid Electric Vehicle Charge Pattern Optimization for Energy
Cost and Battery Longevity”. In: Journal of Power Sources 196.1, pp. 541–549. ISSN: 0378-7753. DOI: 10.1016/j.jpowsour.2010.07.001.
URL: https://www.sciencedirect.com/science/article/pii/S0378775310011390 (visited on 07/03/2025).

Bilfinger, P., M. Schreiber, P. Rosner, K. A. Gamra, J. Schöberl, C. Grosu, and M. Lienkamp (Aug. 21, 2025). Why We Need a Standardized State
of Health Definition for Electric Vehicle Battery Packs – a Proposal for Energy- and Capacity-Based Metrics. DOI: 10.48550/arXiv.2508.
15517. arXiv: 2508.15517 [eess.SY]. URL: http://arxiv.org/abs/2508.15517 (visited on 07/01/2026). Pre-published.

Boudway, I. (Dec. 16, 2020). “Batteries For Electric Cars Speed Toward a Tipping Point”. In: Bloomberg.com. URL: https://www.bloomberg.
com / news / articles / 2020 - 12 - 16 / electric - cars - are - about - to - be - as - cheap - as - gas - powered - models (visited on
07/03/2025).

Braun, V. and V. Clarke (Jan. 1, 2006). “Using Thematic Analysis in Psychology”. In: Qualitative Research in Psychology 3.2, pp. 77–101. ISSN:
1478-0887. DOI: 10.1191/1478088706qp063oa. URL: https://doi.org/10.1191/1478088706qp063oa (visited on 06/28/2026).
Brückmann, G., M. Wicki, and T. Bernauer (Nov. 2021). “Is Resale Anxiety an Obstacle to Electric Vehicle Adoption? Results from a Survey
Experiment in Switzerland”. In: Environmental Research Letters 16.12, p. 124027. ISSN: 1748-9326. DOI: 10.1088/1748- 9326/ac3531.
URL: https://dx.doi.org/10.1088/1748-9326/ac3531 (visited on 06/30/2025).

Canals Casals, L., M. Rodríguez, C. Corchero, and R. E. Carrillo (Dec. 2019). “Evaluation of the End-of-Life of Electric Vehicle Batteries According
to the State-of-Health”. In: World Electric Vehicle Journal 10.4 (4), p. 63. ISSN: 2032-6653. DOI: 10.3390/wevj10040063. URL: https:
//www.mdpi.com/2032-6653/10/4/63 (visited on 07/02/2025).

Canepa, K., S. Hardman, and G. Tal (June 1, 2019). “An Early Look at Plug-in Electric Vehicle Adoption in Disadvantaged Communities in
California”. In: Transport Policy 78, pp. 19–30. ISSN: 0967-070X. DOI: 10 . 1016 / j . tranpol . 2019 . 03 . 009. URL: https : / / www .
sciencedirect.com/science/article/pii/S0967070X18303524 (visited on 07/01/2025).

Chen, C.-F., C. Fu, and Y.-C. Chen (Aug. 2023). “Exploring Tourist Preference for Mobility-as-a-Service (MaaS) – A Latent Class Choice
Approach”. In: Transportation Research Part A: Policy and Practice 174, p. 103750. ISSN: 09658564. DOI: 10.1016/j.tra.2023.103750.
URL: https://linkinghub.elsevier.com/retrieve/pii/S0965856423001702 (visited on 05/18/2026).

Clarke, W. (2024). Car Warranty Coverage on an Electric Car Battery. US News & World Report. URL: https://cars.usnews.com/cars-

trucks/advice/ev-battery-warranty (visited on 07/02/2025).

Cox Automotive Inc. (2025). EV Market Monitor – February 2025. URL: https://www.coxautoinc.com/market-insights/ev-market-

monitor-february-2025/ (visited on 06/30/2025).

Dimitropoulos, A., P. Rietveld, and J. N. van Ommeren (Sept. 1, 2013). “Consumer Valuation of Changes in Driving Range: A Meta-Analysis”.
In: Transportation Research Part A: Policy and Practice 55, pp. 27–45. ISSN: 0965-8564. DOI: 10 . 1016 / j . tra . 2013 . 08 . 001. URL:
https://www.sciencedirect.com/science/article/pii/S096585641300147X (visited on 07/07/2026).

Dnistran, I. (2024). EV Batteries Can Outlast A Vehicle’s Lifetime With Minimal Degradation, Study Finds. InsideEVs. URL: https://insideevs.

com/news/733987/ev-batteries-outlast-vehicle-degradation-study/ (visited on 07/02/2025).

Ferguson, M., M. Mohamed, C. D. Higgins, E. Abotalebi, and P. Kanaroglou (Jan. 2018). “How Open Are Canadian Households to Electric Vehicles?
A National Latent Class Choice Analysis with Willingness-to-Pay and Metropolitan Characterization”. In: Transportation Research Part D:
Transport and Environment 58, pp. 208–224. ISSN: 13619209. DOI: 10 . 1016 / j . trd . 2017 . 12 . 006. URL: https : / / linkinghub .
elsevier.com/retrieve/pii/S1361920916306058 (visited on 11/06/2025).

Fernandes, J. (2023). US: Understanding Consumer Opinion about Used Electric Cars. URL: https://business.yougov.com/content/

47834-us-understanding-consumer-opinion-about-used-electric-cars (visited on 06/30/2025).

Forsythe, C. R., K. T. Gillingham, J. J. Michalek, and K. S. Whitefoot (June 6, 2023). “Technology Advancement Is Driving Electric Vehicle
Adoption”. In: Proceedings of the National Academy of Sciences 120.23, e2219396120. DOI: 10.1073/pnas.2219396120. URL: https:
//www.pnas.org/doi/10.1073/pnas.2219396120 (visited on 07/08/2026).

Page 26 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

52

53

54

55

56

57

58

59

60

61

62

Foster, M., P. Isely, C. R. Standridge, and M. M. Hasan (Oct. 31, 2014). “Feasibility assessment of remanufacturing, repurposing, and recycling
of end of vehicle application lithium-ion batteries”. In: Journal of Industrial Engineering and Management. ISSN: 2013-0953. URL: https:
//raco.cat/index.php/JIEM/article/view/282032 (visited on 07/06/2026).

Gore, C., X. Iogansen, J. Kneifel, and J. Helveston (Nov. 18, 2025). “DCI: Consumer Valuation of Battery Health Information in Used Electric Vehicle
Markets”. In: NIST. URL: https://www.nist.gov/publications/dci- consumer- valuation- battery- health- information-
used-electric-vehicle-markets (visited on 05/11/2026).

Greene, D. L., A. Hossain, J. Hofmann, G. Helfand, and R. Beach (2018). “Consumer Willingness to Pay for Vehicle Attributes: What Do We

Know?” In: Transportation Research Part A: Policy and Practice 118, pp. 258–279.

Greene, W. H. and D. A. Hensher (2003). “A Latent Class Model for Discrete Choice Analysis: Contrasts with Mixed Logit”. In: Transportation
Research Part B: Methodological 37.8, pp. 681–698. URL: https://ideas.repec.org//a/eee/transb/v37y2003i8p681-698.html
(visited on 05/18/2026).

Guzek, M., J. Jackowski, R. S. Jurecki, E. M. Szumska, P. Zdanowicz, and M. Żmuda (Jan. 2024). “Electric Vehicles—An Overview of Current
Issues—Part 1—Environmental Impact, Source of Energy, Recycling, and Second Life of Battery”. In: Energies 17.1 (1), p. 249. ISSN: 1996-
1073. DOI: 10.3390/en17010249. URL: https://www.mdpi.com/1996-1073/17/1/249 (visited on 07/01/2025).

Hackbarth, A. and R. Madlener (Mar. 1, 2016). “Willingness-to-Pay for Alternative Fuel Vehicle Characteristics: A Stated Choice Study for
Germany”. In: Transportation Research Part A: Policy and Practice 85, pp. 89–111. ISSN: 0965-8564. DOI: 10.1016/j.tra.2015.12.005.
URL: https://www.sciencedirect.com/science/article/pii/S0965856415002797 (visited on 07/07/2026).

Haghani, M., M. C. J. Bliemer, J. M. Rose, H. Oppewal, and E. Lancsar (Dec. 1, 2021). “Hypothetical Bias in Stated Choice Experiments: Part
II. Conceptualisation of External Validity, Sources and Explanations of Bias and Effectiveness of Mitigation Methods”. In: Journal of Choice
Modelling 41, p. 100322. ISSN: 1755-5345. DOI: 10.1016/j.jocm.2021.100322. URL: https://www.sciencedirect.com/science/
article/pii/S1755534521000555 (visited on 07/02/2026).

Hagman, J., S. Ritzén, J. J. Stier, and Y. Susilo (Mar. 1, 2016). “Total Cost of Ownership and Its Potential Implications for Battery Electric Vehicle
Diffusion”. In: Research in Transportation Business & Management. Innovations in Technologies for Sustainable Transport 18, pp. 11–17.
ISSN: 2210-5395. DOI: 10 . 1016 / j . rtbm . 2016 . 01 . 003. URL: https : / / www . sciencedirect . com / science / article / pii /
S2210539516000043 (visited on 07/01/2025).

Helveston, J. (Oct. 20, 2025). cbcTools: Design and Analyze Choice-Based Conjoint Experiments. Version 0.7.1. URL: https : / / cran . r -

project.org/web/packages/cbcTools/index.html (visited on 10/22/2025).

Helveston, J. P. (Feb. 7, 2023). “Logitr: Fast Estimation of Multinomial and Mixed Logit Models with Preference Space and Willingness-to-Pay
Space Utility Parameterizations”. In: Journal of Statistical Software 105, pp. 1–37. ISSN: 1548-7660. DOI: 10.18637/jss.v105.i10. URL:
https://doi.org/10.18637/jss.v105.i10 (visited on 06/24/2025).

Helveston, J. P., E. M. Feit, and J. J. Michalek (2018). “Pooling Stated and Revealed Preference Data in the Presence of RP Endogeneity”. In:
Transportation Research Part B: Methodological 109, pp. 70–89. URL: https://www.sciencedirect.com/science/article/pii/
S0191261516304647 (visited on 06/05/2026).

Helveston, J. P., Y. Liu, E. M. Feit, E. Fuchs, E. Klampfl, and J. J. Michalek (Mar. 1, 2015). “Will Subsidies Drive Electric Vehicle Adoption?
Measuring Consumer Preferences in the U.S. and China”. In: Transportation Research Part A: Policy and Practice 73, pp. 96–112. ISSN: 0965-
8564. DOI: 10.1016/j.tra.2015.01.002. URL: https://www.sciencedirect.com/science/article/pii/S0965856415000038
(visited on 07/01/2026).

Hensher, D. A., J. Rose, and W. H. Greene (May 1, 2005). “The Implications on Willingness to Pay of Respondents Ignoring Specific Attributes”. In:
Transportation 32.3, pp. 203–222. ISSN: 1572-9435. DOI: 10.1007/s11116-004-7613-8. URL: https://doi.org/10.1007/s11116-
004-7613-8 (visited on 07/09/2026).

Hess, S. and D. Palma (2019). “Apollo: A flexible, powerful and customisable freeware package for choice model estimation and application”. In:

Journal of choice modelling 32, p. 100170.

Hidrue, M. K., G. R. Parsons, W. Kempton, and M. P. Gardner (Sept. 1, 2011). “Willingness to Pay for Electric Vehicles and Their Attributes”.
In: Resource and Energy Economics 33.3, pp. 686–705. ISSN: 0928-7655. DOI: 10 . 1016 / j . reseneeco . 2011 . 02 . 002. URL: https :
//www.sciencedirect.com/science/article/pii/S0928765511000200 (visited on 07/01/2026).

Iogansen, X., C. Gore, J. Kneifel, S. Ranganath, and J. P. Helveston (Apr. 1, 2026). “Consumers ’ Perceptions, Knowledge, and Adoption Patterns
of Battery Electric Vehicles”. In: Transportation Research Part D: Transport and Environment 153, p. 105194. ISSN: 1361-9209. DOI: 10.
1016/j.trd.2025.105194. URL: https://www.sciencedirect.com/science/article/pii/S1361920925006042 (visited on
07/01/2026).

Iogansen, X., K. Wang, D. Bunch, G. Matson, and G. Circella (Feb. 1, 2023). “Deciphering the Factors Associated with Adoption of Alternative
Fuel Vehicles in California: An Investigation of Latent Attitudes, Socio-Demographics, and Neighborhood Effects”. In: Transportation Research
Part A: Policy and Practice 168, p. 103535. ISSN: 0965-8564. DOI: 10.1016/j.tra.2022.10.012. URL: https://www.sciencedirect.
com/science/article/pii/S0965856422002701 (visited on 06/10/2026).

Jiao, N. and S. Evans (Jan. 1, 2016). “Business Models for Sustainability: The Case of Second-life Electric Vehicle Batteries”. In: Procedia CIRP.
13th Global Conference on Sustainable Manufacturing – Decoupling Growth from Resource Use 40, pp. 250–255. ISSN: 2212-8271. DOI:
10.1016/j.procir.2016.01.114. URL: https://www.sciencedirect.com/science/article/pii/S2212827116001293 (visited
on 07/07/2025).

Kamakura, W. A. and G. J. Russell (Nov. 1, 1989). “A Probabilistic Choice Model for Market Segmentation and Elasticity Structure”. In: Journal
of Marketing Research 26.4, pp. 379–390. ISSN: 0022-2437. DOI: 10.1177/002224378902600401. URL: https://doi.org/10.1177/
002224378902600401 (visited on 05/18/2026).

Kothari, S. (2024). Battery Replacement Costs Are Poised To Plunge: ’Cheaper Than Fixing An Engine’. InsideEVs. URL: https://insideevs.

com/news/742022/battery-replacement-costs-fall-cheaper-than-fixing-engine/ (visited on 03/31/2025).

Page 27 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

22

23

24

25

26

27

28

29

30

31

32

33

34

35

36

37

38

39

40

41

42

43

44

45

46

47

48

49

50

51

52

53

54

55

56

57

58

59

60

61

62

63

Krishna, G. (June 1, 2021). “Understanding and Identifying Barriers to Electric Vehicle Adoption through Thematic Analysis”. In: Transportation
Research Interdisciplinary Perspectives 10, p. 100364. ISSN: 2590-1982. DOI: 10 . 1016 / j . trip . 2021 . 100364. URL: https : / / www .
sciencedirect.com/science/article/pii/S2590198221000713 (visited on 07/01/2025).

Letmathe, P. and M. Suares (Dec. 1, 2017). “A Consumer-Oriented Total Cost of Ownership Model for Different Vehicle Types in Germany”. In:
Transportation Research Part D: Transport and Environment 57, pp. 314–335. ISSN: 1361-9209. DOI: 10.1016/j.trd.2017.09.007. URL:
https://www.sciencedirect.com/science/article/pii/S1361920917304868 (visited on 07/07/2025).

Lim, M. K., H.-Y. Mak, and Y. Rong (Feb. 2015). “Toward Mass Adoption of Electric Vehicles: Impact of the Range and Resale Anxieties”. In:
Manufacturing & Service Operations Management 17.1, pp. 101–119. ISSN: 1523-4614. DOI: 10.1287/msom.2014.0504. URL: https:
//pubsonline.informs.org/doi/10.1287/msom.2014.0504 (visited on 06/30/2025).

Loh, W. S. and R. B. Noland (Mar. 1, 2024). “Concerns Expressed by Used Electric Vehicle Owners Based on Surveying Social Media”. In:
Transportation Research Part D: Transport and Environment 128, p. 104086. ISSN: 1361-9209. DOI: 10.1016/j.trd.2024.104086. URL:
https://www.sciencedirect.com/science/article/pii/S1361920924000439 (visited on 07/01/2026).

Lohawala, N. and M. A. Rahman (May 1, 2026). “Do Determinants of EV Purchase Intent Vary across the Spectrum? Evidence from Bayesian
Analysis of US Survey Data”. In: Transportation Research Part A: Policy and Practice 207, p. 104961. ISSN: 0965-8564. DOI: 10.1016/j.
tra.2026.104961. URL: https://www.sciencedirect.com/science/article/pii/S0965856426001023 (visited on 07/01/2026).

Market Research Future (2025). United States Used Car Market Trends & Growth By 2034. URL: https://www.marketresearchfuture.com/

reports/us-used-car-market-21771 (visited on 07/01/2025).

Mashrur, Sk. Md. and M. Mohamed (Feb. 1, 2025). “Uncovering Factors Affecting Consumers’ Decisions for Pre-Owned Electric Vehicles”. In:
Transportation Research Part D: Transport and Environment 139, p. 104555. ISSN: 1361-9209. DOI: 10.1016/j.trd.2024.104555. URL:
https://www.sciencedirect.com/science/article/pii/S1361920924005121 (visited on 11/18/2025).

McFadden, D. and K. Train (Sept. 2000). “Mixed MNL Models for Discrete Response”. In: Journal of Applied Econometrics 15.5, pp. 447–470.
ISSN: 0883-7252, 1099-1255. DOI: 10 . 1002 / 1099 - 1255(200009 / 10 ) 15 : 5<447 :: AID - JAE570 > 3 . 0 . CO ; 2 - 1. URL: https :
//onlinelibrary.wiley.com/doi/10.1002/1099- 1255(200009/10)15:5%3C447::AID- JAE570%3E3.0.CO;2- 1 (visited on
06/05/2026).

Naseri, H., E. O. D. Waygood, Z. Patterson, and B. Wang (Sept. 1, 2024). “Who Is More Likely to Buy Electric Vehicles?” In: Transport Policy 155,
pp. 15–28. ISSN: 0967-070X. DOI: 10.1016/j.tranpol.2024.06.013. URL: https://www.sciencedirect.com/science/article/
pii/S0967070X24001756 (visited on 07/01/2025).

Neubauer, J., A. Brooker, and E. Wood (July 1, 2012). “Sensitivity of Battery Electric Vehicle Economics to Drive Patterns, Vehicle Range, and
Charge Strategies”. In: Journal of Power Sources 209, pp. 269–277. ISSN: 0378-7753. DOI: 10.1016/j.jpowsour.2012.02.107. URL:
https://www.sciencedirect.com/science/article/pii/S0378775312005290 (visited on 07/03/2025).

Nylund, K. L., T. Asparouhov, and B. O. Muthén (Oct. 23, 2007). “Deciding on the Number of Classes in Latent Class Analysis and Growth
Mixture Modeling: A Monte Carlo Simulation Study”. In: Structural Equation Modeling: A Multidisciplinary Journal 14.4, pp. 535–569.
ISSN: 1070-5511. DOI: 10.1080/10705510701575396. URL: https://doi.org/10.1080/10705510701575396 (visited on 06/05/2026).
Park, J., K. Kim, S. Geum, J. Lee, H. Son, and S. Han (Mar. 23, 2026). Battery Health Reporting Fails Independent Validation across Manufacturers.
DOI: 10 . 48550 / arXiv . 2603 . 21592. arXiv: 2603 . 21592 [eess.SP]. URL: http : / / arxiv . org / abs / 2603 . 21592 (visited on
07/01/2026). Pre-published.

Pedrosa, G. and H. Nobre (Jan. 2018). “Second-Hand Electrical Vehicles: A First Look at the Secondary Market of Modern EVs”. In: International
Journal of Electric and Hybrid Vehicles 10.3, pp. 236–252. ISSN: 1751-4088. DOI: 10.1504/IJEHV.2018.097378. URL: https://www.
inderscienceonline.com/doi/abs/10.1504/IJEHV.2018.097378 (visited on 07/01/2025).

Scarpa, R., T. J. Gilbride, D. Campbell, and D. A. Hensher (June 1, 2009). “Modelling Attribute Non-Attendance in Choice Experiments for Rural
Landscape Valuation”. In: European Review of Agricultural Economics 36.2, pp. 151–174. ISSN: 0165-1587. DOI: 10.1093/erae/jbp012.
URL: https://doi.org/10.1093/erae/jbp012 (visited on 07/09/2026).

Shaikh, W. A., M. A. Kalwar, M. A. Khan, A. N. Wassan, M. H. Wadho, and M. F. Shahzad (2023). “A System Dynamics Costing Model for The
Refurbishment of Electric Vehicle Batteries”. In: Jordan Journal of Mechanical and Industrial Engineering 17.01, pp. 15–32. ISSN: 1995-6665,
1995-6665. DOI: 10.59038/jjmie/170102. URL: http://jjmie.hu.edu.jo/vol17/vol17-1/02.htm (visited on 07/02/2025).

Sheykhfard, A., M. Azmoodeh, S. Das, and B. Kutela (Jan. 1, 2025). “Analyzing Purchase Intentions of Used Electric Vehicles through Consumer
Experiences: A Structural Equation Modeling Approach”. In: Transport Policy 160, pp. 125–137. ISSN: 0967-070X. DOI: 10 . 1016 / j .
tranpol . 2024 . 10 . 038. URL: https : / / www . sciencedirect . com / science / article / pii / S0967070X24003305 (visited on
07/01/2025).

Skeete, J.-P., P. Wells, X. Dong, O. Heidrich, and G. Harper (Nov. 1, 2020). “Beyond the EVent Horizon: Battery Waste, Recycling, and Sustainability
in the United Kingdom Electric Vehicle Transition”. In: Energy Research & Social Science 69, p. 101581. ISSN: 2214-6296. DOI: 10.1016/j.
erss.2020.101581. URL: https://www.sciencedirect.com/science/article/pii/S2214629620301572 (visited on 07/07/2025).
Sonar, H., H. M. Belal, C. Foropon, R. Manatkar, and V. Sonwaney (July 20, 2023). “Examining the Causal Factors of the Electric Vehicle Adoption:
A Pathway to Tackle Climate Change in Resource-Constrained Environment”. In: Annals of Operations Research. ISSN: 1572-9338. DOI:
10.1007/s10479-023-05519-8. URL: https://doi.org/10.1007/s10479-023-05519-8 (visited on 07/01/2025).

Tal, G., M. A. Nicholas, and T. S. Turrentine (2017). “First Look at the Plug-in Vehicle Secondary Market”. In: URL: https://escholarship.

org/uc/item/22p191zs (visited on 07/01/2026).

Tanaka, M., T. Ida, K. Murakami, and L. Friedman (Dec. 1, 2014). “Consumers’ Willingness to Pay for Alternative Fuel Vehicles: A Comparative
Discrete Choice Analysis between the US and Japan”. In: Transportation Research Part A: Policy and Practice 70, pp. 194–209. ISSN: 0965-
8564. DOI: 10.1016/j.tra.2014.10.019. URL: https://www.sciencedirect.com/science/article/pii/S0965856414002614
(visited on 07/07/2026).

Tankou, A., G. Bieker, and D. Hall (2023). Scaling up Reuse and Recycling of Electric Vehicle Batteries: Assessing Challenges and Policy

Approaches.

Page 28 of 29

1

2

3

4

5

6

7

8

9

10

11

12

13

14

15

16

17

18

19

20

21

Tire Review Staff (Apr. 21, 2026). EV Battery Repair vs Replacement Explained. Tire Review Magazine. URL: https://www.tirereview.com/

ev-battery-repair-vs-replacement/ (visited on 07/06/2026).

Train, K. E. (2009). Discrete Choice Methods with Simulation. 2nd ed. Cambridge: Cambridge University Press. ISBN: 978-0-521-76655-5. DOI:
10.1017/CBO9780511805271. URL: https://www.cambridge.org/core/books/discrete-choice-methods-with-simulation/
49CABD00F3DDDA088A8FBFAAAD7E9546 (visited on 06/05/2026).

Walker, J. L., Y. Wang, M. Thorhauge, and M. Ben-Akiva (2018). “D-efficient or deficient? A robustness analysis of stated choice experimental

designs”. In: Theory and Decision 84.2, pp. 215–238.

Webb, D., C. Gore, and J. Kneifel (Jan. 30, 2025). Consumer Perspectives on Battery Electric Vehicles : An Analysis of Battery Information and
Knowledge. NIST TN 2328. Gaithersburg, MD: National Institute of Standards and Technology (U.S.), NIST TN 2328. DOI: 10.6028/NIST.
TN.2328. URL: https://nvlpubs.nist.gov/nistpubs/TechnicalNotes/NIST.TN.2328.pdf (visited on 07/06/2026).

Witt, J. (2024). Electric Car Battery Replacement Costs. URL: https : / / www . recurrentauto . com / research / costs - ev - battery -

replacement?utm_source=chatgpt.com (visited on 03/31/2025).

Yang, F., Y. Xie, Y. Deng, and C. Yuan (June 21, 2018). “Predictive Modeling of Battery Degradation and Greenhouse Gas Emissions from U.S.
State-Level Electric Vehicle Operation”. In: Nature Communications 9.1, p. 2429. ISSN: 2041-1723. DOI: 10.1038/s41467-018-04826-0.
URL: https://www.nature.com/articles/s41467-018-04826-0 (visited on 07/03/2025).

Yuan, Q., W. Hao, H. Su, G. Bing, X. Gui, and A. Safikhani (2018). “Investigation on Range Anxiety and Safety Buffer of Battery Electric Vehicle
Drivers”. In: Journal of Advanced Transportation 2018.1, p. 8301209. ISSN: 2042-3195. DOI: 10 . 1155 / 2018 / 8301209. URL: https :
//onlinelibrary.wiley.com/doi/abs/10.1155/2018/8301209 (visited on 07/07/2025).

Zou, T., M. Khaloei, and D. MacKenzie (Dec. 1, 2020). “Effects of Charging Infrastructure Characteristics on Electric Vehicle Preferences of New
and Used Car Buyers in the United States”. In: Transportation Research Record 2674.12, pp. 165–175. ISSN: 0361-1981. DOI: 10 . 1177 /
0361198120952792. URL: https://doi.org/10.1177/0361198120952792 (visited on 07/01/2025).

Page 29 of 29

