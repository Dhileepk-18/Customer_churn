# Project Report
# Predicting Customer Churn: A Rigorous Probability and Statistics Investigation

**Project Title:** Predicting Customer Churn  
**Author / Lab Team:** K.Dhileep kumar  
**Environment:** R (v4.6.1), RStudio, R Shiny Dashboard  
**Dataset:** Telco Customer Churn Dataset (IBM Sample Data)  

---

## 1. Executive Summary

Customer attrition (churn) represents one of the most critical challenges in recurring-revenue service industries. This laboratory project develops an end-to-end statistical analysis and predictive modeling framework in **R** to analyze customer behavior, identify primary attrition drivers, and estimate the empirical likelihood of a customer terminating their contract.

Rather than approaching churn through obscure machine learning techniques, this study deliberately leverages core principles of **Probability Theory** and **Inferential Statistics**. By computing measures of central tendency and dispersion, empirical conditional probabilities, bivariate hypothesis tests ($\chi^2$ test of independence, Welch's two-sample $t$-test, and Pearson correlation), and binary logistic regression, the project demonstrates how mathematical statistics provides transparent, interpretable, and actionable insights.

---

## 2. Problem Statement & Objectives

### 2.1 Problem Statement
Telecommunication service providers face high subscriber acquisition costs. When existing customers depart, revenue is forfeited and replacement costs erode profitability. Identifying high-risk churn patterns before customer cancellation enables data-informed retention interventions.

### 2.2 Core Project Objectives
1. **Data Hygiene & Preprocessing:** Systematically ingest, clean, and structure raw customer records, resolving missing values and recoding categorical features into mathematical factors.
2. **Descriptive Statistical Profiling:** Compute and contrast measures of central tendency (Mean, Median, Mode) and dispersion (Variance, Standard Deviation, Interquartile Range, Skewness) across customer cohorts.
3. **Probability & Conditional Risk Modeling:** Formulate marginal, joint, and conditional probabilities to calculate empirical churn hazards:
   $$P(\text{Churn} \mid \text{Subgroup})$$
4. **Hypothesis Testing:** Quantify statistical significance at $\alpha = 0.05$ across categorical and continuous predictors using Pearson's Chi-Square tests of independence and Welch's two-sample $t$-tests.
5. **Predictive Modeling:** Train and evaluate a binary logistic regression model with an 80/20 train/test split, extracting odds ratios ($\text{OR} = e^\beta$) to quantify risk multipliers.
6. **Interactive Scientific Interface:** Deploy an interactive R Shiny application providing real-time probability lookups, dynamic hypothesis test calculators, publication-grade visual charts, and single-customer risk scoring.

---

## 3. Dataset Description & Preprocessing Pipeline

### 3.1 Raw Dataset Architecture
The investigation uses the Telco Customer Churn dataset, containing 7,043 customer accounts described by 21 attributes:
- **Demographics:** `gender`, `SeniorCitizen`, `Partner`, `Dependents`
- **Account & Service Details:** `tenure`, `PhoneService`, `MultipleLines`, `InternetService`, `OnlineSecurity`, `OnlineBackup`, `DeviceProtection`, `TechSupport`, `StreamingTV`, `StreamingMovies`
- **Contract & Billing:** `Contract`, `PaperlessBilling`, `PaymentMethod`, `MonthlyCharges`, `TotalCharges`
- **Response Variable:** `Churn` (Binary categorical: `"No"` vs `"Yes"`)

### 3.2 Data Cleaning Protocol
1. **Handling Incomplete Records in `TotalCharges`:**
   - Inspection revealed 11 missing values (`NA`) in `TotalCharges`.
   - Investigation established that all 11 missing cases corresponded to customers with $\text{tenure} = 0$ months (newly registered subscribers whose initial monthly cycle had not completed).
   - These 11 records were removed to ensure complete cases, yielding $N = 7,032$ valid observations.
2. **Type Casting & Encoding:**
   - Converted `SeniorCitizen` from discrete numeric codes ($0, 1$) into factor levels (`"No"`, `"Yes"`).
   - Explicitly converted string attributes (`gender`, `Contract`, `PaymentMethod`, etc.) into R factors.
   - Set reference levels such that the baseline outcome is $\text{Churn} = \text{"No"}$.
3. **Identifier Removal:**
   - Dropped the arbitrary string key `customerID` from the modeling data frame to eliminate spurious unique identification noise.
4. **Derived Statistical Stratifications:**
   - `TenureGroup`: Discretized tenure into lifecycle intervals: $0\text{--}12$ months (New), $13\text{--}24$ months, $25\text{--}48$ months, and $49\text{--}72$ months (Loyal).
   - `MonthlyChargeGroup`: Stratified monthly spend into spending tiers: Low ($\le \$35$), Medium ($\$35\text{--}\$70$), High ($\$70\text{--}\$90$), and Very High ($> \$90$).

---

## 4. Descriptive Statistics & Exploratory Data Analysis

### 4.1 Numerical Summary of Continuous Variables ($N = 7,032$)

| Feature | Mean ($\bar{x}$) | Median ($Q_2$) | Mode | Std Dev ($s$) | Variance ($s^2$) | Min | $Q_1$ (25%) | $Q_3$ (75%) | Max | IQR | Skewness |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **Tenure (Months)** | 32.42 | 29.00 | 1.00 | 24.55 | 602.47 | 1.00 | 9.00 | 55.00 | 72.00 | 46.00 | +0.418 |
| **Monthly Charges ($)** | 64.80 | 70.35 | 20.00 | 30.09 | 905.17 | 18.25 | 35.59 | 89.86 | 118.75 | 54.27 | -0.554 |
| **Total Charges ($)** | 2283.30 | 1397.47 | 19.60 | 2266.77 | 5,138,252.41 | 18.80 | 401.45 | 3794.74 | 8684.80 | 3393.29 | +1.172 |

### 4.2 Stratified Comparison by Churn Status
Dividing the sample into retained customers ($n_1 = 5,163$) and churned customers ($n_2 = 1,869$):
- **Tenure:** Retained customers exhibit a mean tenure of **37.65 months** ($s = 24.08$) compared to **17.98 months** ($s = 19.53$) for churned customers. Churn is heavily concentrated among recent sign-ups.
- **Monthly Charges:** Churned customers paid significantly higher monthly rates ($\bar{x} = \$74.44$, median = $\$79.65$) than retained customers ($\bar{x} = \$61.31$, median = $\$64.43$).
- **Skewness Analysis:** Total charges exhibit strong positive right-skewness ($+1.172$) because accumulated lifetime billing accumulates gradually over tenure.

---

## 5. Probability Analysis & Conditional Risk Foundations

### 5.1 Marginal Probability of Churn
Let event $A$ denote that a randomly selected subscriber leaves the service ($\text{Churn} = \text{Yes}$):
$$P(A) = \frac{n(A)}{N} = \frac{1,869}{7,032} \approx 0.2658 \quad (26.58\%)$$
The complementary probability of customer retention is:
$$P(A^c) = 1 - P(A) = \frac{5,163}{7,032} \approx 0.7342 \quad (73.42\%)$$

### 5.2 Conditional Probability Definition
Given condition or event $B$ (e.g., subscribing under a specific contract or paying with a specific method), the conditional probability of churn is defined as:
$$P(A \mid B) = \frac{P(A \cap B)}{P(B)} = \frac{n(A \cap B)}{n(B)}$$
The **Relative Risk (Lift)** compares conditional risk against the unconditional population baseline:
$$\text{Relative Risk} = \frac{P(A \mid B)}{P(A)}$$

### 5.3 Laboratory Conditional Probability Results

#### 1. Contract Commitment
- **Month-to-Month:** $n(B) = 3,875$; $n(A \cap B) = 1,655$
  $$P(\text{Churn} \mid \text{Month-to-Month}) = \frac{1,655}{3,875} = 0.4271 \quad (42.71\%)$$
  $$\text{Relative Risk} = \frac{0.4271}{0.2658} = 1.61\times$$
- **One Year:** $n(B) = 1,472$; $n(A \cap B) = 166$
  $$P(\text{Churn} \mid \text{One Year}) = \frac{166}{1,472} = 0.1128 \quad (11.28\%) \implies 0.42\times \text{ risk}$$
- **Two Year:** $n(B) = 1,685$; $n(A \cap B) = 48$
  $$P(\text{Churn} \mid \text{Two Year}) = \frac{48}{1,685} = 0.0285 \quad (2.85\%) \implies 0.11\times \text{ risk}$$

#### 2. Customer Tenure Lifecycle
- **New Customers ($\text{Tenure} \le 12\text{ mos}$):**
  $$P(\text{Churn} \mid \text{Tenure} \le 12) = \frac{1,037}{2,175} = 0.4768 \quad (47.68\%) \implies 1.79\times \text{ baseline risk}$$
- **Established Customers ($\text{Tenure} > 48\text{ mos}$):**
  $$P(\text{Churn} \mid \text{Tenure} > 48) = \frac{213}{2,239} = 0.0951 \quad (9.51\%) \implies 0.36\times \text{ baseline risk}$$

#### 3. Monthly Spend Exposure
- **High Monthly Charges ($> \text{Median } \$70.35$):**
  $$P(\text{Churn} \mid \text{Charges} > \text{Median}) = \frac{1,237}{3,513} = 0.3521 \quad (35.21\%) \implies 1.32\times \text{ baseline risk}$$
- **Low/Moderate Monthly Charges ($\le \text{Median } \$70.35$):**
  $$P(\text{Churn} \mid \text{Charges} \le \text{Median}) = \frac{632}{3,519} = 0.1796 \quad (17.96\%) \implies 0.68\times \text{ baseline risk}$$

#### 4. Internet Infrastructure & Payment Channels
- **Fiber Optic Internet:** $P(\text{Churn} \mid \text{Fiber Optic}) = 41.89\%$ ($1.58\times$ baseline risk).
- **Electronic Check:** $P(\text{Churn} \mid \text{Electronic Check}) = 45.29\%$ ($1.70\times$ baseline risk).

---

## 6. Inferential Statistics & Hypothesis Testing

All inferential tests were executed at significance level $\alpha = 0.05$.

### 6.1 Pearson's Chi-Square Test of Independence ($\chi^2$)
To test whether customer churn is statistically independent of categorical attributes:
$$\chi^2 = \sum_{i=1}^r \sum_{j=1}^c \frac{(O_{ij} - E_{ij})^2}{E_{ij}}, \quad E_{ij} = \frac{R_i \times C_j}{N}$$

#### Test 1: Contract Type vs Churn
- **$H_0$:** Contract Type and Customer Churn are independent.
- **$H_1$:** Contract Type and Customer Churn are dependent (associated).
- **Observed Result:** $\chi^2 = 1179.5$, $df = 2$, $p$-value $< 10^{-200}$.
- **Decision & Conclusion:** Since $p < 0.05$, reject $H_0$. Strong evidence confirms customer contract length directly correlates with retention probability.

#### Test 2: Internet Service vs Churn
- **$H_0$:** Internet Service Type and Churn are independent.
- **$H_1$:** Internet Service Type and Churn are dependent.
- **Observed Result:** $\chi^2 = 728.7$, $df = 2$, $p$-value $< 10^{-150}$.
- **Decision & Conclusion:** Reject $H_0$. Fiber optic customers churn at a significantly higher rate than DSL or non-internet subscribers.

#### Test 3: Payment Method vs Churn
- **$H_0$:** Payment Method and Churn are independent.
- **$H_1$:** Payment Method and Churn are dependent.
- **Observed Result:** $\chi^2 = 645.4$, $df = 3$, $p$-value $< 10^{-130}$.
- **Decision & Conclusion:** Reject $H_0$. Subscribers utilizing manual electronic checks exhibit significantly elevated churn hazards.

---

### 6.2 Two-Sample Independent Welch's $t$-Tests
To test whether the true population means $\mu_{\text{No}}$ (Retained) and $\mu_{\text{Yes}}$ (Churned) are equal without assuming equal population variances:
$$t = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}}$$

#### Test 1: Difference in Mean Tenure
- **$H_0: \mu_{\text{Tenure, No}} = \mu_{\text{Tenure, Yes}}$** (No difference in mean tenure).
- **$H_1: \mu_{\text{Tenure, No}} \neq \mu_{\text{Tenure, Yes}}$** (Significant difference).
- **Observed Means:** Retained = $37.65$ mos, Churned = $17.98$ mos ($\Delta = +19.67$ mos).
- **Test Statistic:** $t = 34.97$, $df = 3,576.4$, $p$-value $= 2.35 \times 10^{-234}$.
- **95% Confidence Interval for $(\mu_1 - \mu_2)$:** $[18.57, 20.77]$ months.
- **Decision & Conclusion:** Reject $H_0$. Retained customers average approximately 19.7 additional months of tenure.

#### Test 2: Difference in Mean Monthly Charges
- **$H_0: \mu_{\text{Charges, No}} = \mu_{\text{Charges, Yes}}$**
- **$H_1: \mu_{\text{Charges, No}} \neq \mu_{\text{Charges, Yes}}$**
- **Observed Means:** Retained = $\$61.31$, Churned = $\$74.44$ ($\Delta = -\$13.13$).
- **Test Statistic:** $t = -18.47$, $df = 3,634.6$, $p$-value $= 8.60 \times 10^{-73}$.
- **95% Confidence Interval:** $[-\$14.52, -\$11.74]$.
- **Decision & Conclusion:** Reject $H_0$. Churned customers pay an average of $\$13.13$ more per month.

---

### 6.3 Pearson Correlation Analysis
Evaluating linear association between continuous metrics:
$$r = \frac{\sum (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum (x_i - \bar{x})^2 \sum (y_i - \bar{y})^2}}$$

- **Tenure vs Total Charges:** $r = +0.8259$, $t = 122.9$, $p < 10^{-300}$.
  - *Conclusion:* Very strong, positive, statistically significant linear correlation. Total billing naturally expands over customer lifetime.
- **Tenure vs Monthly Charges:** $r = +0.2469$, $t = 21.2$, $p < 10^{-90}$.
  - *Conclusion:* Moderate positive correlation, indicating customers tend to adopt additional services as tenure increases.

---

## 7. Predictive Modeling: Binary Logistic Regression

### 7.1 Mathematical Model Formulation
Because customer churn is a binary outcome ($Y \in \{0, 1\}$), ordinary linear regression is inappropriate (it produces probabilities $< 0$ or $> 1$). We apply **Logistic Regression**, modeling the log-odds (logit) as a linear combination of predictors:
$$\text{Logit}(p) = \ln\left(\frac{p}{1 - p}\right) = \beta_0 + \sum_{j=1}^k \beta_j X_j$$
Applying the logistic sigmoid transformation yields the predicted probability:
$$p = P(Y = 1 \mid \mathbf{X}) = \frac{1}{1 + e^{-(\beta_0 + \sum \beta_j X_j)}}$$

### 7.2 Training Setup & Estimated Coefficients
The dataset was partitioned into an **80% training set** ($n_{\text{train}} = 5,625$) and a **20% testing set** ($n_{\text{test}} = 1,407$) using random seed `42`.

| Parameter / Feature | Coefficient ($\beta$) | Std. Error | $z$-statistic | $p$-value | Odds Ratio ($e^\beta$) | 95% CI for OR | Statistical Interpretation |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :--- |
| **(Intercept)** | -0.2018 | 0.2391 | -0.844 | 0.3985 | 0.817 | [0.511, 1.306] | Baseline reference log-odds. |
| **Tenure** | -0.0569 | 0.0068 | -8.409 | $< 0.0001$ | **0.945** | [0.932, 0.957] | **Protective:** Each extra month decreases churn odds by 5.5%. |
| **Monthly Charges** | -0.0028 | 0.0039 | -0.717 | 0.4734 | 0.997 | [0.990, 1.005] | Not statistically significant after adjusting for internet tier. |
| **Total Charges** | +0.0003 | 0.0001 | 3.935 | $< 0.0001$ | 1.000 | [1.000, 1.000] | Slight lifetime accumulation effect. |
| **Contract: One Year** | -0.7251 | 0.1153 | -6.287 | $< 0.0001$ | **0.484** | [0.386, 0.607] | **Protective:** Decreases churn odds by 51.6% vs Month-to-month. |
| **Contract: Two Year** | -1.5695 | 0.1917 | -8.189 | $< 0.0001$ | **0.208** | [0.143, 0.303] | **Strong Protective:** Decreases churn odds by 79.2% vs Month-to-month. |
| **Internet: Fiber Optic** | +0.8989 | 0.1461 | 6.154 | $< 0.0001$ | **2.457** | [1.845, 3.271] | **Major Risk:** Increases churn odds by 145.7% vs DSL. |
| **Internet: No Internet** | -0.8624 | 0.1737 | -4.965 | $< 0.0001$ | **0.422** | [0.300, 0.593] | **Protective:** Decreases churn odds by 57.8% vs DSL. |
| **Senior Citizen: Yes** | +0.2741 | 0.0984 | 2.786 | 0.0053 | **1.315** | [1.085, 1.595] | **Risk:** Senior subscribers face 31.5% higher churn odds. |

---

### 7.3 Model Performance Evaluation on Test Set ($n_{\text{test}} = 1,407$)

#### Confusion Matrix (Classification Threshold = 0.50):
```
                 Actual Retained (No)    Actual Churned (Yes)
Predicted No             918                     160
Predicted Yes            115                     214
```

#### Performance Metrics:
- **Accuracy:** $\frac{TP + TN}{N} = \frac{214 + 918}{1,407} = \mathbf{80.45\%}$
- **Precision:** $\frac{TP}{TP + FP} = \frac{214}{214 + 115} = \mathbf{65.05\%}$
- **Recall (Sensitivity):** $\frac{TP}{TP + FN} = \frac{214}{214 + 160} = \mathbf{57.22\%}$
- **Specificity:** $\frac{TN}{TN + FP} = \frac{918}{918 + 115} = \mathbf{88.87\%}$
- **F1-Score:** $2 \times \frac{0.6505 \times 0.5722}{0.6505 + 0.5722} = \mathbf{0.6088} \quad (60.88\%)$
- **Area Under the ROC Curve (AUC):** $\mathbf{0.8516} \quad (85.16\%)$

**Diagnostic Evaluation:**  
An AUC of **0.8516** demonstrates very strong discriminative power. The model reliably separates loyal subscribers from churn candidates across varying threshold cutoffs.

---

## 8. Summary of Findings & Key Insights

1. **The Critical Onboarding Window:** Subscribers with tenure under 12 months face a **47.68%** churn probability, nearly double the baseline rate ($1.79\times$). Once tenure surpasses 48 months, churn probability plunges to **9.51%**.
2. **Contract Commitment Eliminates Churn:** Annual and two-year commitments drop churn odds by **51.6%** and **79.2%** respectively compared to month-to-month plans.
3. **Fiber Optic Experience Disparity:** Despite carrying premium monthly charges, Fiber Optic subscribers exhibit a **41.89%** churn rate and an odds ratio of **2.46**, pointing to possible competitive pricing pressures or customer service friction.
4. **Friction in Manual Payments:** Subscribers paying via Electronic Check churn at **45.29%**, compared to under **17%** for automated bank transfers and credit cards.

---

## 9. Conclusion & Practical Recommendations

This project demonstrates how classical **Probability and Statistics** concepts—unconditional and conditional probability, descriptive measures, hypothesis tests, and logistic regression—form a complete, rigorous decision-science toolkit.

### Concrete Business Recommendations:
1. **Targeted Onboarding Retainers:** Incentivize new subscribers in their first 6 months with loyalty bonuses.
2. **Migration to Multi-Year Contracts:** Offer modest discounts to encourage subscribers to move from month-to-month contracts to 12-month or 24-month commitments.
3. **Automated Billing Incentives:** Encourage adoption of paperless automated bank or credit card billing over manual electronic checks.
4. **Fiber Optic Quality Audit:** Investigate service reliability and pricing satisfaction for fiber optic subscribers to mitigate the $2.46\times$ odds multiplier.
