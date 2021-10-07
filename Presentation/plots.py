# %%
from ipaddress import summarize_address_range
import statsmodels.formula.api as smf
import statsmodels.api as sm
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
plt.rcParams['font.family'] = "Arial"
# %%
df = pd.read_csv("../Data/lalondedata.txt")

# %%

df['race'] = np.select(
    [
        df.black == 1,
        df.hispan == 1,
    ],
    ['black', 'hispan'],
    default='other'
)

df['race'] = df['race'].astype('category')
df = df.assign(diff_re=df['re78'] - df['re74'])


# %%
# Distribution of re78
fig, ax = plt.subplots(figsize=(10, 8))
ax.hist(
    df['re78'],
    bins=30,
    ec='k',
    color="#03529B"
)

ax.tick_params(axis='x', labelsize=18)
ax.tick_params(axis='y', labelsize=18)

ax.xaxis.set_major_formatter(lambda x, _: f"${x/1000:.0f}k")
ax.set_xlabel("\nWage in 1978 (re78)", size=18)
ax.set_ylabel("Count", size=18)
ax.set_title("Distribution of Wages in 1978", size=20, weight='bold')
sns.despine()
plt.savefig("./re78_distribution.png", dpi=300, facecolor='white')
plt.close()



# %%
sns.boxplot(data=df, x='treat', y='diff_re', hue='race')

# %%
sns.set_context('poster')
fg = sns.catplot(
    data=df,
    kind="box",
    y='treat',
    x='diff_re',
    row='race',
    palette=["#03529B", "#061953"],
    # boxprops=dict(ec='white'),
    medianprops=dict(color="white"),
    flierprops=dict(marker="x"),
    height=4,
    aspect=8,
    orient='h'
)

for ax in fg.axes.ravel():
    # y axis
    ax.spines['left'].set_visible(False)
    ax.yaxis.set_tick_params(which='both', length=0)
    ax.set_ylabel('')
    ax.set_yticklabels([])

    # rest
    ax.set_title(ax.get_title(), size=26, weight='bold')
    ax.xaxis.set_major_formatter(lambda x, _: f"${x/1000:.0f}k")
    ax.text(s="No Treatment", x=40_000, y=0, color='#03529B', size=24, weight='bold', va='center')
    ax.text(s="Treatment", x=40_000, y=1, color='#061953', size=24, weight='bold', va='center')

    # ax.axvline(0, ls='--', alpha=0.4, color='black', zorder=0)
fg.axes.ravel()[-1].set_xlabel("\nDifference between $re78 - re74$")
plt.tight_layout()
plt.savefig("interaction_treat_race_wide.png", dpi=300, facecolor="white")

# %%

mod = smf.ols(formula="diff_re ~ age + treat + married + treat:age", data=df)
res = mod.fit()
print(res.summary())

# %%
sns.set_context('talk')
ages = list(range(df.age.min(), df.age.max() + 1))
result_mtx = {}
for married in (0, 1):
    for treated in (0, 1):
        pdp_data_points = []
        for x in ages:
            copydf = df["age treat married diff_re".split()].copy()
            copydf['age'] = np.full_like(df.age, fill_value=x)
            copydf['treat'] = np.full_like(df.shape[0], fill_value=treated)
            copydf['married'] = np.full_like(df.shape[0], fill_value=married)
            preds = res.predict(copydf)
            pdp_data_points.append(preds.mean())
        result_mtx[f"married_{married}+treated_{treated}"] = pdp_data_points
result_df = pd.DataFrame(result_mtx, index=ages)

# %%

fig, ax = plt.subplots(figsize=(10, 6))


config = {
    "married_0+treated_1": dict(color="#03529B", annot="Treated + Not Married"),
    "married_1+treated_1": dict(color="#061953", annot="Treated + Married"),
    "married_0+treated_0": dict(color="red", annot="Not Treated + Not Married"),
    "married_1+treated_0": dict(color="darkred", annot="Not Treated + Married"),
}

for key, val in config.items():
    ax.plot(result_df[key], color=val['color'])
    # ax.text(s=val['annot'], x=57, y=result_df[key].values[-1], va='center', color=val['color'], family="Menlo")
    ax.text(s=val['annot'], x=56, y=result_df[key].values[-1], va='center', color=val['color'], size=14, weight='bold')


sns.despine()
ax.yaxis.set_major_formatter(lambda x, _: f"${x/1000:.0f}k")
ax.axhline(0, ls="--", color="0.8", zorder=0)

ax.set_xlabel("Age (years)", size=18)
ax.set_ylabel("Predicted re78 - re74", size=18)
ax.set_title("Predicted Difference in Earnings", size=20, weight='bold')
plt.tight_layout()
plt.savefig("prediction_plot.png", dpi=300, facecolor="white")
