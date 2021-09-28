# %%
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
plt.rcParams['font.family'] = "Arial"
# %%
df = pd.read_csv("../Data/lalondedata.txt")

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
    aspect=4,
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
    ax.text(s="No Treatment", x=40_000, y=0, color='#03529B', size=20, weight='bold', va='center')
    ax.text(s="Treatment", x=40_000, y=1, color='#061953', size=20, weight='bold', va='center')

    # ax.axvline(0, ls='--', alpha=0.4, color='black', zorder=0)
fg.axes.ravel()[-1].set_xlabel("\nDifference between $re78 - re74$")
plt.savefig("interaction_treat_race.png", dpi=300, facecolor="white")

