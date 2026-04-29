import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegressionCV
from sklearn.metrics import classification_report, roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import warnings
warnings.filterwarnings("ignore", message="The max_iter was reached which means the coef_ did not converge")


file_path = 'data/GSS.xlsx' 
df = pd.read_excel(file_path)

gss_missing = ['.n:  No answer', '.d:  Do not Know/Cannot Choose', 
               '.i:  Inapplicable', '.r:  Refused', '.u:  Uncodeable', '.s:  Skipped on Web']
df.replace(gss_missing, np.nan, inplace=True)

# Encode target
satjob_mapping = {'Very dissatisfied': 0, 'A little dissatisfied': 0, 
                  'Moderately satisfied': 1, 'Very satisfied': 1}
df['satjob_encoded'] = df['satjob'].map(satjob_mapping)  # use .map not .replace

# Drop high-cardinality and leakage columns
drop_cols = ['satjob', 'satjob_encoded', 'id_', 'occ10', 'indus10', 
             'ethnic', 'ballot', 'year', 'income', 'wrkstat', 'marital','hrs2']
#, 'wrkslf', health
feature_cols = [c for c in df.columns if c not in drop_cols]
#print(df.dtypes)
df = df.replace('89 or older', 90)

df['age'] = df['age'].astype(float)

#print(df.dtypes)
#print(feature_cols)
# Check missingness BEFORE dropna
target = 'satjob_encoded'
working = df[feature_cols + [target]]

# Drop columns with 40% missing, then drop remaining null rows
thresh = 0.60
keep_cols = working.columns[working.isnull().mean() < thresh].tolist()
working = working[keep_cols].dropna()

print(df['year'])

X = working[[c for c in keep_cols if c != target]]
y = working[target]

# Encode categoricals + scale
X = pd.get_dummies(X, drop_first=True)
scaler = StandardScaler()

# Test AUC with and without specific variables
def quick_auc(X_in, y_in):
    Xtr, Xte, ytr, yte = train_test_split(X_in, y_in, test_size=0.2, random_state=42)
    sc = StandardScaler()
    Xtr = sc.fit_transform(Xtr)
    Xte = sc.transform(Xte)
    m = LogisticRegressionCV(cv=5, l1_ratios=(1,), solver='saga', Cs=10,
                              max_iter=500, random_state=42, class_weight='balanced',
                              use_legacy_attributes=False)
    m.fit(Xtr, ytr)
    return roc_auc_score(yte, m.predict_proba(Xte)[:, 1])

# Baseline
#print(f"Baseline AUC: {quick_auc(X, y):.4f}")

# Test dropping each original column one at a time
#original_features = [c for c in working.columns if c not in [target, 'decade']]
#for col in original_features:
    # Drop all dummies related to this column
#    cols_to_drop = [c for c in X.columns if c.startswith(col)]
#    if not cols_to_drop:
#        continue
#    X_reduced = X.drop(columns=cols_to_drop)
#    auc = quick_auc(X_reduced, y)
#    print(f"Drop '{col}': AUC={auc:.4f}")


#for name, cols in subsets.items():
#    # Only keep cols that actually exist in working
#    cols = [c for c in cols if c in working.columns]
#    X_sub = pd.get_dummies(working[cols], drop_first=True)
#    auc = quick_auc(X_sub, y)
#    print(f"{name:20s} ({len(cols)} vars): AUC={auc:.4f}")


# 1. Check how many features you have after get_dummies
#print(X.shape) 

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42
)

# Fit scaler on train only
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)  # no leakage

model = LogisticRegressionCV(
    cv=5,
    l1_ratios=(1,),  
    solver='saga',
    max_iter=1000,
    Cs=np.logspace(-2, 2, 50),
    random_state=42,
    class_weight='balanced'
)

model.fit(X_train_scaled, y_train)

y_pred = model.predict(X_test_scaled)
y_prob = model.predict_proba(X_test_scaled)[:, 1]

#print(f"\nBest C: {model.C_[0]:.4f}")
#print(classification_report(y_test, y_pred))
#print(f"ROC-AUC: {roc_auc_score(y_test, y_prob):.4f}")

# Selected features
coef_df = pd.DataFrame({
    'feature': X.columns,
    'coefficient': model.coef_[0]
}).query('coefficient != 0').sort_values('coefficient', key=abs, ascending=False)

#print(f"\n{len(coef_df)} features selected:")
#print(coef_df.to_string(index=False))





working['decade'] = df.loc[working.index, 'year'].apply(lambda x: (x // 1) * 1)

decade_results = {}

for decade, group in working.groupby('decade'):
    if len(group) < 200:
        continue
    
    X_d = group[[c for c in keep_cols if c not in [target, 'decade']]]
    y_d = group[target]
    
    X_d = pd.get_dummies(X_d, drop_first=True)
    X_d = X_d.reindex(columns=X.columns, fill_value=0)
    
    X_train_d, X_test_d, y_train_d, y_test_d = train_test_split(
        X_d, y_d, test_size=0.2, random_state=42
    )
    
    X_train_scaled_d = scaler.fit_transform(X_train_d)
    X_test_scaled_d = scaler.transform(X_test_d)
    
    model_d = LogisticRegressionCV(
        cv=5, l1_ratios=(1,), solver='saga',
        Cs=10, max_iter=500, random_state=42,
        class_weight='balanced', use_legacy_attributes=False
    )
    model_d.fit(X_train_scaled_d, y_train_d)
    
    y_prob_d = model_d.predict_proba(X_test_scaled_d)[:, 1]
    auc = roc_auc_score(y_test_d, y_prob_d)
    
    coefs = pd.Series(model_d.coef_[0], index=X.columns)
    coefs = coefs[coefs != 0].sort_values(key=abs, ascending=False)
    
    decade_results[decade] = {'auc': auc, 'coefs': coefs, 'n': len(group)}
    print(f"{decade}s — n={len(group)}, AUC={auc:.4f}")

feature = 'age'
print(f"\n'{feature}' coefficient over time:")
for decade, res in sorted(decade_results.items()):
    coef = res['coefs'].get(feature, 0)
    print(f"  {decade}s: {coef:.4f}")