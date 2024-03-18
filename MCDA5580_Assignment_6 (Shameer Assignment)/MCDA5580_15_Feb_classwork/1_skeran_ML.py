from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn import tree
from joblib import dump

data = load_iris()

X = data["data"]
y = data["target"]

train_X, test_X, train_y, test_y = train_test_split(X, 
                                                    y, 
                                                    test_size=0.2,
                                                    random_state=44)
clf = tree.DecisionTreeClassifier()
clf = clf.fit(train_X, train_y)

# Predict test data set
pred_y = clf.predict(test_X)
accuracy = accuracy_score(test_y, pred_y)
print(accuracy)

# Predict train data set
pred_y_train = clf.predict(train_X)
accuracy_train = accuracy_score(train_y, pred_y_train)
print(accuracy_train)

print(data["target_names"][pred_y])

#dump(clf,"DT.joblib")
tree.plot_tree(clf, filled=True)



