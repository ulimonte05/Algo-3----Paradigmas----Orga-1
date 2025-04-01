#include <iostream>
#include <vector>

using namespace std;

vector<vector<int> > res; 


vector<vector<int> > Subsets(vector<int> set);
void backtrack(int i, vector<int>& sol, vector<int>& set);

int main() {
    vector<int> set;
    vector<vector<int> > allSubsets;

    int n;
    cout << "Suma k: ";
    cin >> n;
    set.resize(n);
    cout << "Lista de elementos: ";
    for (int i = 0; i < n; ++i) {
        cin >> set[i];
    }

    allSubsets = Subsets(set);

    for (const auto& subset : allSubsets) {
        cout << "[";
        for (const auto& element : subset) {
            cout << " " << element;
            cout << ',';
        }
        cout << ']';
    }
    cout << endl;
    return 0;
}

vector<vector<int> > Subsets(vector<int> set) {
    vector<int> sol;
    int i = 0;
    backtrack(i, sol, set);
    return res;
}

void backtrack(int i, vector<int>& sol, vector<int>& set) {
    if (i == set.size()) {
        res.push_back(sol); 
        return;
    }

    // No sumo el iesimo elemento
    backtrack(i + 1, sol, set);

    // sumo el iesimo elemento
    sol.push_back(set[i]);
    backtrack(i + 1, sol, set);
    sol.pop_back(); // Bactrack, terminan los llamados recursivos de "arriba" y sigo con el siguiente
}