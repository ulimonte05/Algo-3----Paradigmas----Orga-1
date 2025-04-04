#include <iostream>
#include <vector>

using namespace std;

int main()
{
    int casos;
    cin >> casos;

    int n, m;
    cin >> n >> m;
    vector<vector<int> > ady(n, vector<int>(m, 0));
    for (int z = 0; z < casos; z++)
    {
        for (int i = 0; i < ady.size(); i++)
        {
            for (int j = 0; j < ady[i].size(); j++)
            {
                int in;
                cin >> in;
                ady[i][j] = in;
            }
        }
    }
}