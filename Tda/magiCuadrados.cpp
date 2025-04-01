#include <iostream>
#include <vector>
#include <numeric>
#include <chrono>
using namespace std;

int cantMagicuadrados = 0;
int operaciones = 0;
bool esMagicuadrado(vector<vector<int> > matriz);
void backtrack(int n, vector<vector<int> > grilla, vector<bool> visitados, int i, int j);
bool checkFila(vector<int> fila);

int main()
{
    vector<int> numeros;
    int n;
    cout << "n =  ";
    cin >> n;
    vector<vector<int> > magiCuadrado(n, vector<int>(n, 0));
    vector<bool> visitados(n * n, false);
    using std::chrono::high_resolution_clock;
    using std::chrono::duration_cast;
    using std::chrono::duration;
    using std::chrono::milliseconds;

    auto t1 = high_resolution_clock::now();
    backtrack(n, magiCuadrado, visitados, 0, 0);
    cout << cantMagicuadrados << endl;
    auto t2 = high_resolution_clock::now();

    /* Getting number of milliseconds as an integer. */
    auto ms_int = duration_cast<milliseconds>(t2 - t1);
    
    cout << ms_int.count() << "ms\n" << endl;
    cout << operaciones << endl;
    return 0;
}

void backtrack(int n, vector<vector<int> > grilla, vector<bool> visitados, int i, int j)
{
    operaciones++;
    if (i == n)
    {
        if (esMagicuadrado(grilla))
        {
            cantMagicuadrados++;
            cout << cantMagicuadrados << endl;
        }
        return;
    }
    if (i != 0 && j == 0)
    {
        if (!checkFila(grilla[i - 1])) return;
    }
    if (i == n - 1 && j != 0) {
        vector<int> col(n,0);
        for(int h = 0; h < n; h++) {
            col[h] = grilla[h][j - 1];
        }
        if(!checkFila(col)) return;
    }
    for (int z = 0; z < n * n; z++)
    {
        if (!visitados[z])
        {
            grilla[i][j] = z + 1;
            visitados[z] = true;
            int ProxI = i;
            int ProxJ = j + 1;

            if (ProxJ >= n)
            {
                ProxJ = 0;
                ProxI++;
            }
            
            backtrack(n, grilla, visitados, ProxI, ProxJ);

            visitados[z] = false;
            grilla[i][j] = 0;
        }
    }
}
bool esMagicuadrado(vector<vector<int> > matriz)
{
    int n = matriz.size();
    if (n == 0)
        return false;

    int magicConstant = n * (n * n + 1) / 2;

    
    for (int i = 0; i < n; i++)
    {
        int sumafila = 0;
        int sumaCol = 0;
        for (int j = 0; j < n; j++)
        {
            sumafila += matriz[i][j];
            sumaCol += matriz[j][i];
        }
        if (sumafila != magicConstant || sumaCol != magicConstant)
        {
            return false;
        }
    }

    
    int sumaDiag1 = 0;
    int sumaDiag2 = 0;
    for (int i = 0; i < n; i++)
    {
        sumaDiag1 += matriz[i][i];
        sumaDiag2 += matriz[i][n - 1 - i];
    }
    if (sumaDiag1 != magicConstant || sumaDiag2 != magicConstant)
    {
        return false;
    }

    return true;
}

bool checkFila(vector<int> fila)
{
    int suma = 0;
    int n = fila.size();
    for (int i = 0; i < fila.size(); i++)
    {
        suma += fila[i];
    }
    int magicConstant = n * (n * n + 1) / 2;
    return suma == magicConstant;
}
