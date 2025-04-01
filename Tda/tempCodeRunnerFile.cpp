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