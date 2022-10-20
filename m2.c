int r = 2;

int global = 12312;
int
f(int ll){
    if (ll) {
        int m = 1 + 2;
        return global + r;
    } else {
        f(2);
    }
    return ll + 2;
};

