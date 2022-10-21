int r = 2;

int f(int arg_1, int arg_2) {
    if (arg_1 > arg_2) {
        int ll = 3;
    } else {
        int ll = 1;
    }
    return ll;
};

int m = 3;

void main() {
    int loc = f(2, m) + 1;
    return loc;
};

