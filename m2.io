int m;
char c;
void f(int p, int k, char m);

void f(int p) -> {
    int ret;
    ret = 6;
    if (p == 5) {
        ret = 0;
    } else {
        ret = 1;
    };
    return ret;
};

int main() -> {
    int m = 2;
    f(m);
    return 0;
};
