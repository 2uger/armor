int r;
int fjskl;
int c;
int f (int m, int t) ->  {
    c = 2 * t + m;
    return c;
    f(m, t);
};

void main(int k) -> {
    c = 2;
    f(2, c);

};
