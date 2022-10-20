int r = 2;

int
f(int arg_1, int arg_2)
{
    return arg_1 + arg_2;
};

int
main(int k){
    int loc = k + 1;
    return f(2, loc);
};

