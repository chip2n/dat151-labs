
int main() {
    int h = 0;
    if(h > 0) {
        int h = 1;
        int i = 0;
        i = 1;
        h = 4;
    } else {
        int h = 1;
        h = 3;
    }

    {
        h = 10;
    }

    while(h < 10) {
        h = 1;
        test(142);
    }

    return h+1;
}

int test(int t) {
    t = 10;
    return 10;
}
