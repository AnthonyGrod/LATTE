void myFunc(var int n) {
    n = 99999;
    return;
}

int main() {
    printString("ilovejpp");
    var int x = 1;
    var int y = 8;
    {
        var int x = 99;
        y = 123;
        printInt(x);
    }
    printInt(y);
    printInt(x);
    myFunc(x);
    printInt(x);
    if (y == 123) {
        printString("y is large!");
        x = 98;
        return 200;
    }
    return 20012;
    var int z = 1;
    while (z < 3) {
        printInt(z);
        var int x = 1;
        z++;
    }
    printInt(z);
    return x;
}