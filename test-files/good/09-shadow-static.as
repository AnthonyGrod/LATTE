var int x = 1;

int main() {
    printInt(x);
    var int x = 9;
    printInt(x);
    var int y = 8;
    {
        var int x = 99;
        y = 123;
        printInt(x);
    }
    printInt(y);
    printInt(x);
    if (y == 123) {
        printString("y is large!");
        x = 98;
        return x;
    }
    return y;
}