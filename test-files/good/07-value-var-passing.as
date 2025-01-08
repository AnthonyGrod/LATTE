void funcVarPass(var int x, int y) {
    x = 10;
    y = 5;
    return;
}

int main() {
    var int a = 1;
    var int b = 7;
    funcVarPass(a, b);
    printInt(a);
    printInt(b);
    return 1;
}
