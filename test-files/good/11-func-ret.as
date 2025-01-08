boolean myFunc(int x) {
    return (x == 1);
}

int main() {
    var int x = 1;
    var boolean y = false;
    printBool(y);
    y = myFunc(1);
    printBool(y);
    return x;
}