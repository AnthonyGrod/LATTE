int main() {
    var int x = 1;
    var int y = 8;
    var int z = 20;
    if (y == 8) {
        printString("y is an eight!");
        x = 98;
        while (z < 30) {
            printInt(z);
            var int x = 1;
            z++;
        }
        return z;
    } else {
        x = 5;
    }
    return x;
}