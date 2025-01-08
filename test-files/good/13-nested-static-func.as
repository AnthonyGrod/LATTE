int outerFunc() {
    return 2;
}

int main() {
    printInt(outerFunc());

    int outerFunc() {
        return 3;
    }

    printInt(outerFunc());
    return 1;
}