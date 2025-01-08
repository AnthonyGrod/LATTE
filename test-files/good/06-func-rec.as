int recFun(int x) {
    if (x == 0) {
        return 1;
    }
    else {
        return recFun(x-1);
    }
}

int main() {
    return recFun(3);
}