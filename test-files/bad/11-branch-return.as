int f(int x) {
    if (true) {
       printInt(1);
    } else {
       return 0;
    }
}

int main(){
     printInt(f(2));
     return 1;
}