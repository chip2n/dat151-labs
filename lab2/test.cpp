int main() {
  int i = 78;
  {
    int i = 1;
    printInt(i);
    {
        i = 2;
        printInt(i);
    }
    printInt(i);
  }
  printInt(i);

}

//void printInt(int x) { }
//void printDouble(double x) { }
