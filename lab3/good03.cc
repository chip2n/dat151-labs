int main ()
{
  int arg = readInt() ;
  int ret = 1 ;

  int i = 1 ;

  while (i < arg + 1) {
    printInt(i*ret);
    ret = i * ret ;
    ++i ;
  }
  printInt(ret) ;

}
