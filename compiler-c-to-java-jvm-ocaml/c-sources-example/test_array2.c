

extern "Utils/print_string"
void print_string(string);
extern "Utils/int_to_string"
string int_to_string(int);


void
main(string[] args) {
  int[] a;
    
  a = new int[10];
  a[0]=2;
  a[0]++;
   
  print_string(int_to_string( a[0])) ;
  
  
  

}
