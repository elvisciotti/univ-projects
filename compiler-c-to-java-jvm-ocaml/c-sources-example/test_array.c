

extern "Utils/print_string"
void print_string(string);
extern "Utils/int_to_string"
string int_to_string(int);


void
main(string[] args) {
  int[][] a;
  int i;
  int j;
  int [][][]b;
  
  a = new int[][10];
  

  for(i=0;; i<10;  i++; )
     a[i]=new int[10];

   
 for(i=0;; i<10;  i++; )
    for(j=0;; j<10;  j++; )
       a[i][j] = i * j;
       

 for(i=0;; i<10;  i++; )
  {  for(j=0;; j<10;  j++; )
       print_string(int_to_string( a[i][j] )) ;
     print_string("-");
  }     
  

  b = new int [][][10];
  b[0] = new int [][10];
  b[0][0] = new int [10];
  b[0][0][0]=34;
  b[0][0][0]++;
   
  
  print_string(int_to_string( b[0][0][0] )) ;
  
  
  

}
