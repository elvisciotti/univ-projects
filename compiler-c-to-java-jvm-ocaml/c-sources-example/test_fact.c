extern "Utils/print_string" void print_string(string);
extern "Utils/int_to_string" string int_to_string(int);

int fact(int x)
{
int y;
if(x==0) y=1; 
else y=x*fact(x-1); return (y);
}
void main(string[] args)
{
int i;
int[] x;
int[] y;
int j;
i =13;
j=17; /*
x = new int[5];
y = new int[7];


x[2]=4;
c="ciao";
for(x=0;;x<3;x=x+1;)
print_string(int_to_string(x));

print_string(c);
print_string("fattoriale: ");*/
print_string(int_to_string(i));
print_string(int_to_string(j));


}





