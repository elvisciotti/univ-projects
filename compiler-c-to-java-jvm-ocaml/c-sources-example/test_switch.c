extern "Utils/print_string" void print_string(string);
extern "Utils/int_to_string" string int_to_string(int);

void main(string[] args)
{
	int b;
	b=3;
	b++;

	print_string(int_to_string(b));
	switch (b)
	{
		case 1:
			print_string("e' 1");
		case 2: 
			print_string("e' 2");
		case 3: 
			print_string("e' 3");

		default:
			print_string("default");
	}
}





