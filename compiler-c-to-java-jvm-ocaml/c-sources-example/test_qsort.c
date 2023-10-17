/* qsort.c */

extern "Utils/print_string"
void print_string(string);
extern "Utils/int_to_string"
string int_to_string(int);

void
swap(int[] array, int left, int right) {
  int tmp;
  tmp = array[left];
  array[left] = array[right];
  array[right] = tmp;
}

void
qsort(int[] array, int begin, int end) {
  if (end > begin) {
    int pivot;
    int l;
    int r;
    pivot = array[begin];
    l = begin + 1;
    r = end;
    while (l < r) {
      if (array[l] <= pivot) {
        l = l + 1;
      } else {
        r = r - 1;
        swap(array, l, r);
      }
    }

    l = l - 1;
    swap(array, begin, l);
    qsort(array, begin, l);
    qsort(array, r, end);
  }
}

void
init_array(int[] array, int size) {
  int i;
  i = 0;
  while (i < size) {
    array[i] = 10 - i;
    i = i + 1;
  }
}

void
print_array(int[] array, int size) {
  int i;
  i = 0;
  while (i < size) {
    print_string(int_to_string(array[i]));
    i = i + 1;
  }
}

void
main(string[] args) {
  int[] a;
  a = new int[10];
  init_array(a, 10);
  print_string("before sorting:");
  print_array(a, 10);
  qsort(a, 0, 10);
  print_string("after sorting:");
  print_array(a, 10);
}
