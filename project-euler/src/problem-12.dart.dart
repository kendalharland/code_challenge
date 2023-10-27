import 'dart:math';

int nthTriangleNumber(int n) => n * (n + 1) / 2;

int divCount(int divisor, int number) {
  int count = 0;

  assert(divisor != 1);
  while (number % divisor == 0) {
    count++;
    number /= divisor;
  }
  return count;
}

int numDivisors(int number) {
    int divisors = 1;
    var prime = [2, 3, 5, 7, 11, 13, 17].iterator..moveNext();
    while (prime.current != null) {
      int exponent = divCount(prime.current, number);
      divisors *= (exponent + 1);
      number /= pow(prime.current, exponent);
      prime.moveNext();
    }
    return divisors;
}

void main() {
  int divisors = 0;
  int number;
  for (int n=1; divisors <= 500; n++) {
    number = nthTriangleNumber(n);
    divisors = numDivisors(number);
  }
  print("$number has $divisors divisors");
}
