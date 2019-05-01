
def factorial(n: Int): Int =
  if (n == 1) 1 else n * factorial(n - 1)


def factorialTailRec(n: Int, r: Int): Int = {
  if (n == 1) r else factorialTailRec(n - 1, n * r)
}

factorial(4)
factorialTailRec(4, 1)


4 * 3 * 2 * 1