
def abs(x: Double) = if (x < 0) -x else x

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnoughB(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess) - x < 0.001

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrt(x: Double) = sqrtIter(1.0, x)

// Improving is GoodEnough (make difference between guess and x relative to x)
def isGoodEnoughB(guess: Double, x: Double) =
  abs(guess * guess - x) / x < 0.001


sqrt(3)
sqrt(1e-6)
sqrt(4e20)


// Defining scope
def sqrtB(x: Double) = {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
  (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrtB(3.0)








