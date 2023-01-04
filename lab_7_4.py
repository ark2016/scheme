def memoize(f):
  memo = {}
  def nested_function(*args):
    if args not in memo:
      memo[args] = f(*args)
    return memo[args]
  return nested_function

@memoize
def fibonacci(n):
    if n <= 2:
        return 1
    return fibonacci(n - 1) + fibonacci(n - 2)


print(fibonacci(250))
