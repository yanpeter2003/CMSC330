def fib(n)
  first_term = 0
  second_term = 1
  next_term = 0
  counter = 0
  nums = []

  if n != 0
    nums.push(first_term)
  end
  while counter < n + 1
    if counter <= 1
      next_term = counter
    else
      nums.push(next_term)
      next_term = first_term + second_term
      first_term = second_term
      second_term = next_term
    end
    counter += 1
  end
  puts "#{nums}"
end

def isPalindrome(n)
  reverse = (n.to_s).reverse
  if reverse == n.to_s
    puts true
  else 
    puts false
  end
end

def nthmax(n, a)
  sorted_a = a.sort
  if n > a.length()
    puts nil
  else
    puts sorted_a[a.length - 1 - n]
  end
end

def freq(s)
  max = 0
  char = ""
  hash = Hash.new(0)
  s.split('').each{|ch| hash[ch] += 1}
  hash.each do |k, v|
    if (v > max)
      max = v
      char = k
    end
  end
  puts char
end

def zipHash(arr1, arr2)
  if arr1.length != arr2.length
    return nil
  end
  hash = Hash[arr1.zip(arr2)]
  puts hash
end

def hashToArray(hash)
  array = []
  hash.each do 
    pair = hash.shift
    array.push(pair)
  end
  puts "#{array}"
end

def maxProcChain(init, procs)
  max = init
  procs.each do |x|
    if x.call(max) > max
      max = x.call(max)
    end
  end
  puts max
end
