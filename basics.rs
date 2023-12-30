/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    let mut i = 0;
    if n < 0 {
        i = -1;  
    }
    else {
        for x in 1..(n+1) {
            i += x;
        }
    }
    return i
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut num = 0;
    for item in ls.iter() {
        if (item >= &s) && (item <= &e) {
            num += 1;
        }
    }
    return num;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let total = target.len();
    let mut check = 0;
    for index in target.iter() {
        for num in set.iter() {
            if num == index {
                check += 1;
            }
        }
    }
    if check == total {
        return true;
    }
    return false;
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.len() == 0 {
        return None;
    }
    else {
        let sum = ls.iter().fold(0.0, |acc, x| acc + x);
        let result = sum / (ls.len() as f64);
        return Some(result);
    }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut power = (ls.len()) as u32;
    let mut dec = 0;
    for index in ls.iter(){
        power -= 1;
        dec += index * (2_i32).pow(power);
    }
    return dec;
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut number = n;
    let mut vec = Vec::new();
    for i in 2..n+1 {
        while number % i == 0 {
            vec.push(i);
            number /= i;
        }
    }
    return vec;
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let length = lst.len();
    let mut vec = Vec::new();
    if length == 0 {
        return vec;
    }
    else if length == 1 {
        vec.push(lst[0]);
        return vec;
    }
    for i in 1..length {
        vec.push(lst[i]);
    }
    vec.push(lst[0]);
    return vec;
}
/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let s_length = s.len();
    let target_length = target.len();
    let mut check = false;
    for i in 0..s_length {
        let mut temp_target = String::new();
        if s.chars().nth(i) == target.chars().nth(0) {
            for j in 0..target_length {
                if s.chars().nth(i + j) == None {
                    return check;
                }
                else {
                    temp_target.push((s.chars().nth(i + j)).unwrap());
                }
            }
        }
        if temp_target == target {
            check = true;
            return check;
        }
    }
    return check;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    let s_length = s.len();
    let mut current_char = s.chars().nth(0);
    let mut count = 0;
    let mut current_index = 0;
    let mut longest_index = 0;
    let mut longest_length = 0;
    if s_length == 0 {
        return None;
    }
    for i in 0..s_length {
        if s.chars().nth(i) == current_char {
          count += 1;
        }
        else {
            if count > longest_length {
                longest_length = count;
                longest_index = current_index;
            }
            current_char = s.chars().nth(i).clone();
            count = 1;
            current_index = i;
        }
    }
    if count > longest_length {
        longest_length = count;
        longest_index = current_index;
    }
    return Some(&s[longest_index..(longest_index + longest_length)]);
}
