let x = 3; //x=3
let name = "John"; //simple string
let num = 4 * (2 + 3) / 5; //math exp

let a = [1, 2, 3]; // array

let add = fn(a, b) {return a + b;};
let add = fn(a, b) {a + b;};

add(3, 6);

let fib = fn(x) {//fib func
    if (x == 0) {
        0
    } else {
    if (x == 1) {
        1
    } else {
        fib(x - 1) + fib(x - 2);
    }}
};

let twice = fn(f, x) {
 return f(f(x));
};

let square = fn(x) {
 return x*x;
}

twice(square,2);