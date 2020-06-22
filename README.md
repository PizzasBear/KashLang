# Lang
An interpreted lisp like language written in rust.

Features include:
- variables
- lists
- lambdas
- scopes
- operator scope
- `if` and `if_else` functions
- function return propegation

The language is made almost entirely of function calles. A function call is structured as a line, the first word in it is the
function itself everything after is an argument of that function. The semicolon signifies the end of the function call. The
hello world is the following.
```
print "Hello World!";
^     ^------       ^
function    |       end of function
           first argument
Output:
Hello World!
```

To define a variable use the function `let`, the first argument is the name of the variable as a string
and the second argument is its value. To use that variable you write its name.
For example to create the variable `foo` with value `12` you write:
```
let "foo" 12;
print foo;


Output:
12
```

To change the value of a variable use the function `set`. It's very similar to `let` but the variable has to be defined before
this function is called. As an example changing the value of `foo` to `"hello"` add the following line:
```
let "foo" 12;
print foo;

set "foo" "hello";
print foo;


Output:
12
hello
```

A scope is a code block it returns the last expression if it's not covered by a semicolon like in rust.
All variables that are defined in a block are deleted when it's closed. Overriding variables is allowed in a scope
if the variable was defined outside the scope. An example of a use of scope is:
```
let "a" "Out of scope A";
print a;
print (
    let "a" "In scope A";
    a
);
print a;


Output:
Out of scope A
In scope A
Out of scope A
```
You may notice that `a` was called as a function at the end of that scope. This is normal because if you call a type which
is not ment to be used as a function it will just return itself, so in this example calling the string `"In scope A"` returns
itself (`"In scope A"`).

A list is a dynamicly sized collection of different values. A list has its elements seperated with a space. To index a list
you call it as a function with an `int`/`uint` index if the index is negative then it's indexes from the end of the list.
An example of a list is:
```
let "list" [ 1 "Hello" 2.4 ];
print list;
print (list 0);
print (list -1);


Output:
[ 1, Hello, 2.4 ]
1
2.4
```

To create a lambda with no parameters use curly braces. The lambda will act like a scope, only it has to be
called like a function.
If you want to return early use the ret function.
```
let "f" {
    print "F was called";
    "F output"
};
print (f);
print "";
print (f);


Output:
F was called
F output

F was called
F output
```

For the lambda to accept parameters, it needs to be created by calling a list with the only argument being the lambda. The list
will be a list of the arguments the lambda receives. Each argument may be either the name of the argument or a list with the name
and the type of the argument. For example:
```
let "f" (["a" "b"] {
    let "res" (add a b);
    print res;
    [a b]
});
print (f 1 2);
print (f 4.2 5.8);

let "print_str" ([["s" str]] { print s; });
print_str "Hello World!";
print_str 2;


Output:
3
[ 1, 2 ]
10
[ 4.2, 5.8 ]
Hello World!
Error: Runtime: Expected the data type Str but found Int at 11:11.
```

Lambdas can also propegate their return, that means if a `ret` call occurs within the lambda it will continue the call and pass
it to the lambda that called it. This is used implicitly in the if and if_else functions. To explicitly specify this property
in the lambda creation add as the last element of the list a boolean which specefies if this property will be enabled or not.
```
let "ret_prop" (["a" true] {
    ret a;
});
let "lam" (["a"] {
    ret_prop a;
    ret true;
});

print (lam 2);
print (lam "LANG");


Output:
2
LANG
```
