# Lang
An interpreted lisp like language written in rust.

### Function call
The language is made almost entirely of function calls. A function call is structured as a line, the first word in it is the
function itself everything after is an argument of that function. The newline or a semicolon signifies the end of the function call.
```
println "Hello World!"
# ^     ^-----        ^
# function   |       end of function (indicated by a new line or
#            first argument             a semicolon if neccesery)


Output:
Hello World!
```

### Data Types
#### Str
There are two ways to write a string, put it in double quotes `"Hello World"`
or write a mini-string. A mini-string is used primarily for names, it has a
single quote at its beginning and it will close automatically at the
first space or at block opening/closing.
```
println "Full string"
#       ^           ^-- ends here because of the double quote
#       begins here


println 'Mini          'string
#       ^    ^-------  ^      ^-- ends here because of the new line
#       begins here |  begins here
#                   ends here because of the space

print (typeof ')


Output:
Full String
Mini-string
Str
```
String escape characters are used for representing special characters. To
use them write a backslash and then the escape character. Here is the escape
character chart:

Escape Char | String Type | Representing
:---------: | :---------: | ------------
`\n`        | All         | New Line
`\\`        | All         | Backslash
`\"`        | Full String | Double quotes
`\t`        | All         | Tab
`\r`        | All         | Carriage Return
`\0`        | All         | Null
`\SPACE`    | Mini String | Space
`\(`        | Mini String | Open parenthesis
`\)`        | Mini String | Close parenthesis
`\[`        | Mini String | Open square bracket
`\]`        | Mini String | Close square bracket
`\{`        | Mini String | Open curly brace
`\(`        | Mini String | Close curly brace

#### Int
A signed 32 bit integer. Looks like a normal number.
```
println 2 -6 (typeof 4)


Output:
2 -6 Int
```

#### UInt
An unsigned 32 bit integer. It always has a `u` suffix to indicate that it is
a uint.
```
println 2u 7u (typeof 5u)


Output:
2
7
UInt
```

#### Float
A 32 bit floating point number. If the number doesn't include a dot then you
can add an `f` or a `.` suffix for the number to become a float.
```
println 2.6 -3. 2f (typeof 0.4)


Output:
2.6
-3.0
2.0
Float
```

#### Lambdas
An anonymous function more on it later.
```
println print typeof (typeof print)


Output:
lambda lambda Lambda
```

#### Bool
A boolean value which is used in logical functions like `if` and `while`.
```
println false true (typeof true)


Output:
false true Bool
```

#### None
A none type, it's used as the return value of some functions like `print`.
```
print none (typeof none)


Output:
none None
```

#### List
later

### Variables
To define a variable use the function `let`, the first argument is the name of
the variable as a string and the second argument is its value.
To use that variable you write its name.
For example to create the variable `foo` with value `12` you write:
```
let 'foo 12
print foo


Output:
12
```

To change the value of a variable use the function `set`. It's very similar to `let` but the variable has to be defined before
this function is called. As an example changing the value of `foo` to `"hello"` add the following line:
```
let 'foo 12
print foo

set 'foo "hello"
print foo


Output:
12
hello
```

### Scopes
A scope is a code block it returns the last expression if it's not covered by a semicolon like in rust.
All variables that are defined in a block are deleted when it's closed. Overriding variables is allowed in a scope
if the variable was defined outside the scope. An example of a use of scope is:
```
let 'a "Out of scope A"
println a
println (
    let 'a "In scope A"
    a
)
println a


Output:
Out of scope A
In scope A
Out of scope A
```
You may notice that `a` was called as a function at the end of that scope. This is normal because if you call a type which
is not meant to be used as a function it will just return itself, so in this example calling the string `"In scope A"` returns
itself (`"In scope A"`).

### Lists
A list is a dynamically sized collection of different values. A list has its elements separated
with a space. To index a list you call the function `idx` with an `int`/`uint` index.
If the index is negative then it's indexes from the end of the list. An example of a list is:
```
let 'list [ 1 "Hello" 2.4 ]
println list
println (idx list 0u) (idx list -1)


Output:
[ 1, Hello, 2.4 ]
1 2.4
```

### Lambdas
To create a lambda with no parameters use curly braces. The lambda will act like a scope, only it has to be
called like a function.
If you want to return early use the ret function.
```
let 'f {
    print "F was called"
    "F output"
}
print (f)
print '
print (f)


Output:
F was called
F output

F was called
F output
```

#### Arguments
For the lambda to accept parameters, it needs to be created by calling a list with the only argument being the lambda. The list
will be a list of the arguments the lambda receives. Each argument may be either the name of the argument or a list with the name
and the type of the argument. For example:
```
let 'f (lam ['a 'b] {
    let 'res (add a b)
    print res
    [a b]
})
print (f 1 2)
print (f 4.2 5.8)

let "print_str" ([['s str]] { print s })
print_str "Hello World!"
print_str 2


Output:
3
[ 1, 2 ]
10
[ 4.2, 5.8 ]
Hello World!
Error: Runtime: Expected the data type Str but found Int at 11:11.
```

#### Return propagation
Lambdas can also propagate their return, that means if a `ret` call occurs within the lambda it will continue the call and pass
it to the lambda that called it. This is used implicitly in the if and if_else functions. To explicitly specify this property
in the lambda creation add as the last element of the list a boolean which specifies if this property will be enabled or not.
```
let 'ret_prop (['a true] {
    ret a
})
let 'lam (['a] {
    ret_prop a
    ret true
})

print (lam 2)
print (lam "LANG")


Output:
2
LANG
```
