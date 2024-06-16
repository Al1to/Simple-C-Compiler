# Simple C Compiler

### О проекте
Пока что проект недописан. Есть только вроде готовый лексер и минимальный парсер. Позже проект будет дописан.
<br/>
<br/>

## Лексер
Эта штука разбивает текст *(код)* на токены и возвращает вектор со всеми ними. У меня все токены расписаны в `enum Token` в файле `lexer.rs`.
Он умеет обрабатывать все эти символы:
```c
( ) [ ] { } ; , . > < * / % - + & = ! 
```
Также все эти комбинации символов:
```с
>= <= *= /= %= -= -- += ++ && || == != 
```
Еще все эти ключевые слова:
```c
if else for while break continue return int float char void
```
Еще числа, в том числе с точкой.
<br/>Еще умеет обрабатывать char (`'a'`).
<br/>Ну и на конец, все что не стало чем-то из предыдущего и подходит под такие критерии: 
1) начинается с буквы,
2) продолжается буквами, цифрами, или символом `_`,

становится идентификатором - именем переменной, функции, или т.п.
<br/>Ну и в довесок, лексер умеет обрабатывать комментарии, начинающиеся с `//`.
<br/>
<br/>

#### Пример работы лексера. 
Дан вот такой код на C:
```c
int main() {
    int x = 2;
    x += test(a) + 2;
    return 0;
}
```
Лексер выдаст нам вот такой вектор токенов:
```rust
[IntT, Id { id: "main" }, LP, RP, LC, IntT, Id { id: "x" }, Assignment, Inum { n: 2 }, Semicolon, Id { id: "x" }, AddAssign, Id { id: "test" }, LP, Id { id: "a" }, RP, Plus, Inum { n: 2 }, Semicolon, Return, Inum { n: 0 }, Semicolon, RC]
```
<br/>

## Парсер
Грамматика парсера:
```bash
Program ::= [ FuncDecl ]

FuncDecl  ::= Type ID ArgsDecl Block
ArgsDecl  ::= "(" _ ")" | "(" ArgDecl ")" | "(" [ ArgDecl "," ] ArgDecl ")"
Args      ::= "(" _ ")" | "(" Arg ")" | "(" [ Arg "," ] Arg ")"
ArgDecl   ::= Type ID
Arg       ::= Expr

VarDecl ::= Type ID ";" | ...TODO...

If     ::= "if" "(" Expr ")" Block [ ElseIf ] { Else }
ElseIf ::= "else" If
Else   ::= "else" Block

For ::= "for" "(" Stmt Stmt Expr ")" Block

While ::= "while" "(" Expr ")" Block

Block ::= "{" [ Stmt ] "}" | Stmt

Stmt  ::= Expr ";" | VarDecl | Var | If | For | While | "break" ";" | "continue" ";" | "return" Expr ";" | ";"
Expr  ::= Terma | Terma "=" Expr | Terma "+=" Expr | Terma "-=" Expr | Terma "*=" Expr | Terma "/=" Expr | Terma "%=" Expr
Terma ::= Termb | Termb "||" Terma
Termb ::= Termc | Termc "&&" Termb
Termc ::= Termd | Termd "==" Termc | Termd "!=" Termc
Termd ::= Terme | Terme ">" Termd | Terme "<" Termd | Terme ">=" Termd  | Terme "<=" Termd
Terme ::= Termf | Termf "+" Terme | Termf "-" Terme
Termf ::= Termg | Termg "*" Termf | Termg "/" Termf | Termg "%" Termf
Termg ::= "&" Termh | "*" Termh | "!" Termh | "++" Termh | "--" Termh | "+" Termh | "-" Termh | Termh
Termh ::= Fact "++" | Fact "--" | Fact Args | Fact "[" Expr "]" | Type "(" Expr ")"
Fact  ::= NUM | ID | "(" Expr ")"

Type ::= "int" | "float" | "char" | "void" | "*" Type | "&" Type | Type ID "[" Expr "]"
```
<br/>

#### Пример работы парсера.
Из вот такого кода на C:
```c
int func() {
    // int x = 9;     // декларирование переменных, пока что, мой прасер нормально обрабатывать не умеет 
    x += test(a) + 2; // надо дописать функцию parse_var_decl
    return x*6;       // хотя этот код я довольно давно писал, поэтому он выглядит даже для меня едва ли читаемым 
}
```
Лексер создает вот такой вектор токенов:
```rust
[IntT, Id { id: "func" }, LP, RP, LC, Id { id: "x" }, AddAssign, Id { id: "test" }, LP, Id { id: "a" }, RP, Plus, Inum { n: 2 }, Semicolon, Return, Id { id: "x" }, Star, Inum { n: 6 }, Semicolon, RC]
```
Парсер из этого вектора создает вот такое дерево:
```rust
Block { v: [RefCell { value: FuncDecl { t: Int, id: "func", args: Block { v: [] }, block: Block { v: [RefCell { value: Expr { e: BinOp { o: AddAssign { le: Id { s: "x" }, re: BinOp { o: Add { le: UnOp { o: FuncCall { id: "test", args: Block { v: [RefCell { value: Expr { e: Id { s: "a" } } }] } } }, re: IntN { num: 2 } } } } } } }, RefCell { value: Return { e: BinOp { o: Mul { le: Id { s: "x" }, re: IntN { num: 6 } } } } }] } } }] }
```
