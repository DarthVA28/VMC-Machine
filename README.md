# **VMC Machine** 
This project implements a **VMC Machine**, which is a virtual machine with a value stack **V**, a memory **M** and a control stack **C**. For the execution of the AST developed by the compiler, we first convert it into a bracket-free postfix expression, and then operate on it using the operational semantics as defined in a later section. The evaluation is considered complete when the control stack is empty (Note that if the program does not terminate this situation will not arise).

The files as a part of this project are: 
- vmc_machine.sml (Implementation of the VMC Machine)
- stack.sml (Stack Data Structure)
- while.lex (Lexer)
- while.grm (Parser-Generator)
- while_ast.sml (Datatypes for AST)
- glue.sml (Links lexer and parser)
- compiler.sml (Contains the main compiler)
- while.cm (Links together all the files - Makefile)

## **Execution Instructions**
Follow these instructions for compiling and executing While programs. The following example assumes that the While program is contained in ``test.while`` (in general, any valid ``<filename>.while`` works).

### **Compilation**
The below commands are used to compile all the .sml files, including creating the parser-generator as well as VMC machine for the execution of the program.
```
> sml
- use "exec.sml";
- val ast = While.compile "test.while";
```
#### **Output**
```
val it = AST
    (PROG
       (BLK
          (....) : tree
```
### **Execution**
```
- Vmc.execute(ast);
```
If the program terminates succesfully, you should see the message: 
```Execution successful. Exit code: 0```

### **Configuration of VMC Machine**
The configuration of the stack machine can be seen as a 3-tuple containing the configuration of the V (Value Stack), M (Memory), and C (Control Stack). This may be called as:
```
Vmc.config();
```
Note that this is slightly different from the syntax mentioned in the assignment: ```config()``` replaces ```toString()```. See the section on Design and Implementation Decisions for more information.

### **Execution in Debugging Mode**
For the ease of debugging, we have included a debugging mode which prints the state of the VMC machine after the application of each rule of the operational semantics.

The debugging mode is controlled by the value ```dbg``` which is set to false by default.

The method to use the debugging mode is as follows:
```
- Vmc.dbg := true;
```
Now, execute the program as usual using ```Vmc.execute(ast)```. Note that to turn off debugging mode we may simply set,
```
- Vmc.dbg := false;
```
An example of how the configuration of the VMC is printed in debugging mode: 
```
------------------------------
The value stack is: A
The state of the memory is: M[0] = 0 M[1] = 0 M[2] = 0
The control stack is: READ A 2 =  B  10 SET  B  32 SET ITE B WRITE
------------------------------
```

---
## **Operational Semantics**
The operational semantics of the While programming language have been provided below:
```
Rule 
1  <V, M, m.C> −→ <m.V, M, C>
2  <V, M, x.C> −→ <#xV, M, C>
3  <V, M, m.n.O.C> −→ <n.m.V, M, O.C>
4  <V, M, m.x.O.C> −→ <#x.m.V, M, x. O.C>
5  <V, M, x.m.O.C> −→ <m.#x.V, M, O.C>
6  <V, M, x.y.O.C> −→ <#y.#x.V, M, O.C>
7  <n.m.V, M, O.C> −→ <p.V, M, C>
8  <V, M, {}.C> −→ <V, M, C>
9  <V, M, x.e.SET.C> −→ <x.V, M, e.SET.C>
10 <m.x.V, M, SET.C> −→ <V, M{x ← m}, C>
11 <V, M, b.c.d.ITE.C> −→ <b.V, M, c.d.ITE.C>
12 <0.V, M, c.d.ITE.C> −→ <V, M, d.C>
13 <1.V, M, c.d.ITE.C> −→ <V, M, c.C>
14 <V, M, b.c.WH.C> −→ <c.b.V, M, b.WH.C>
15 <0.c.b.V, M, WH.C> −→ <V, M, C>
16 <1.c.b.V, M, C> −→ <V, M, c.b.c.WH.C>
```

### **Type Checking**
Type checking has been implemented for both expressions and identifiers. The implementation uses the inbuilt structure "HashTable" from SMJ/NJ. An expression/identifier having incorrect type raises an error ```TypeMismatchException```. 

Further, we also check for declaration before use using the same symbol table - if any identifier has not been declared in the header of the program, we raise an error- 
> "ERROR: Identifier not declared before use."

## **References**
- [Modern Compiler Design in Standard ML](https://www.cs.princeton.edu/~appel/modern/ml/) - Andrew W. Appel
- [SML/NJ Official Documentation](https://www.smlnj.org/doc/smlnj-lib/Util/str-HashTable.html)
- [COL226 Hyper Notes](https://www.cse.iitd.ac.in/~sak/courses/pl/pl.pdf), Section 4.6 (WHILE Language EBNF)
- [Reference for defining SML Datatypes](https://homepages.inf.ed.ac.uk/stg/NOTES/node41.html)
