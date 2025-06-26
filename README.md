
# clox — Lox Interpreter in C

`clox` is an implementation of the [Lox programming language](https://craftinginterpreters.com/) in C, based on the book **Crafting Interpreters** by Robert Nystrom. Lox is a simple, dynamically-typed, object-oriented language designed for learning interpreter and compiler design.

This project implements the **tree-walk interpreter** version (clox), written entirely in C.

---

## ✨ Features

* Full implementation of the Lox language (as per Part II of the book)
* Lexical scanner
* Pratt parser
* Resolver for variable scoping and closures
* Support for:

  * Classes and inheritance
  * Closures and first-class functions
  * Native functions (e.g., `clock()`)

---

## 🧠 Project Structure

```
clox/
├── CMakeLists.txt
├── codecrafters.yml
├── README.md
├── src
│   ├── arena.h              # Memory management
│   ├── eval.c/h             # Evaluator (Interpreter)
│   ├── main.c/h             # Entry point
│   ├── parser.c/h           # Pratt parser
│   └── scanner.c/h          # Tokenizer (lexical analysis)
├── vcpkg-configuration.json
├── vcpkg.json
└── your_program.sh
```

---

## 🛠️ Building

### Requirements

* C compiler (GCC or Clang)
* `cmake`

### Compile

```bash
git clone https://github.com/rahulsenna/crafting-interpreters-c.git 
cd crafting-interpreters-c
./your_program.sh
```

## 📚 Resources

* [Crafting Interpreters (official site)](https://craftinginterpreters.com/)
* [Lox Language Specification](https://craftinginterpreters.com/the-lox-language.html)
* [Bob Nystrom’s GitHub](https://github.com/munificent)

---

## 🧼 TODO / Improvements

* Add garbage collection statistics
* Better error messages with line/column info
* Optional static analysis or type hints
* Package manager or REPL history

---

## 📝 License

This project is open-source and available under the MIT License.
Credits to Bob Nystrom for the language and book design.
