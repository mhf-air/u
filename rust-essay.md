# the essence of Rust I learned

## Note

- all the code below are run on a 64 bit machine

### the Stack and the Heap

	- call stacks reside on the stack
	- each variable within a function has a type with a fixed size, say variable abc has a type
	that is N bytes, everytime abc is declared, N bytes are pushed on the stack
	- of the N bytes a type occupies,
	it may contain pointers(which point to somewhere on the stack or on the heap),
	and/or non-pointer types(i.e. i32, f64, bool, char, etc.),
	and/or paddings(i.e. within a struct)

	- String and Vec[T] (24 bytes)
	have the same memory layout: Struct(ptr, len, cap),
	- slice types: &str, &{{}} (16 bytes)
	(i.e. string slice, string literal, or array slice)
	have the same memory layout: Struct(ptr, len)

	- str is the text itself, which is variable sized, so you can't declare a variable of type str.
	You have to use it behind a pointer, like &str.
	- string literals are of type &str(actually &'static str), which is a pointer to some text
	resided on the text segment of the binary

	- pointer types (8 bytes), can point to anywhere(stack, heap, text segment, static variable, etc.)
		- &T, const reference
		- &mut T, mutable reference
		- *const T, const pointer
		- *mut T, mutable pointer
	- pointer like types (a pointer and possibly some meta data), can only point to the heap
		- Box (8 bytes)
		- Rc/Arc (8 bytes)
		- String (24 bytes)
		- Vec (24 bytes)
		- slice types: &str, &{{}} (16 bytes)

### ownership

	- reference types
		- &T, &mut T
		- &str, &mut str
		- &{{}}, &mut {{}}

	- multiple-owner types
		- Rc, Arc

	- single-owner types (the rest types)
		- String, Vec
		- Option, Result
		- etc.

### compared with C

	Rust puts extreme restrictions on C by default, then offer workarounds

	- can't mutate data behind immutable reference

		workaround: use Cell[T], Ref-cell[T], or Unsafe-cell[T] in single-threaded,
			and use Mutex[T] or something similar in multi-theaded

		design decision:
