@compiler(llvm: i1)
type bool;

@compiler(llvm: i8)
type byte;

@compiler(llvm: i8)
type char;

@compiler(llvm: i8)
type i8;

@compiler(llvm: i16)
type i16;

@compiler(llvm: i32)
type i32;

@compiler(llvm: i64)
type i64;

@compiler(llvm: i8)
type u8;

@compiler(llvm: i16)
type u16;

@compiler(llvm: i32)
type u32;

@compiler(llvm: i64)
type u64;

@compiler(llvm: half)
type f16;

@compiler(llvm: float)
type f32;

@compiler(llvm: double)
type f64;

@compiler(llvm: i64)
type isize;

@compiler(llvm: i64)
type usize;

struct string {
  const ptr: *const char;
  const len: usize;
}
