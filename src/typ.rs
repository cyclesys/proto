#[repr(C)]
#[derive(Clone, Copy)]
pub struct Type {
    pub name: Slice<u8>,
    pub def: TypeDef,
}

impl Type {
    pub const fn new(name: &[u8], def: TypeDef) -> Type {
        Type {
            name: Slice::<u8> {
                ptr: name.as_ptr(),
                len: name.len(),
            },
            def,
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct TypeDef {
    pub tag: Tag,
    pub data: Data,
}

impl TypeDef {
    pub const fn new(tag: Tag) -> TypeDef {
        unsafe {
            TypeDef {
                tag,
                data: std::mem::zeroed(),
            }
        }
    }

    pub const fn new_data(tag: Tag, data: Data) -> TypeDef {
        TypeDef { tag, data }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Tag {
    Void,

    I8,
    I16,
    I32,
    I64,

    U8,
    U16,
    U32,
    U64,

    F32,
    F64,

    Bool,
    Str,
    Ref,

    Optional,
    Array,
    List,

    Struct,
    Tuple,
    Enum,
    Union,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union Data {
    pub optional: Optional,
    pub array: Array,
    pub list: List,

    pub r#struct: Struct,
    pub tuple: Tuple,
    pub r#enum: Enum,
    pub r#union: Union,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Optional {
    pub child: *const TypeDef,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Array {
    pub size: u16,
    pub child: *const TypeDef,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct List {
    pub child: *const TypeDef,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Struct {
    pub fields: Slice<StructField>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct StructField {
    pub name: Slice<u8>,
    pub typ: TypeDef,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Tuple {
    pub fields: Slice<TupleField>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct TupleField {
    pub typ: TypeDef,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Enum {
    pub fields: Slice<EnumField>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct EnumField {
    pub name: Slice<u8>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Union {
    pub fields: Slice<UnionField>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct UnionField {
    pub name: Slice<u8>,
    pub typ: TypeDef,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Slice<T> {
    pub ptr: *const T,
    pub len: usize,
}

impl<T> Slice<T> {
    pub fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }
}

extern "C" {

    pub fn cycle_read_type(slice: Slice<u8>, out: *mut Type) -> bool;

    pub fn cycle_write_type(t: Type, out: *mut Slice<u8>) -> bool;

    pub fn cycle_free_type(t: Type);

    pub fn cycle_free_bytes(slice: Slice<u8>);

    pub fn cycle_type_eql(left: Type, right: Type) -> bool;

    pub fn cycle_type_valid(slice: Slice<u8>) -> bool;
}
