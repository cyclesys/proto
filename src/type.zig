const std = @import("std");

export fn cycle_type_validate(bytes: Slice(u8)) bool {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    _ = readType(arena.allocator(), bytes.toZigSlice()) catch return false;
    return true;
}

pub const Type = extern struct {
    name: Slice(u8),
    def: TypeDef,

    pub fn eql(left: Type, right: Type) bool {
        return std.mem.eql(u8, left.name.toZigSlice(), right.name.toZigSlice()) and left.def.eql(right.def);
    }

    pub fn deinit(self: Type, allocator: std.mem.Allocator) void {
        allocator.free(self.name.toZigSlice());
        self.def.deinit(allocator);
    }
};

pub const TypeDef = extern struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        void,

        i8,
        i16,
        i32,
        i64,

        u8,
        u16,
        u32,
        u64,

        f32,
        f64,

        bool,
        str,
        ref,

        optional,
        array,
        list,

        @"struct",
        tuple,
        @"enum",
        @"union",
    };

    pub const Data = extern union {
        void: void,

        i8: void,
        i16: void,
        i32: void,
        i64: void,

        u8: void,
        u16: void,
        u32: void,
        u64: void,

        f32: void,
        f64: void,

        bool: void,
        str: void,
        ref: void,

        optional: Optional,
        array: Array,
        list: List,

        @"struct": Struct,
        tuple: Tuple,
        @"enum": Enum,
        @"union": Union,
    };

    pub const Optional = extern struct {
        child: *const TypeDef,
    };

    pub const Array = extern struct {
        size: u16,
        child: *const TypeDef,
    };

    pub const List = extern struct {
        child: *const TypeDef,
    };

    pub const Struct = extern struct {
        fields: Slice(StructField),
    };

    pub const StructField = extern struct {
        name: Slice(u8),
        type: TypeDef,
    };

    pub const Tuple = extern struct {
        fields: Slice(TupleField),
    };

    pub const TupleField = extern struct {
        type: TypeDef,
    };

    pub const Enum = extern struct {
        fields: Slice(EnumField),
    };

    pub const EnumField = extern struct {
        name: Slice(u8),
    };

    pub const Union = extern struct {
        fields: Slice(UnionField),
    };

    pub const UnionField = extern struct {
        name: Slice(u8),
        type: TypeDef,
    };

    pub fn eql(left: TypeDef, right: TypeDef) bool {
        return left.tag == right.tag and dataEql(left.data, right.data, left.tag);
    }

    fn dataEql(left: Data, right: Data, tag: Tag) bool {
        switch (tag) {
            .optional => {
                return eql(left.optional.child.*, right.optional.child.*);
            },
            .array => {
                return (left.array.size == right.array.size) and eql(left.array.child.*, right.array.child.*);
            },
            .list => {
                return eql(left.list.child.*, right.list.child.*);
            },
            .@"struct" => {
                const left_fields = left.@"struct".fields.toZigSlice();
                const right_fields = right.@"struct".fields.toZigSlice();

                if (left_fields.len != right_fields.len) {
                    return false;
                }

                for (left_fields, right_fields) |left_field, right_field| {
                    if (!std.mem.eql(u8, left_field.name.toZigSlice(), right_field.name.toZigSlice()) or
                        !eql(left_field.type, right_field.type))
                    {
                        return false;
                    }
                } else {
                    return true;
                }
            },
            .tuple => {
                const left_fields = left.tuple.fields.toZigSlice();
                const right_fields = right.tuple.fields.toZigSlice();

                if (left_fields.len != right_fields.len) {
                    return false;
                }

                for (left_fields, right_fields) |left_field, right_field| {
                    if (!eql(left_field.type, right_field.type)) {
                        return false;
                    }
                } else {
                    return true;
                }
            },
            .@"enum" => {
                const left_fields = left.@"enum".fields.toZigSlice();
                const right_fields = right.@"enum".fields.toZigSlice();

                if (left_fields.len != right_fields.len) {
                    return false;
                }

                for (left_fields, right_fields) |left_field, right_field| {
                    if (!std.mem.eql(u8, left_field.name.toZigSlice(), right_field.name.toZigSlice())) {
                        return false;
                    }
                } else {
                    return true;
                }
            },
            .@"union" => {
                const left_fields = left.@"union".fields.toZigSlice();
                const right_fields = right.@"union".fields.toZigSlice();

                if (left_fields.len != right_fields.len) {
                    return false;
                }

                for (left_fields, right_fields) |left_field, right_field| {
                    if (!std.mem.eql(u8, left_field.name.toZigSlice(), right_field.name.toZigSlice()) or
                        !eql(left_field.type, right_field.type))
                    {
                        return false;
                    }
                } else {
                    return true;
                }
            },
            else => {
                return true;
            },
        }
    }

    pub fn deinit(self: TypeDef, allocator: std.mem.Allocator) void {
        switch (self.tag) {
            .optional => {
                self.data.optional.child.deinit(allocator);
                allocator.destroy(self.data.optional.child);
            },
            .array => {
                self.data.array.child.deinit(allocator);
                allocator.destroy(self.data.array.child);
            },
            .list => {
                self.data.list.child.deinit(allocator);
                allocator.destroy(self.data.list.child);
            },
            .@"struct" => {
                const fields = self.data.@"struct".fields.toZigSlice();
                for (fields) |field| {
                    allocator.free(field.name.toZigSlice());
                    field.type.deinit(allocator);
                }
                allocator.free(fields);
            },
            .tuple => {
                const fields = self.data.tuple.fields.toZigSlice();
                for (fields) |field| {
                    field.type.deinit(allocator);
                }
                allocator.free(fields);
            },
            .@"enum" => {
                const fields = self.data.@"enum".fields.toZigSlice();
                for (fields) |field| {
                    allocator.free(field.name.toZigSlice());
                }
                allocator.free(fields);
            },
            .@"union" => {
                const fields = self.data.@"union".fields.toZigSlice();
                for (fields) |field| {
                    allocator.free(field.name.toZigSlice());
                    field.type.deinit(allocator);
                }
                allocator.free(fields);
            },
            else => {},
        }
    }
};

pub fn Slice(comptime T: type) type {
    return extern struct {
        ptr: [*]const T,
        len: usize,

        const Self = @This();

        pub fn fromZigSlice(slice: []const T) Self {
            var self: Self = undefined;
            self.ptr = slice.ptr;
            self.len = slice.len;
            return self;
        }

        pub fn toZigSlice(self: Self) []const T {
            var slice: []const T = undefined;
            slice.ptr = self.ptr;
            slice.len = self.len;
            return slice;
        }
    };
}

pub const Error = error{
    NameOverflow,
    FieldOverflow,
    InvalidMagicString,
    InvalidTypeName,
    InvalidFieldName,
    InvalidEnumTag,
    InvalidUtf8,
    EndOfStream,
    OutOfMemory,
};

const magic_string = "cycle type";

pub fn readType(allocator: std.mem.Allocator, bytes: []const u8) Error!Type {
    var stream = std.io.fixedBufferStream(bytes);
    const reader = stream.reader();

    const magic_string_bytes = try reader.readBytesNoEof(magic_string.len);
    if (!std.mem.eql(u8, &magic_string_bytes, magic_string)) {
        return error.InvalidMagicString;
    }

    // in future versions this will be used to determine the format
    const format = try reader.readInt(u8, .big);
    _ = format;

    const type_name = try readTypeName(allocator, reader);
    const type_def = try readTypeDef(allocator, reader);
    return Type{
        .name = type_name,
        .def = type_def,
    };
}

fn readTypeDef(allocator: std.mem.Allocator, reader: anytype) !TypeDef {
    const tag = try std.meta.intToEnum(TypeDef.Tag, try reader.readInt(u8, .big));
    var data: TypeDef.Data = undefined;
    switch (tag) {
        .optional => {
            const child_ptr = try allocator.create(TypeDef);
            child_ptr.* = try readTypeDef(allocator, reader);
            data = .{ .optional = .{ .child = child_ptr } };
        },
        .array => {
            const size = try reader.readInt(u16, .big);

            const child_ptr = try allocator.create(TypeDef);
            child_ptr.* = try readTypeDef(allocator, reader);

            data = .{ .array = .{
                .size = size,
                .child = child_ptr,
            } };
        },
        .list => {
            const child_ptr = try allocator.create(TypeDef);
            child_ptr.* = try readTypeDef(allocator, reader);
            data = .{ .list = .{ .child = child_ptr } };
        },
        .@"struct" => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.StructField, fields_len);
            for (fields) |*field| {
                field.name = try readFieldName(allocator, reader);
                field.type = try readTypeDef(allocator, reader);
            }
            data = .{ .@"struct" = .{ .fields = Slice(TypeDef.StructField).fromZigSlice(fields) } };
        },
        .tuple => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.TupleField, fields_len);
            for (fields) |*field| {
                field.type = try readTypeDef(allocator, reader);
            }
            data = .{ .tuple = .{ .fields = Slice(TypeDef.TupleField).fromZigSlice(fields) } };
        },
        .@"enum" => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.EnumField, fields_len);
            for (fields) |*field| {
                field.name = try readFieldName(allocator, reader);
            }
            data = .{ .@"enum" = .{ .fields = Slice(TypeDef.EnumField).fromZigSlice(fields) } };
        },
        .@"union" => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.UnionField, fields_len);
            for (fields) |*field| {
                field.name = try readFieldName(allocator, reader);
                field.type = try readTypeDef(allocator, reader);
            }
            data = .{ .@"union" = .{ .fields = Slice(TypeDef.UnionField).fromZigSlice(fields) } };
        },
        inline else => |comptime_tag| {
            // void, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool, str, ref
            data = @unionInit(TypeDef.Data, @tagName(comptime_tag), undefined);
        },
    }
    return TypeDef{
        .tag = tag,
        .data = data,
    };
}

fn readTypeName(allocator: std.mem.Allocator, reader: anytype) !Slice(u8) {
    const name = try readName(allocator, reader);
    if (!validateTypeName(name)) return error.InvalidTypeName;
    return name;
}

fn readFieldName(allocator: std.mem.Allocator, reader: anytype) !Slice(u8) {
    const name = try readName(allocator, reader);
    if (!validateFieldName(name)) return error.InvalidFieldName;
    return name;
}

fn readName(allocator: std.mem.Allocator, reader: anytype) !Slice(u8) {
    // First read the length of the string
    const len = try reader.readInt(u16, .big);

    // Allocate and read a string with the given length
    const buf = try allocator.alloc(u8, len);
    try reader.readNoEof(buf);

    // Validate that the string is valid utf8
    if (!std.unicode.utf8ValidateSlice(buf)) {
        return error.InvalidUtf8;
    }

    // Convert the Zig slice to the C compatible slice struct
    var slice: Slice(u8) = undefined;
    slice.ptr = buf.ptr;
    slice.len = buf.len;
    return slice;
}

pub fn writeType(writer: anytype, t: Type) Error!void {
    // magic string
    try writer.writeAll(magic_string);

    // format version
    try writer.writeInt(u8, 1, .big);

    try writeTypeName(writer, t.name);
    try writeTypeDef(writer, t.def);
}

fn writeTypeDef(writer: anytype, def: TypeDef) !void {
    try writer.writeInt(u8, @intFromEnum(def.tag), .big);
    switch (def.tag) {
        .void, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .bool, .str, .ref => {},
        .optional => {
            try writeTypeDef(writer, def.data.optional.child.*);
        },
        .array => {
            const arr = def.data.array;
            try writer.writeInt(u16, arr.size, .big);
            try writeTypeDef(writer, arr.child.*);
        },
        .list => {
            try writeTypeDef(writer, def.data.list.child.*);
        },
        .@"struct" => {
            const fields = def.data.@"struct".fields.toZigSlice();
            try writeFieldsLen(writer, fields.len);
            for (fields) |field| {
                try writeFieldName(writer, field.name);
                try writeTypeDef(writer, field.type);
            }
        },
        .tuple => {
            const fields = def.data.tuple.fields.toZigSlice();
            try writeFieldsLen(writer, fields.len);
            for (fields) |field| {
                try writeTypeDef(writer, field.type);
            }
        },
        .@"enum" => {
            const fields = def.data.@"enum".fields.toZigSlice();
            try writeFieldsLen(writer, fields.len);
            for (fields) |field| {
                try writeFieldName(writer, field.name);
            }
        },
        .@"union" => {
            const fields = def.data.@"union".fields.toZigSlice();
            try writeFieldsLen(writer, fields.len);
            for (fields) |field| {
                try writeFieldName(writer, field.name);
                try writeTypeDef(writer, field.type);
            }
        },
    }
}

fn writeTypeName(writer: anytype, slice: Slice(u8)) !void {
    if (!validateTypeName(slice)) return error.InvalidTypeName;
    try writeName(writer, slice);
}

fn writeFieldName(writer: anytype, slice: Slice(u8)) !void {
    if (!validateFieldName(slice)) return error.InvalidFieldName;
    try writeName(writer, slice);
}

fn writeName(writer: anytype, slice: Slice(u8)) !void {
    const name = slice.toZigSlice();

    if (name.len > std.math.maxInt(u16)) {
        return error.NameOverflow;
    }
    try writer.writeInt(u16, @intCast(name.len), .big);
    try writer.writeAll(name);
}

fn writeFieldsLen(writer: anytype, len: usize) !void {
    if (len > std.math.maxInt(u16)) {
        return error.FieldOverflow;
    }
    try writer.writeInt(u16, @intCast(len), .big);
}

// Type names consist of segments separated by periods.
// Each segment can contain only alphanumeric characters, and must not be empty.
fn validateTypeName(slice: Slice(u8)) bool {
    const name = slice.toZigSlice();

    var segment_start = true;
    for (name) |c| {
        if (segment_start) {
            switch (c) {
                'A'...'Z', 'a'...'z' => {
                    segment_start = false;
                },
                else => return false,
            }
        } else {
            switch (c) {
                'A'...'Z', 'a'...'z', '0'...'9' => {},
                '.' => {
                    segment_start = true;
                },
                else => return false,
            }
        }
    }

    // Empty names, and names ending with a period are invalid.
    return !segment_start;
}

fn validateFieldName(slice: Slice(u8)) bool {
    const name = slice.toZigSlice();

    if (name.len == 0) return false;

    switch (name[0]) {
        'A'...'Z', 'a'...'z', '_' => {},
        else => return false,
    }

    for (name[1..]) |c| {
        switch (c) {
            'A'...'Z', 'a'...'z', '0'...'9', '_' => {},
            else => return false,
        }
    }
    return true;
}

test "basic types ser/de" {
    try testTypeSerDe(.{ .tag = .void, .data = undefined });
    try testTypeSerDe(.{ .tag = .i8, .data = undefined });
    try testTypeSerDe(.{ .tag = .i16, .data = undefined });
    try testTypeSerDe(.{ .tag = .i32, .data = undefined });
    try testTypeSerDe(.{ .tag = .i64, .data = undefined });
    try testTypeSerDe(.{ .tag = .u8, .data = undefined });
    try testTypeSerDe(.{ .tag = .u16, .data = undefined });
    try testTypeSerDe(.{ .tag = .u32, .data = undefined });
    try testTypeSerDe(.{ .tag = .u64, .data = undefined });
    try testTypeSerDe(.{ .tag = .f32, .data = undefined });
    try testTypeSerDe(.{ .tag = .f64, .data = undefined });
    try testTypeSerDe(.{ .tag = .bool, .data = undefined });
    try testTypeSerDe(.{ .tag = .str, .data = undefined });
    try testTypeSerDe(.{ .tag = .ref, .data = undefined });
}

test "optional type ser/de" {
    try testTypeSerDe(.{
        .tag = .optional,
        .data = .{
            .optional = .{
                .child = &.{ .tag = .void, .data = undefined },
            },
        },
    });
}

test "array type ser/de" {
    try testTypeSerDe(.{
        .tag = .array,
        .data = .{
            .array = .{
                .size = 8,
                .child = &.{ .tag = .u8, .data = undefined },
            },
        },
    });
}

test "list type ser/de" {
    try testTypeSerDe(.{
        .tag = .list,
        .data = .{
            .list = .{
                .child = &.{ .tag = .u8, .data = undefined },
            },
        },
    });
}

test "struct type ser/de" {
    try testTypeSerDe(.{
        .tag = .@"struct",
        .data = .{
            .@"struct" = .{
                .fields = Slice(TypeDef.StructField).fromZigSlice(&.{
                    .{ .name = Slice(u8).fromZigSlice("a"), .type = .{ .tag = .u8, .data = undefined } },
                    .{ .name = Slice(u8).fromZigSlice("b"), .type = .{ .tag = .i32, .data = undefined } },
                }),
            },
        },
    });
}

test "tuple type ser/de" {
    try testTypeSerDe(.{
        .tag = .tuple,
        .data = .{
            .tuple = .{
                .fields = Slice(TypeDef.TupleField).fromZigSlice(&.{
                    .{ .type = .{ .tag = .u8, .data = undefined } },
                    .{ .type = .{ .tag = .i32, .data = undefined } },
                }),
            },
        },
    });
}

test "enum type ser/de" {
    try testTypeSerDe(.{
        .tag = .@"enum",
        .data = .{
            .@"enum" = .{
                .fields = Slice(TypeDef.EnumField).fromZigSlice(&.{
                    .{ .name = Slice(u8).fromZigSlice("a") },
                    .{ .name = Slice(u8).fromZigSlice("b") },
                }),
            },
        },
    });
}

test "union type ser/de" {
    try testTypeSerDe(.{
        .tag = .@"union",
        .data = .{
            .@"union" = .{
                .fields = Slice(TypeDef.UnionField).fromZigSlice(&.{
                    .{ .name = Slice(u8).fromZigSlice("a"), .type = .{ .tag = .u8, .data = undefined } },
                    .{ .name = Slice(u8).fromZigSlice("b"), .type = .{ .tag = .i32, .data = undefined } },
                }),
            },
        },
    });
}

fn testTypeSerDe(def: TypeDef) !void {
    const expected = Type{
        .name = Slice(u8).fromZigSlice("test.Type"),
        .def = def,
    };

    var bytes = std.ArrayList(u8).init(std.testing.allocator);
    defer bytes.deinit();

    try writeType(bytes.writer(), expected);

    const actual = try readType(std.testing.allocator, bytes.items);
    defer actual.deinit(std.testing.allocator);

    //try std.testing.expect(Type.eql(expected, actual));
}
