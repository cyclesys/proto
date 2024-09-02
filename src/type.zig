const std = @import("std");

pub const Type = struct {
    name: []const u8,
    def: TypeDef,

    pub fn eql(self: Type, other: Type) bool {
        return std.mem.eql(u8, self.name, other.name) and self.def.eql(other.def);
    }

    pub fn deinit(self: Type, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.def.deinit(allocator);
    }
};

pub const TypeDef = union(enum(u8)) {
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

    optional: Optional,
    array: Array,
    list: List,

    @"struct": Struct,
    tuple: Tuple,
    @"enum": Enum,
    @"union": Union,

    pub const Optional = struct {
        child: *const TypeDef,
    };

    pub const Array = struct {
        size: u16,
        child: *const TypeDef,
    };

    pub const List = struct {
        child: *const TypeDef,
    };

    pub const Struct = struct {
        fields: []const StructField,
    };

    pub const StructField = struct {
        name: []const u8,
        type: TypeDef,
    };

    pub const Tuple = struct {
        fields: []const TupleField,
    };

    pub const TupleField = struct {
        type: TypeDef,
    };

    pub const Enum = struct {
        fields: []const EnumField,
    };

    pub const EnumField = struct {
        name: []const u8,
    };

    pub const Union = struct {
        fields: []const UnionField,
    };

    pub const UnionField = struct {
        name: []const u8,
        type: TypeDef,
    };

    pub fn eql(self: TypeDef, other: TypeDef) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) {
            return false;
        }

        switch (self) {
            .optional => |opt| {
                return eql(opt.child.*, other.optional.child.*);
            },
            .array => |arr| {
                return (arr.size == other.array.size) and eql(arr.child.*, other.array.child.*);
            },
            .list => |list| {
                return eql(list.child.*, other.list.child.*);
            },
            .@"struct" => |s| {
                const other_s = other.@"struct";
                if (s.fields.len != other_s.fields.len) {
                    return false;
                }
                for (s.fields, 0..) |field, i| {
                    const other_field = other_s.fields[i];
                    if (!std.mem.eql(u8, field.name, other_field.name) or
                        !eql(field.type, other_field.type))
                    {
                        return false;
                    }
                }
                return true;
            },
            .tuple => |t| {
                const other_t = other.tuple;
                if (t.fields.len != other_t.fields.len) {
                    return false;
                }
                for (t.fields, 0..) |field, i| {
                    const other_field = other_t.fields[i];
                    if (!eql(field.type, other_field.type)) {
                        return false;
                    }
                }
                return true;
            },
            .@"enum" => |e| {
                const other_e = other.@"enum";
                if (e.fields.len != other_e.fields.len) {
                    return false;
                }
                for (e.fields, 0..) |field, i| {
                    const other_field = other_e.fields[i];
                    if (!std.mem.eql(u8, field.name, other_field.name)) {
                        return false;
                    }
                }
                return true;
            },
            .@"union" => |u| {
                const other_u = other.@"union";
                if (u.fields.len != other_u.fields.len) {
                    return false;
                }
                for (u.fields, 0..) |field, i| {
                    const other_field = other_u.fields[i];
                    if (!std.mem.eql(u8, field.name, other_field.name) or
                        !eql(field.type, other_field.type))
                    {
                        return false;
                    }
                }
                return true;
            },
            else => {
                return true;
            },
        }
    }

    pub fn deinit(self: TypeDef, allocator: std.mem.Allocator) void {
        switch (self) {
            .optional => |opt| {
                opt.child.deinit(allocator);
                allocator.destroy(opt.child);
            },
            .array => |arr| {
                arr.child.deinit(allocator);
                allocator.destroy(arr.child);
            },
            .list => |list| {
                list.child.deinit(allocator);
                allocator.destroy(list.child);
            },
            .@"struct" => |s| {
                for (s.fields) |field| {
                    allocator.free(field.name);
                    field.type.deinit(allocator);
                }
                allocator.free(s.fields);
            },
            .tuple => |t| {
                for (t.fields) |field| {
                    field.type.deinit(allocator);
                }
                allocator.free(t.fields);
            },
            .@"enum" => |e| {
                for (e.fields) |field| {
                    allocator.free(field.name);
                }
                allocator.free(e.fields);
            },
            .@"union" => |u| {
                for (u.fields) |field| {
                    allocator.free(field.name);
                    field.type.deinit(allocator);
                }
                allocator.free(u.fields);
            },
            else => {},
        }
    }
};

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

pub fn writeType(writer: anytype, t: Type) Error!void {
    // magic string
    try writer.writeAll(magic_string);

    // format version
    try writer.writeInt(u8, 1, .big);

    try writeTypeName(writer, t.name);
    try writeTypeDef(writer, t.def);
}

fn writeTypeDef(writer: anytype, def: TypeDef) !void {
    try writer.writeInt(u8, @intFromEnum(def), .big);
    switch (def) {
        .void, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .bool, .str, .ref => {},
        .optional => |opt| {
            try writeTypeDef(writer, opt.child.*);
        },
        .array => |arr| {
            try writer.writeInt(u16, arr.size, .big);
            try writeTypeDef(writer, arr.child.*);
        },
        .list => |list| {
            try writeTypeDef(writer, list.child.*);
        },
        .@"struct" => |s| {
            try writeFieldsLen(writer, s.fields.len);
            for (s.fields) |field| {
                try writeFieldName(writer, field.name);
                try writeTypeDef(writer, field.type);
            }
        },
        .tuple => |t| {
            try writeFieldsLen(writer, t.fields.len);
            for (t.fields) |field| {
                try writeTypeDef(writer, field.type);
            }
        },
        .@"enum" => |e| {
            try writeFieldsLen(writer, e.fields.len);
            for (e.fields) |field| {
                try writeFieldName(writer, field.name);
            }
        },
        .@"union" => |u| {
            try writeFieldsLen(writer, u.fields.len);
            for (u.fields) |field| {
                try writeFieldName(writer, field.name);
                try writeTypeDef(writer, field.type);
            }
        },
    }
}

fn writeTypeName(writer: anytype, name: []const u8) !void {
    if (!validateTypeName(name)) return error.InvalidTypeName;
    try writeName(writer, name);
}

fn writeFieldName(writer: anytype, name: []const u8) !void {
    if (!validateFieldName(name)) return error.InvalidFieldName;
    try writeName(writer, name);
}

fn writeName(writer: anytype, name: []const u8) !void {
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

pub fn readType(allocator: std.mem.Allocator, bytes: []const u8) Error!Type {
    var stream = std.io.fixedBufferStream(bytes);
    const reader = stream.reader();

    const magic_string_bytes = try reader.readBytesNoEof(magic_string.len);
    if (!std.mem.eql(u8, &magic_string_bytes, magic_string)) {
        return error.InvalidMagicString;
    }

    // in future versions this will be used to determine the format
    const version = try reader.readInt(u8, .big);
    _ = version;

    const type_name = try readTypeName(allocator, reader);
    const type_def = try readTypeDef(allocator, reader);
    return Type{
        .name = type_name,
        .def = type_def,
    };
}

fn readTypeDef(allocator: std.mem.Allocator, reader: anytype) !TypeDef {
    const tag = try std.meta.intToEnum(std.meta.Tag(TypeDef), try reader.readInt(u8, .big));
    switch (tag) {
        .optional => {
            const child_ptr = try allocator.create(TypeDef);
            child_ptr.* = try readTypeDef(allocator, reader);
            return TypeDef{ .optional = .{ .child = child_ptr } };
        },
        .array => {
            const size = try reader.readInt(u16, .big);

            const child_ptr = try allocator.create(TypeDef);
            child_ptr.* = try readTypeDef(allocator, reader);

            return TypeDef{ .array = .{
                .size = size,
                .child = child_ptr,
            } };
        },
        .list => {
            const child_ptr = try allocator.create(TypeDef);
            child_ptr.* = try readTypeDef(allocator, reader);
            return TypeDef{ .list = .{ .child = child_ptr } };
        },
        .@"struct" => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.StructField, fields_len);
            for (fields) |*field| {
                field.name = try readFieldName(allocator, reader);
                field.type = try readTypeDef(allocator, reader);
            }
            return TypeDef{ .@"struct" = .{ .fields = fields } };
        },
        .tuple => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.TupleField, fields_len);
            for (fields) |*field| {
                field.type = try readTypeDef(allocator, reader);
            }
            return TypeDef{ .tuple = .{ .fields = fields } };
        },
        .@"enum" => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.EnumField, fields_len);
            for (fields) |*field| {
                field.name = try readFieldName(allocator, reader);
            }
            return TypeDef{ .@"enum" = .{ .fields = fields } };
        },
        .@"union" => {
            const fields_len = try reader.readInt(u16, .big);
            const fields = try allocator.alloc(TypeDef.UnionField, fields_len);
            for (fields) |*field| {
                field.name = try readFieldName(allocator, reader);
                field.type = try readTypeDef(allocator, reader);
            }
            return TypeDef{ .@"union" = .{ .fields = fields } };
        },
        inline else => |comptime_tag| {
            // void, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool, str, ref
            return @unionInit(TypeDef, @tagName(comptime_tag), undefined);
        },
    }
}

fn readTypeName(allocator: std.mem.Allocator, reader: anytype) ![]const u8 {
    const name = try readName(allocator, reader);
    if (!validateTypeName(name)) return error.InvalidTypeName;
    return name;
}

fn readFieldName(allocator: std.mem.Allocator, reader: anytype) ![]const u8 {
    const name = try readName(allocator, reader);
    if (!validateFieldName(name)) return error.InvalidFieldName;
    return name;
}

fn readName(allocator: std.mem.Allocator, reader: anytype) ![]const u8 {
    const len = try reader.readInt(u16, .big);
    const buf = try allocator.alloc(u8, len);
    try reader.readNoEof(buf);
    if (!std.unicode.utf8ValidateSlice(buf)) {
        return error.InvalidUtf8;
    }
    return buf;
}

// Type names consist of segments separated by periods.
// Each segment can contain only alphanumeric characters, and must not be empty.
fn validateTypeName(name: []const u8) bool {
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

fn validateFieldName(name: []const u8) bool {
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
    try testTypeSerDe(.void);
    try testTypeSerDe(.i8);
    try testTypeSerDe(.i16);
    try testTypeSerDe(.i32);
    try testTypeSerDe(.i64);
    try testTypeSerDe(.u8);
    try testTypeSerDe(.u16);
    try testTypeSerDe(.u32);
    try testTypeSerDe(.u64);
    try testTypeSerDe(.f32);
    try testTypeSerDe(.f64);
    try testTypeSerDe(.bool);
    try testTypeSerDe(.str);
    try testTypeSerDe(.ref);
}

test "optional type ser/de" {
    try testTypeSerDe(.{ .optional = .{
        .child = &.{ .void = undefined },
    } });
}

test "array type ser/de" {
    try testTypeSerDe(.{ .array = .{
        .size = 8,
        .child = &.{ .u8 = undefined },
    } });
}

test "list type ser/de" {
    try testTypeSerDe(.{ .list = .{
        .child = &.{ .u8 = undefined },
    } });
}

test "struct type ser/de" {
    try testTypeSerDe(.{ .@"struct" = .{
        .fields = &.{
            .{ .name = "a", .type = .u8 },
            .{ .name = "b", .type = .i32 },
        },
    } });
}

test "tuple type ser/de" {
    try testTypeSerDe(.{ .tuple = .{
        .fields = &.{
            .{ .type = .u8 },
            .{ .type = .i32 },
        },
    } });
}

test "enum type ser/de" {
    try testTypeSerDe(.{ .@"enum" = .{
        .fields = &.{
            .{ .name = "a" },
            .{ .name = "b" },
        },
    } });
}

test "union type ser/de" {
    try testTypeSerDe(.{ .@"union" = .{
        .fields = &.{
            .{ .name = "a", .type = .u8 },
            .{ .name = "b", .type = .i32 },
        },
    } });
}

fn testTypeSerDe(def: TypeDef) !void {
    const expected = Type{
        .name = "test.Type",
        .def = def,
    };

    var bytes = std.ArrayList(u8).init(std.testing.allocator);
    defer bytes.deinit();

    try writeType(bytes.writer(), expected);

    const actual = try readType(std.testing.allocator, bytes.items);
    defer actual.deinit(std.testing.allocator);

    try std.testing.expect(Type.eql(expected, actual));
}
