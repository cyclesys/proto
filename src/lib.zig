export fn cycle_read_type(slice: Slice(u8), out: *Type) bool {
    const t = readType(c_allocator, slice.toZigSlice()) catch return false;
    out.* = t;
    return true;
}

export fn cycle_write_type(t: Type, out: *Slice(u8)) bool {
    var bytes = std.ArrayList(u8).init(c_allocator);
    writeType(bytes.writer(), t) catch return false;

    const slice = bytes.toOwnedSlice() catch return false;
    out.* = Slice(u8).fromZigSlice(slice);

    return true;
}

export fn cycle_free_type(t: Type) void {
    t.deinit(c_allocator);
}

export fn cycle_free_bytes(slice: Slice(u8)) void {
    c_allocator.free(slice.toZigSlice());
}

export fn cycle_type_eql(left: Type, right: Type) bool {
    return Type.eql(left, right);
}

export fn cycle_type_valid(bytes: Slice(u8)) bool {
    var arena = std.heap.ArenaAllocator.init(c_allocator);
    defer arena.deinit();
    _ = readType(arena.allocator(), bytes.toZigSlice()) catch return false;
    return true;
}

const ty = @import("type.zig");
pub const Type = ty.Type;
pub const TypeDef = ty.TypeDef;
pub const Slice = ty.Slice;
pub const readType = ty.readType;
pub const writeType = ty.writeType;

const std = @import("std");
const c_allocator = std.heap.c_allocator;

test {
    _ = @import("type.zig");
}
