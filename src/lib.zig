const ty = @import("type.zig");
pub const Type = ty.Type;
pub const TypeDef = ty.TypeDef;
pub const readType = ty.readType;
pub const writeType = ty.writeType;

// lib export for the rust side
export fn cycle_type_validate(ptr: [*]const u8, len: usize) bool {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    _ = readType(arena.allocator(), ptr[0..len]) catch return false;
    return true;
}

const std = @import("std");
