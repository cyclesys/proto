const ty = @import("type.zig");
pub const Type = ty.Type;
pub const TypeDef = ty.TypeDef;
pub const readType = ty.readType;
pub const writeType = ty.writeType;

const std = @import("std");

test {
    _ = @import("type.zig");
}
