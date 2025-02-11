const std = @import("std");
const Type = std.builtin.Type;

/// Generates a lexer iterator with a method that returns an union defining the matched token
/// ---
/// Parameters:
/// - `Space` defines a type to just put some state for the lexer actions. By default
/// the lexer already comes with some bear bones state. This includes:
///   - Line and column in the text
/// - `tokens` describes a tuple of matching patterns, each corresponding to a variant
/// in the token type returned by the lexer type.
/// ---
/// Example:
/// ```zig
/// Lexer(void, .{
///     .variant_with_void_type = "matching_pattern",
///     .plus_eq = "+=",
///     .variant_with_type_or_action = struct {
///         pub const capture = "(0|([1-9][0-9]*))(.[0-9]+)?";
///
///         // You can omit this if you like, which will make this equivalent to the above patterns
///         pub fn action(state: *State, match: []const u8) !f64 {
///             return std.fmt.parseFloat(f64, match);
///         }
///     },
///     .whitespace = struct {
///         pub const skip = "[ \t\n]+";
///
///         // You can ommit this as well like above
///         pub fn action(state: *State, match: []const u8) void {
///             // Even though the whitespace should be skipped you can still see what matched here
///             _ = state;
///             _ = match;
///         }
///     },
/// })
/// ```
pub fn Lexer(comptime State: type, comptime tokens: anytype) type {
    _ = State;

    return struct {
        pub const Token = TokenUnion(TokenTag(tokens), tokens);
    };
}

// Generates an enum, every variant corresponding to a struct field passed in
// ---
// - `tokens` generate a unique variant for each member of the anonymous struct
fn TokenTag(comptime tokens: anytype) type {
    const fields = std.meta.fields(@TypeOf(tokens));
    var variants: [fields.len]Type.EnumField = undefined;
    for (fields, 0..) |field, idx| {
        variants[idx] = .{ .name = field.name, .value = idx };
    }

    return @Type(Type{ .Enum = .{
        .tag_type = u32,
        .fields = &variants,
        .decls = &.{},
        .is_exhaustive = false,
    } });
}

/// Generates a union with every variant corresponding to a struct field passed in
/// ---
/// - `Tag` the tag for the generated union to use (must have same variant names as fields's fields)
/// - `fields` fields to generate variants for, with same names as variants in Tag
fn TokenUnion(comptime Tag: type, comptime tokens: anytype) type {
    const tag_variants = std.meta.fields(Tag);
    var union_variants: [tag_variants.len]Type.UnionField = undefined;
    for (tag_variants, &union_variants) |tag_variant, *union_variant| {
        // This is the real value of the field, which may be a `str` or a `type` refrencing a struct
        const field = @field(tokens, tag_variant.name);
        
        // If the type of it is a `type` and that type has a function `action`, get it's return type
        const VariantType = if (@TypeOf(field) == type and @hasDecl(field, "action"))
            switch (@typeInfo(@TypeOf(field.action))) {
                // If its an error union, get its underlying type
                .Fn => |fn_info| switch (@typeInfo(fn_info.return_type orelse void)) {
                    .ErrorUnion => |return_info| return_info.payload,
                    else => fn_info.return_type orelse void,
                },
                else => void,
            }
        else
            void;
        
        // Use the found type to generate the union variant
        union_variant.* = .{
            .name = tag_variant.name,
            .type = VariantType,
            .alignment = @alignOf(VariantType),
        };
    }

    return @Type(Type{ .Union = .{
        .layout = .auto,
        .tag_type = Tag,
        .fields = &union_variants,
        .decls = &.{},
    } });
}

test "Token" {
    const lexer = Lexer(void, .{
        .digit = struct {
            pub const capture = "[0-9]";
            pub fn action(_: void, match: []const u8) !u4 {
                return match[0] - '0';
            }
        },
        .ident = "[a-zA-Z_][a-zA-Z0-9_]*",
        .plus = "+",
        .minus = "-",
    });

    const tags = std.meta.fields(std.meta.Tag(lexer.Token));
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "digit",
        .value = 0,
    }, tags[0]);
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "ident",
        .value = 1,
    }, tags[1]);
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "plus",
        .value = 2,
    }, tags[2]);
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "minus",
        .value = 3,
    }, tags[3]);

    const fields = std.meta.fields(lexer.Token);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "digit",
        .type = u4,
        .alignment = 1,
    }, fields[0]);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "ident",
        .type = void,
        .alignment = 1,
    }, fields[1]);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "plus",
        .type = void,
        .alignment = 1,
    }, fields[2]);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "minus",
        .type = void,
        .alignment = 1,
    }, fields[3]);
}
