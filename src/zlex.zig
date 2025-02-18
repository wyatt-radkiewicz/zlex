const std = @import("std");
const parser = @import("parser.zig");
const token = @import("token.zig");

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
///     .number = struct {
///         pub const capture = "(0|([1-9][0-9]*))(.[0-9]+)?";
///
///         // You can omit this if you like, which will make this equivalent to the above patterns
///         // You can also choose not to return an error if you don't need one (less branching!)
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
/// }).init(state_object, src_string)
/// ```
pub fn Lexer(comptime State: type, comptime tokens: anytype) type {
    return struct {
        state: State,

        const Self = @This();
        pub const Token = token.Token(tokens);
    };
}
